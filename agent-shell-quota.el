;;; agent-shell-quota.el --- Subscription quota report via codexbar -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Asynchronous subscription quota report across configurable agent
;; backends backed by the `codexbar' CLI.  Queries each configured
;; provider independently in parallel (`codexbar --provider <name>
;; --json --json-only') and updates the `*agent-shell-quota*'
;; `tabulated-list-mode' buffer incrementally as each subprocess
;; completes.  Strictly zero-token: never invokes LLM inference.
;;
;; Enable `agent-shell-quota-mode-line-mode' for a clickable miscellaneous
;; mode-line indicator with background refreshes.  Reports pop up briefly
;; at the bottom of the frame and stay open when selected.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

;;; Customization

(defgroup agent-shell-quota nil
  "Subscription quota reporting for agent-shell backends."
  :group 'agent-shell-queue
  :prefix "agent-shell-quota-")

(defcustom agent-shell-quota-executable "codexbar"
  "Executable name or path for the CodexBar CLI."
  :type 'string
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-providers '("antigravity" "codex" "claude")
  "Ordered list of CodexBar provider names to query."
  :type '(repeat string)
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-extra-args nil
  "Additional command-line arguments passed to `codexbar'."
  :type '(repeat string)
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-timeout-seconds 15
  "Maximum seconds to wait for a single `codexbar' provider call."
  :type 'integer
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-warn-threshold 60
  "Percentage at or above which quota usage is highlighted as a warning."
  :type 'number
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-critical-threshold 85
  "Percentage at or above which quota usage is highlighted as critical."
  :type 'number
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-bar-width 10
  "Character width of the utilization progress bar."
  :type 'integer
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-display 'usage
  "Whether quota bars and summaries show usage or remaining capacity.
Warning and critical faces always reflect usage, even in capacity view."
  :type '(choice (const :tag "Usage (% used)" usage)
                 (const :tag "Capacity (% remaining)" capacity))
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-refresh-interval 300
  "Seconds between background refreshes while the mode-line mode is enabled."
  :type '(number :match (lambda (_widget value)
                         (and (numberp value) (> value 0))))
  :group 'agent-shell-quota)

(defcustom agent-shell-quota-popup-seconds 5
  "Seconds before an unattended quota popup disappears.
Selecting or using the report keeps it open."
  :type 'number
  :group 'agent-shell-quota)

(defconst agent-shell-quota-buffer-name "*agent-shell-quota*"
  "Buffer name for the subscription quota report.")

(defconst agent-shell-quota-detail-buffer-name "*agent-shell-quota-detail*"
  "Buffer name for the provider detail inspector.")

;;; Faces

(defface agent-shell-quota-ok-face
  '((t :inherit success))
  "Face for normal quota utilization below `agent-shell-quota-warn-threshold'."
  :group 'agent-shell-quota)

(defface agent-shell-quota-warn-face
  '((t :inherit warning))
  "Face for elevated quota utilization."
  :group 'agent-shell-quota)

(defface agent-shell-quota-critical-face
  '((t :inherit error :weight bold))
  "Face for critical quota utilization."
  :group 'agent-shell-quota)

(defface agent-shell-quota-error-face
  '((t :inherit error))
  "Face for provider fetch errors."
  :group 'agent-shell-quota)

(defface agent-shell-quota-muted-face
  '((t :inherit shadow))
  "Face for secondary metadata and loading indicators."
  :group 'agent-shell-quota)

;;; Data Structures and State

(cl-defstruct (agent-shell-quota-window (:copier nil))
  "A single quota window or spend-limit row for a provider."
  label         ; string, e.g. "Session", "Gemini 5-hour", "Spend limit"
  used-percent  ; float 0.0..100.0 or nil
  resets-at     ; float Unix timestamp (> 0) or nil
  note)         ; string, e.g. pace summary, spend pair, or source

(cl-defstruct (agent-shell-quota-entry (:copier nil))
  "Cached fetch state for one provider."
  provider      ; string, e.g. "codex"
  status        ; symbol: loading, ok, error
  source        ; string or nil, e.g. "oauth", "cli"
  plan          ; string or nil, e.g. "plus", "Claude Enterprise"
  windows       ; list of `agent-shell-quota-window'
  fetched-at    ; float Unix timestamp or nil
  error-message ; string or nil
  raw)          ; parsed JSON alist for detail inspector

(defvar agent-shell-quota--cache (make-hash-table :test #'equal)
  "Hash table mapping provider name string to `agent-shell-quota-entry'.")

(defvar agent-shell-quota--inflight (make-hash-table :test #'equal)
  "Hash table mapping provider name string to (PROCESS . WATCHDOG-TIMER).")

(defvar agent-shell-quota--refresh-timer nil
  "Timer for background quota refreshes.")

(defvar agent-shell-quota--mode-line
  '(:eval (agent-shell-quota--mode-line-segment))
  "Quota indicator displayed through `global-mode-string'.")

(put 'agent-shell-quota--mode-line 'risky-local-variable t)

;;; Parsing Helpers

(defun agent-shell-quota--parse-iso8601 (timestamp)
  "Parse ISO 8601 TIMESTAMP string into a float Unix epoch, or nil.
Returns nil when TIMESTAMP is nil, empty, invalid, or at/before epoch 0."
  (when (and (stringp timestamp) (not (string-empty-p timestamp)))
    (condition-case nil
        (let* ((decoded (iso8601-parse timestamp))
               (epoch (float-time (encode-time decoded))))
          (when (> epoch 0.0)
            epoch))
      (error nil))))

(defun agent-shell-quota--format-currency (amount currency)
  "Format numeric AMOUNT with CURRENCY code."
  (let ((num (if (and (numberp amount) (= amount (truncate amount)))
                 (format "%d" (truncate amount))
               (format "%.2f" (float amount)))))
    (if (or (null currency) (equal currency "USD"))
        (concat "$" num)
      (format "%s %s" num currency))))

(defun agent-shell-quota--extract-windows (obj)
  "Extract a list of `agent-shell-quota-window' from parsed provider OBJ."
  (let* ((usage (map-elt obj 'usage))
         (source (or (map-elt obj 'source) ""))
         (labels (map-elt obj 'rateWindowLabels))
         (pace (map-elt obj 'pace))
         (plan (or (map-elt usage 'loginMethod)
                   (map-elt (map-elt usage 'identity) 'loginMethod)))
         (extra-windows (map-elt usage 'extraRateWindows))
         (provider-cost (map-elt usage 'providerCost))
         (windows nil))
    (if (and (listp extra-windows) extra-windows)
        (dolist (item extra-windows)
          (let* ((title (or (map-elt item 'title)
                            (map-elt item 'id)
                            "Window"))
                 (win (map-elt item 'window))
                 (pct (map-elt win 'usedPercent))
                 (resets (agent-shell-quota--parse-iso8601
                          (map-elt win 'resetsAt))))
            (push (make-agent-shell-quota-window
                   :label title
                   :used-percent (when (numberp pct) (float pct))
                   :resets-at resets
                   :note source)
                  windows)))
      (dolist (slot '((primary . "Primary")
                      (secondary . "Secondary")
                      (tertiary . "Tertiary")))
        (when-let* ((key (car slot))
                    (win (map-elt usage key)))
          (let* ((label (or (map-elt labels key) (cdr slot)))
                 (pct (map-elt win 'usedPercent))
                 (resets (agent-shell-quota--parse-iso8601
                          (map-elt win 'resetsAt)))
                 (pace-summary (map-elt (map-elt pace key) 'summary)))
            (push (make-agent-shell-quota-window
                   :label label
                   :used-percent (when (numberp pct) (float pct))
                   :resets-at resets
                   :note (or pace-summary source))
                  windows)))))
    (when-let* ((cost provider-cost)
                (limit (map-elt cost 'limit))
                ((and (numberp limit) (> limit 0))))
      (let* ((used (or (map-elt cost 'used) 0))
             (currency (map-elt cost 'currencyCode))
             (period (or (map-elt cost 'period) "Spend limit"))
             (pct (* 100.0 (/ (float used) (float limit))))
             (pair (format "%s / %s"
                           (agent-shell-quota--format-currency used currency)
                           (agent-shell-quota--format-currency limit currency)))
             (note (if (and (stringp plan) (not (string-empty-p plan)))
                       (format "%s (%s)" pair plan)
                     pair)))
        (push (make-agent-shell-quota-window
               :label period
               :used-percent pct
               :resets-at nil
               :note note)
              windows)))
    (nreverse windows)))

(defun agent-shell-quota--parse-output (provider json-string &optional now)
  "Parse `codexbar' JSON-STRING for PROVIDER into an entry struct.
Optional NOW overrides the fetched-at timestamp for deterministic testing."
  (let* ((fetched (or now (float-time)))
         (parsed (json-parse-string json-string
                                    :object-type 'alist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object :false))
         (obj (cond
               ((and (listp parsed) (consp (car parsed)) (symbolp (caar parsed)))
                parsed)
               ((listp parsed)
                (or (seq-find (lambda (item)
                                (equal (map-elt item 'provider) provider))
                              parsed)
                    (car parsed)))
               (t nil))))
    (unless obj
      (error "Empty JSON response for provider %s" provider))
    (let* ((usage (map-elt obj 'usage))
           (plan (or (map-elt usage 'loginMethod)
                     (map-elt (map-elt usage 'identity) 'loginMethod)))
           (windows (agent-shell-quota--extract-windows obj)))
      (make-agent-shell-quota-entry
       :provider provider
       :status 'ok
       :source (map-elt obj 'source)
       :plan plan
       :windows windows
       :fetched-at fetched
       :error-message nil
       :raw obj))))

;;; Async Execution

(defun agent-shell-quota--cleanup-inflight (provider)
  "Cancel any watchdog timer and clear PROVIDER from the inflight table."
  (when-let* ((cell (map-elt agent-shell-quota--inflight provider)))
    (when (timerp (cdr cell))
      (cancel-timer (cdr cell)))
    (map-delete agent-shell-quota--inflight provider)))

(defun agent-shell-quota--record-error (provider message)
  "Store an error entry for PROVIDER with MESSAGE and redraw."
  (setf (map-elt agent-shell-quota--cache provider)
        (make-agent-shell-quota-entry
         :provider provider
         :status 'error
         :fetched-at (float-time)
         :error-message message))
  (agent-shell-quota--redraw-if-live)
  (force-mode-line-update t))

(defun agent-shell-quota--handle-exit (provider proc outbuf)
  "Process sentinel handler for PROVIDER's `codexbar' PROC writing to OUTBUF."
  (unless (process-live-p proc)
    (let ((timed-out (process-get proc 'agent-shell-quota-timed-out))
          (exit-status (process-exit-status proc))
          (output nil))
      (agent-shell-quota--cleanup-inflight provider)
      (unwind-protect
          (when (buffer-live-p outbuf)
            (with-current-buffer outbuf
              (setq output (string-trim (buffer-string)))))
        (when (buffer-live-p outbuf)
          (kill-buffer outbuf)))
      (cond
       (timed-out
        (agent-shell-quota--record-error
         provider
         (format "Timed out after %ds" agent-shell-quota-timeout-seconds)))
       ((not (zerop exit-status))
        (agent-shell-quota--record-error
         provider
         (if (and output (not (string-empty-p output)))
             (truncate-string-to-width
              (car (split-string output "\n" t)) 80 nil nil "...")
           (format "codexbar exited with status %d" exit-status))))
       (t
        (condition-case err
            (let ((entry (agent-shell-quota--parse-output provider output)))
              (setf (map-elt agent-shell-quota--cache provider) entry)
              (agent-shell-quota--redraw-if-live)
              (force-mode-line-update t))
          (error
           (agent-shell-quota--record-error
            provider
            (format "Parse error: %s" (error-message-string err))))))))))

(defun agent-shell-quota-refresh-provider (provider)
  "Asynchronously refresh quota data for a single PROVIDER string."
  (let ((exe (executable-find agent-shell-quota-executable)))
    (unless exe
      (user-error "Cannot find `%s' on exec-path" agent-shell-quota-executable))
    (unless (map-elt agent-shell-quota--inflight provider)
      (unless (map-elt agent-shell-quota--cache provider)
        (setf (map-elt agent-shell-quota--cache provider)
              (make-agent-shell-quota-entry
               :provider provider
               :status 'loading)))
      (let* ((outbuf (generate-new-buffer (format " *agent-shell-quota-%s*" provider)))
             (cmd (append (list exe "--provider" provider "--json" "--json-only")
                          agent-shell-quota-extra-args))
             (proc (make-process
                    :name (format "agent-shell-quota-%s" provider)
                    :buffer outbuf
                    :command cmd
                    :connection-type 'pipe
                    :noquery t
                    :sentinel (lambda (p _event)
                                (agent-shell-quota--handle-exit provider p outbuf))))
             (timer (run-at-time
                     agent-shell-quota-timeout-seconds nil
                     (lambda ()
                       (when (process-live-p proc)
                         (process-put proc 'agent-shell-quota-timed-out t)
                         (delete-process proc))))))
        (setf (map-elt agent-shell-quota--inflight provider) (cons proc timer))
        (agent-shell-quota--redraw-if-live)))))

;;;###autoload
(defun agent-shell-quota-refresh (&optional providers)
  "Asynchronously refresh quota data for PROVIDERS in parallel.
Defaults to `agent-shell-quota-providers'."
  (interactive)
  (let ((exe (executable-find agent-shell-quota-executable)))
    (unless exe
      (user-error "Cannot find `%s' on exec-path" agent-shell-quota-executable)))
  (dolist (provider (or providers agent-shell-quota-providers))
    (agent-shell-quota-refresh-provider provider)))

;;; Formatting

(defun agent-shell-quota--usage-face (percent)
  "Return the display face for utilization PERCENT."
  (cond
   ((null percent) 'agent-shell-quota-muted-face)
   ((>= percent agent-shell-quota-critical-threshold) 'agent-shell-quota-critical-face)
   ((>= percent agent-shell-quota-warn-threshold) 'agent-shell-quota-warn-face)
   (t 'agent-shell-quota-ok-face)))

(defun agent-shell-quota--format-bar (percent)
  "Render used PERCENT according to `agent-shell-quota-display'."
  (if (null percent)
      (propertize "---" 'face 'agent-shell-quota-muted-face)
    (let* ((clamped (max 0.0 (min 100.0 (float percent))))
           (displayed (if (eq agent-shell-quota-display 'capacity)
                          (- 100.0 clamped)
                        clamped))
           (width (max 1 agent-shell-quota-bar-width))
           (filled (round (* width (/ displayed 100.0))))
           (empty (- width filled))
           (bar (concat "["
                        (make-string filled ?█)
                        (make-string empty ?░)
                        "]"))
           (label (format "%s %3.0f%%" bar displayed)))
      (propertize label 'face (agent-shell-quota--usage-face clamped)))))

(defun agent-shell-quota--format-age (fetched-at &optional now)
  "Format the age of FETCHED-AT relative to NOW in compact units."
  (if (null fetched-at)
      "never"
    (let ((age (max 0 (floor (- (or now (float-time)) fetched-at)))))
      (cond
       ((< age 60) (format "%ds" age))
       ((< age 3600) (format "%dm" (/ age 60)))
       ((< age 86400) (format "%dh" (/ age 3600)))
       (t (format "%dd" (/ age 86400)))))))

(defun agent-shell-quota--cache-age (&optional now)
  "Return the age of the oldest successful configured provider at NOW.
Failed fetches do not count as fresh quota data."
  (let ((timestamps
         (thread-last agent-shell-quota-providers
                      (seq-map (lambda (provider)
                                 (when-let* ((entry (map-elt agent-shell-quota--cache provider))
                                             ((eq (agent-shell-quota-entry-status entry) 'ok)))
                                   (agent-shell-quota-entry-fetched-at entry))))
                      (seq-filter #'numberp))))
    (agent-shell-quota--format-age
     (when timestamps (apply #'min timestamps)) now)))

(defun agent-shell-quota--format-reset (resets-at &optional now)
  "Format RESETS-AT Unix timestamp as a concise countdown relative to NOW."
  (if (or (null resets-at) (<= resets-at 0))
      (propertize "---" 'face 'agent-shell-quota-muted-face)
    (let ((delta (- resets-at (or now (float-time)))))
      (cond
       ((<= delta 0)
        (propertize "reset" 'face 'agent-shell-quota-ok-face))
       ((< delta 3600)
        (format "in %dm" (max 1 (round (/ delta 60.0)))))
       ((< delta 86400)
        (let* ((hours (floor (/ delta 3600.0)))
               (mins (floor (/ (mod delta 3600.0) 60.0))))
          (format "in %dh %dm" hours mins)))
       (t
        (let* ((days (floor (/ delta 86400.0)))
               (hours (floor (/ (mod delta 86400.0) 3600.0))))
          (format "in %dd %dh" days hours)))))))

(defun agent-shell-quota--build-rows (&optional now)
  "Build `tabulated-list-entries' from `agent-shell-quota--cache' at NOW."
  (let ((rows nil))
    (dolist (provider agent-shell-quota-providers)
      (let* ((entry (map-elt agent-shell-quota--cache provider))
             (inflight (and (map-elt agent-shell-quota--inflight provider) t))
             (status (and entry (agent-shell-quota-entry-status entry))))
        (cond
         ((or (null entry) (eq status 'loading))
          (push (list (cons provider nil)
                      (vector provider
                              (propertize "loading..." 'face 'agent-shell-quota-muted-face)
                              (propertize "..." 'face 'agent-shell-quota-muted-face)
                              (propertize "---" 'face 'agent-shell-quota-muted-face)
                              (propertize "querying codexbar" 'face 'agent-shell-quota-muted-face)))
                rows))
         ((eq status 'error)
          (push (list (cons provider nil)
                      (vector provider
                              (propertize "error" 'face 'agent-shell-quota-error-face)
                              (propertize "---" 'face 'agent-shell-quota-muted-face)
                              (propertize "---" 'face 'agent-shell-quota-muted-face)
                              (propertize (or (agent-shell-quota-entry-error-message entry)
                                              "unknown error")
                                          'face 'agent-shell-quota-error-face)))
                rows))
         ((null (agent-shell-quota-entry-windows entry))
          (push (list (cons provider nil)
                      (vector provider
                              (propertize "no windows" 'face 'agent-shell-quota-muted-face)
                              (propertize "---" 'face 'agent-shell-quota-muted-face)
                              (propertize "---" 'face 'agent-shell-quota-muted-face)
                              (propertize (or (agent-shell-quota-entry-source entry) "")
                                          'face 'agent-shell-quota-muted-face)))
                rows))
         (t
          (let ((idx 0))
            (dolist (win (agent-shell-quota-entry-windows entry))
              (let* ((note (or (agent-shell-quota-window-note win) ""))
                     (status-col (if inflight
                                     (concat (propertize "[updating] " 'face 'agent-shell-quota-muted-face)
                                             note)
                                   note)))
                (push (list (cons provider idx)
                            (vector provider
                                    (or (agent-shell-quota-window-label win) "")
                                    (agent-shell-quota--format-bar
                                     (agent-shell-quota-window-used-percent win))
                                    (agent-shell-quota--format-reset
                                     (agent-shell-quota-window-resets-at win) now)
                                    status-col))
                      rows))
              (setq idx (1+ idx))))))))
    (nreverse rows)))

;;; Report Buffer UI

(defun agent-shell-quota--redraw-if-live ()
  "Redraw `*agent-shell-quota*' if it exists and is live."
  (when-let* ((buf (get-buffer agent-shell-quota-buffer-name))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (when (derived-mode-p 'agent-shell-quota-mode)
        (let ((id (tabulated-list-get-id)))
          (setf (car (aref tabulated-list-format 2))
                (if (eq agent-shell-quota-display 'capacity)
                    "Capacity"
                  "Usage"))
          (tabulated-list-init-header)
          (setq tabulated-list-entries (agent-shell-quota--build-rows))
          (tabulated-list-print t)
          (when id
            (goto-char (point-min))
            (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
              (forward-line 1))))))))

;;;###autoload
(defun agent-shell-quota-toggle-display ()
  "Toggle between percentage used and percentage remaining."
  (interactive)
  (setq agent-shell-quota-display
        (if (eq agent-shell-quota-display 'usage)
            'capacity
          'usage))
  (agent-shell-quota--redraw-if-live)
  (force-mode-line-update t))

(defun agent-shell-quota-refresh-at-point ()
  "Refresh the provider on the current line."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (provider (and (consp id) (car id))))
    (unless provider
      (user-error "No provider on this line"))
    (agent-shell-quota-refresh-provider provider)))

(defun agent-shell-quota-inspect-at-point ()
  "Open a detail buffer showing raw `codexbar' metadata for the provider at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (provider (and (consp id) (car id)))
         (entry (and provider (map-elt agent-shell-quota--cache provider))))
    (unless entry
      (user-error "No quota data for provider at point"))
    (let ((buf (get-buffer-create agent-shell-quota-detail-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Provider: %s\n" (agent-shell-quota-entry-provider entry)))
          (insert (format "Status:   %s\n" (agent-shell-quota-entry-status entry)))
          (when-let* ((plan (agent-shell-quota-entry-plan entry)))
            (insert (format "Plan:     %s\n" plan)))
          (when-let* ((src (agent-shell-quota-entry-source entry)))
            (insert (format "Source:   %s\n" src)))
          (when-let* ((ts (agent-shell-quota-entry-fetched-at entry)))
            (insert (format "Fetched:  %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" ts))))
          (when-let* ((err (agent-shell-quota-entry-error-message entry)))
            (insert (format "Error:    %s\n" err)))
          (when-let* ((raw (agent-shell-quota-entry-raw entry)))
            (insert "\nRaw JSON:\n")
            (let ((start (point)))
              (insert (json-encode raw))
              (json-pretty-print start (point))))
          (goto-char (point-min)))
        (special-mode))
      (display-buffer buf))))

(defvar-keymap agent-shell-quota-mode-map
  :doc "Keymap for `agent-shell-quota-mode'."
  :parent tabulated-list-mode-map
  "g" #'agent-shell-quota-refresh
  "r" #'agent-shell-quota-refresh-at-point
  "t" #'agent-shell-quota-toggle-display
  "?" #'agent-shell-quota-dispatch
  "RET" #'agent-shell-quota-inspect-at-point)

(put 'agent-shell-quota-mode 'derived-mode-parent 'tabulated-list-mode)

(defvar agent-shell-quota-mode-hook nil
  "Hook run after initializing `agent-shell-quota-mode'.")

(defun agent-shell-quota-mode ()
  "Major mode for viewing agent-shell subscription quotas.
Only initialize an empty, non-file fundamental buffer or a quota report."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-quota-mode)
              (and (eq major-mode 'fundamental-mode)
                   (not buffer-file-name)
                   (zerop (buffer-size))))
    (user-error "Open the quota report with M-x agent-shell-quota"))
  (delay-mode-hooks
    (tabulated-list-mode)
    (setq major-mode 'agent-shell-quota-mode)
    (setq mode-name "quota")
    (use-local-map agent-shell-quota-mode-map)
    (setq tabulated-list-format
          (vector '("Provider" 13 t)
                  '("Window / Pool" 20 t)
                  (list (if (eq agent-shell-quota-display 'capacity)
                            "Capacity"
                          "Usage")
                        19 nil)
                  '("Resets In" 12 nil)
                  '("Status / Notes" 0 nil)))
    (setq-local mode-line-process
                '(:eval (concat " " (agent-shell-quota--cache-age))))
    (setq tabulated-list-padding 1)
    (setq tabulated-list-sort-key nil)
    (tabulated-list-init-header)
    (add-hook 'post-command-hook #'agent-shell-quota--keep-popup nil t)
    (add-hook 'window-scroll-functions #'agent-shell-quota--keep-scrolled-popup nil t))
  (run-mode-hooks 'agent-shell-quota-mode-hook))

(defun agent-shell-quota--keep-scrolled-popup (window _start)
  "Keep WINDOW open when the user scrolls an unattended quota popup."
  (when (memq this-command '(mwheel-scroll mwheel-scroll-up mwheel-scroll-down
                            scroll-up-command scroll-down-command
                            scroll-up scroll-down pixel-scroll-precision))
    (agent-shell-quota--keep-popup window)))

(defun agent-shell-quota--keep-popup (&optional window)
  "Cancel dismissal when the user interacts with the report in WINDOW."
  (let ((window (or window (selected-window))))
    (when-let* (((eq (window-buffer window) (current-buffer)))
                (timer (window-parameter window 'agent-shell-quota-timer)))
      (cancel-timer timer)
      (set-window-parameter window 'agent-shell-quota-timer nil))))

(defun agent-shell-quota--dismiss-popup (window buffer)
  "Dismiss WINDOW if it still shows BUFFER and has not been selected."
  (when (window-live-p window)
    (when-let* ((timer (window-parameter window 'agent-shell-quota-timer)))
      (cancel-timer timer))
    (set-window-parameter window 'agent-shell-quota-timer nil)
    (when (and (eq (window-buffer window) buffer)
               (not (eq window (selected-window))))
      (quit-window nil window))))

;;;###autoload
(defun agent-shell-quota ()
  "Pop up subscription quotas at the bottom and refresh all providers.
The report disappears after `agent-shell-quota-popup-seconds' unless used."
  (interactive)
  (let ((buf (get-buffer-create agent-shell-quota-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-quota-mode)
        (agent-shell-quota-mode))
      (setq tabulated-list-entries (agent-shell-quota--build-rows))
      (tabulated-list-print t))
    (let ((window (display-buffer
                   buf '((display-buffer-in-side-window)
                         (side . bottom)
                         (slot . 0)
                         (window-height . 0.25)
                         (inhibit-switch-frame . t)))))
      (when-let* ((timer (window-parameter window 'agent-shell-quota-timer)))
        (cancel-timer timer))
      (set-window-parameter
       window 'agent-shell-quota-timer
       (run-at-time agent-shell-quota-popup-seconds nil
                    #'agent-shell-quota--dismiss-popup window buf)))
    (agent-shell-quota-refresh)))

;;;###autoload
(transient-define-prefix agent-shell-quota-dispatch ()
  "Manage subscription quota data and its display."
  ["Quota"
   ("r" "Refresh line" agent-shell-quota-refresh-at-point
    :inapt-if-not (lambda ()
                    (and (derived-mode-p 'agent-shell-quota-mode)
                         (tabulated-list-get-id))))
   ("g" "Refresh all" agent-shell-quota-refresh)
   ("t" "Toggle usage/capacity" agent-shell-quota-toggle-display :transient t)
   ("o" "Open report" agent-shell-quota)])

;;; Optional Mode-Line Indicator

(defvar-keymap agent-shell-quota--mode-line-map
  :doc "Mouse actions for the quota indicator."
  "<mode-line> <mouse-1>" #'agent-shell-quota
  "<mode-line> <mouse-3>" #'agent-shell-quota-dispatch)

(defun agent-shell-quota--refresh-background ()
  "Refresh quota data, exposing background failures in the cached report."
  (condition-case err
      (agent-shell-quota-refresh)
    (error
     (seq-do (lambda (provider)
               (agent-shell-quota--record-error
                provider (error-message-string err)))
             agent-shell-quota-providers))))

;;;###autoload
(define-minor-mode agent-shell-quota-mode-line-mode
  "Show a clickable quota indicator in the mode line's miscellaneous section.
Refresh immediately on enable, then every `agent-shell-quota-refresh-interval'
seconds.  Click to open a report popup; right-click to open the quota menu."
  :global t
  :group 'agent-shell-quota
  (when (timerp agent-shell-quota--refresh-timer)
    (cancel-timer agent-shell-quota--refresh-timer))
  (setq agent-shell-quota--refresh-timer nil)
  (if agent-shell-quota-mode-line-mode
      (progn
        (unless (listp global-mode-string)
          (setq global-mode-string (list global-mode-string)))
        (add-to-list 'global-mode-string 'agent-shell-quota--mode-line t)
        (agent-shell-quota--refresh-background)
        (setq agent-shell-quota--refresh-timer
              (run-at-time agent-shell-quota-refresh-interval
                           agent-shell-quota-refresh-interval
                           #'agent-shell-quota--refresh-background)))
    (setq global-mode-string
          (remq 'agent-shell-quota--mode-line global-mode-string)))
  (force-mode-line-update t))

(defun agent-shell-quota--provider-abbrev (provider)
  "Return a short 3-character display abbreviation for PROVIDER."
  (pcase provider
    ("antigravity" "Agy")
    ("codex" "Cdx")
    ("claude" "Cld")
    (_ (capitalize (substring provider 0 (min 3 (length provider)))))))

(defun agent-shell-quota--entry-peak-percent (entry)
  "Return the highest `used-percent' across ENTRY's windows, or nil."
  (let ((percents (thread-last (agent-shell-quota-entry-windows entry)
                              (seq-map #'agent-shell-quota-window-used-percent)
                              (seq-filter #'numberp))))
    (when percents
      (apply #'max percents))))

(defun agent-shell-quota--mode-line-segment ()
  "Render a quota menu button with cache age for the mode line.
Reads strictly from `agent-shell-quota--cache' without spawning I/O."
  (let ((parts
         (seq-map
          (lambda (provider)
            (let* ((entry (map-elt agent-shell-quota--cache provider))
                   (peak (and entry
                              (eq (agent-shell-quota-entry-status entry) 'ok)
                              (agent-shell-quota--entry-peak-percent entry))))
              (if peak
                  (propertize
                   (format "%s:%.0f%%" (agent-shell-quota--provider-abbrev provider)
                           (if (eq agent-shell-quota-display 'capacity)
                               (max 0 (- 100 peak))
                             peak))
                   'face (agent-shell-quota--usage-face peak))
                (format "%s:%s" (agent-shell-quota--provider-abbrev provider)
                        (if entry
                            (agent-shell-quota-entry-status entry)
                          "not fetched")))))
          agent-shell-quota-providers)))
    (propertize
     (concat "quota " (agent-shell-quota--cache-age))
     'local-map agent-shell-quota--mode-line-map
     'mouse-face 'mode-line-highlight
     'help-echo (format "%s: %s\nAge of oldest successful provider refresh\nmouse-1: Quota report; mouse-3: Quota menu"
                        (if (eq agent-shell-quota-display 'capacity)
                            "Capacity (% remaining)"
                          "Usage (% used)")
                        (string-join parts " ")))))

(provide 'agent-shell-quota)
;;; agent-shell-quota.el ends here
