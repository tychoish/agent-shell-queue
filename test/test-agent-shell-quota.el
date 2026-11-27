;;; test-agent-shell-quota.el --- ERT tests for agent-shell-quota -*- lexical-binding: t -*-

;;; Commentary:
;; Offline ERT tests for `agent-shell-quota' using recorded `codexbar'
;; JSON fixtures and mocked async process lifecycle.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-shell-quota)

;;; Recorded Fixtures

(defconst test-agent-shell-quota--codex-json
  "[{\"provider\":\"codex\",\"source\":\"oauth\",\"rateWindowLabels\":{\"primary\":\"Session\",\"secondary\":\"Weekly\"},\"pace\":{\"primary\":{\"summary\":\"5% in reserve | Lasts until reset\"},\"secondary\":{\"summary\":\"17% in deficit | Runs out in 2d 1h\"}},\"usage\":{\"loginMethod\":\"plus\",\"primary\":{\"usedPercent\":60,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:31Z\"},\"secondary\":{\"usedPercent\":30,\"windowMinutes\":10080,\"resetsAt\":\"2026-12-03T03:44:13Z\"},\"tertiary\":null,\"providerCost\":{\"used\":0,\"limit\":0,\"currencyCode\":\"Credits\",\"period\":\"Extra usage\"}}}]"
  "Recorded `codexbar --provider codex --json' fixture.")

(defconst test-agent-shell-quota--claude-enterprise-json
  "[{\"provider\":\"claude\",\"source\":\"oauth\",\"usage\":{\"primary\":null,\"secondary\":null,\"tertiary\":null,\"loginMethod\":\"Claude Enterprise\",\"providerCost\":{\"currencyCode\":\"USD\",\"used\":490.49,\"limit\":1400,\"period\":\"Spend limit\"}}}]"
  "Recorded `codexbar --provider claude --json' fixture for Enterprise spend.")

(defconst test-agent-shell-quota--antigravity-json
  "[{\"provider\":\"antigravity\",\"source\":\"cli\",\"rateWindowLabels\":{\"primary\":\"Gemini Models\",\"secondary\":\"Claude and GPT\"},\"usage\":{\"primary\":{\"usedPercent\":45.1,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:36Z\"},\"secondary\":{\"usedPercent\":0,\"windowMinutes\":300,\"resetsAt\":\"1970-01-01T00:00:00Z\"},\"extraRateWindows\":[{\"id\":\"gemini-5h\",\"title\":\"Gemini 5-hour\",\"window\":{\"usedPercent\":45.1,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:36Z\"}},{\"id\":\"gemini-weekly\",\"title\":\"Gemini weekly\",\"window\":{\"usedPercent\":32.1,\"windowMinutes\":10080,\"resetsAt\":\"2026-12-03T16:29:36Z\"}},{\"id\":\"3p-5h\",\"title\":\"Claude/GPT 5-hour\",\"window\":{\"usedPercent\":0,\"windowMinutes\":300,\"resetsAt\":\"1970-01-01T00:00:00Z\"}},{\"id\":\"3p-weekly\",\"title\":\"Claude/GPT weekly\",\"window\":{\"usedPercent\":0,\"windowMinutes\":10080,\"resetsAt\":\"1970-01-01T00:00:00Z\"}}]}}]"
  "Recorded `codexbar --provider antigravity --json' fixture.")

;;; Parsing Tests

(ert-deftest test-agent-shell-quota/parse-codex ()
  "Codex fixture parses primary/secondary windows, labels, and pace notes."
  (let* ((entry (agent-shell-quota--parse-output
                 "codex" test-agent-shell-quota--codex-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (equal (agent-shell-quota-entry-plan entry) "plus"))
    (should (equal (agent-shell-quota-entry-source entry) "oauth"))
    (should (= (length windows) 2))
    (let ((w1 (nth 0 windows))
          (w2 (nth 1 windows)))
      (should (equal (agent-shell-quota-window-label w1) "Session"))
      (should (= (agent-shell-quota-window-used-percent w1) 60.0))
      (should (numberp (agent-shell-quota-window-resets-at w1)))
      (should (equal (agent-shell-quota-window-note w1)
                     "5% in reserve | Lasts until reset"))
      (should (equal (agent-shell-quota-window-label w2) "Weekly"))
      (should (= (agent-shell-quota-window-used-percent w2) 30.0))
      (should (equal (agent-shell-quota-window-note w2)
                     "17% in deficit | Runs out in 2d 1h")))))

(ert-deftest test-agent-shell-quota/parse-claude-enterprise ()
  "Claude Enterprise fixture emits a Spend limit window from providerCost."
  (let* ((entry (agent-shell-quota--parse-output
                 "claude" test-agent-shell-quota--claude-enterprise-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (equal (agent-shell-quota-entry-plan entry) "Claude Enterprise"))
    (should (= (length windows) 1))
    (let ((w (car windows)))
      (should (equal (agent-shell-quota-window-label w) "Spend limit"))
      (should (< (abs (- (agent-shell-quota-window-used-percent w) 35.035)) 0.01))
      (should (null (agent-shell-quota-window-resets-at w)))
      (should (equal (agent-shell-quota-window-note w)
                     "$490.49 / $1400 (Claude Enterprise)")))))

(ert-deftest test-agent-shell-quota/parse-antigravity-extra-windows ()
  "Antigravity prefers extraRateWindows and converts epoch-0 resetsAt to nil."
  (let* ((entry (agent-shell-quota--parse-output
                 "antigravity" test-agent-shell-quota--antigravity-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (= (length windows) 4))
    (should (equal (agent-shell-quota-window-label (nth 0 windows)) "Gemini 5-hour"))
    (should (numberp (agent-shell-quota-window-resets-at (nth 0 windows))))
    (should (equal (agent-shell-quota-window-label (nth 2 windows)) "Claude/GPT 5-hour"))
    (should (null (agent-shell-quota-window-resets-at (nth 2 windows))))
    (should (equal (substring-no-properties
                    (agent-shell-quota--format-reset
                     (agent-shell-quota-window-resets-at (nth 2 windows)) 1000.0))
                   "---"))))

;;; Formatting and Threshold Tests

(ert-deftest test-agent-shell-quota/faces-and-bars ()
  "Threshold faces match ok (<60), warn (60..84), and critical (>=85)."
  (should (eq (agent-shell-quota--usage-face 30) 'agent-shell-quota-ok-face))
  (should (eq (agent-shell-quota--usage-face 60) 'agent-shell-quota-warn-face))
  (should (eq (agent-shell-quota--usage-face 85) 'agent-shell-quota-critical-face))
  (let ((bar (agent-shell-quota--format-bar 50)))
    (should (equal (substring-no-properties bar) "[█████░░░░░]  50%"))
    (should (eq (get-text-property 0 'face bar) 'agent-shell-quota-ok-face))))

(ert-deftest test-agent-shell-quota/format-reset-countdowns ()
  "Countdown formatting handles past, minutes, hours, and days."
  (let ((now 100000.0))
    (should (equal (substring-no-properties
                    (agent-shell-quota--format-reset (- now 10) now))
                   "reset"))
    (should (equal (agent-shell-quota--format-reset (+ now 600) now) "in 10m"))
    (should (equal (agent-shell-quota--format-reset (+ now 5400) now) "in 1h 30m"))
    (should (equal (agent-shell-quota--format-reset (+ now 93600) now) "in 1d 2h"))))

;;; Incremental Async, Deduplication, Error, and Timeout Tests

(ert-deftest test-agent-shell-quota/incremental-async-and-dedup ()
  "Parallel refreshes update rows incrementally and deduplicate in-flight providers."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota--inflight (make-hash-table :test #'equal))
        (agent-shell-quota-providers '("claude" "codex"))
        (spawned nil)
        (sentinels (make-hash-table :test #'equal))
        (outbufs (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codexbar"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (let* ((cmd (plist-get plist :command))
                        (prov (caddr cmd))
                        (buf (plist-get plist :buffer))
                        (sentinel (plist-get plist :sentinel))
                        (fake-proc (list :provider prov :live t :exit 0)))
                   (push prov spawned)
                   (setf (map-elt sentinels prov) (cons fake-proc sentinel))
                   (setf (map-elt outbufs prov) buf)
                   fake-proc)))
              ((symbol-function 'process-live-p) (lambda (p) (plist-get p :live)))
              ((symbol-function 'process-exit-status) (lambda (p) (plist-get p :exit)))
              ((symbol-function 'process-get) (lambda (p k) (plist-get p k)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      ;; First refresh launches both providers
      (agent-shell-quota-refresh)
      (should (= (length spawned) 2))
      ;; Second refresh while in-flight spawns nothing new
      (agent-shell-quota-refresh)
      (should (= (length spawned) 2))
      ;; Both rows currently show loading
      (let ((rows (agent-shell-quota--build-rows)))
        (should (= (length rows) 2))
        (should (equal (substring-no-properties (aref (cadr (nth 0 rows)) 1)) "loading...")))
      ;; Complete claude first -> incremental update
      (with-current-buffer (map-elt outbufs "claude")
        (insert test-agent-shell-quota--claude-enterprise-json))
      (let ((cell (map-elt sentinels "claude")))
        (plist-put (car cell) :live nil)
        (funcall (cdr cell) (car cell) "finished\n"))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "claude"))
                  'ok))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "codex"))
                  'loading))
      (should-not (buffer-live-p (map-elt outbufs "claude")))
      ;; Complete codex with error -> sibling claude intact
      (with-current-buffer (map-elt outbufs "codex")
        (insert "auth expired\n"))
      (let ((cell (map-elt sentinels "codex")))
        (plist-put (car cell) :live nil)
        (plist-put (car cell) :exit 1)
        (funcall (cdr cell) (car cell) "exited abnormally\n"))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "claude"))
                  'ok))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "codex"))
                  'error))
      (should (equal (agent-shell-quota-entry-error-message
                      (map-elt agent-shell-quota--cache "codex"))
                     "auth expired")))))

(ert-deftest test-agent-shell-quota/timeout-and-bad-json ()
  "Timed-out process and malformed JSON both record structured error entries."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota--inflight (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'process-exit-status) (lambda (p) (plist-get p :exit)))
              ((symbol-function 'process-get) (lambda (p k) (plist-get p k))))
      ;; Timeout case
      (let ((buf (generate-new-buffer " *test-timeout*")))
        (agent-shell-quota--handle-exit
         "antigravity" '(:exit 9 agent-shell-quota-timed-out t) buf)
        (should-not (buffer-live-p buf))
        (let ((entry (map-elt agent-shell-quota--cache "antigravity")))
          (should (eq (agent-shell-quota-entry-status entry) 'error))
          (should (string-match-p "Timed out" (agent-shell-quota-entry-error-message entry)))))
      ;; Bad JSON case
      (let ((buf (generate-new-buffer " *test-badjson*")))
        (with-current-buffer buf (insert "not-json"))
        (agent-shell-quota--handle-exit "codex" '(:exit 0) buf)
        (should-not (buffer-live-p buf))
        (let ((entry (map-elt agent-shell-quota--cache "codex")))
          (should (eq (agent-shell-quota-entry-status entry) 'error))
          (should (string-match-p "Parse error" (agent-shell-quota-entry-error-message entry))))))))

;;; HUD Modeline Segment Tests

(ert-deftest test-agent-shell-quota/hud-segment ()
  "HUD segment labels the quota button and includes cache age."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota-providers '("antigravity" "codex" "claude")))
    (should (equal (substring-no-properties (agent-shell-quota--hud-segment))
                   "quota never"))
    (setf (map-elt agent-shell-quota--cache "antigravity")
          (agent-shell-quota--parse-output
           "antigravity" test-agent-shell-quota--antigravity-json 1000.0))
    (setf (map-elt agent-shell-quota--cache "codex")
          (agent-shell-quota--parse-output
           "codex" test-agent-shell-quota--codex-json 1000.0))
    (setf (map-elt agent-shell-quota--cache "claude")
          (agent-shell-quota--parse-output
           "claude" test-agent-shell-quota--claude-enterprise-json 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1120.0)))
      (let ((seg (agent-shell-quota--hud-segment)))
        (should (equal (substring-no-properties seg) "quota 2m"))
        (should (string-match-p "Agy:45% Cdx:60% Cld:35%"
                                (get-text-property 0 'help-echo seg)))))))

(ert-deftest test-agent-shell-quota/capacity-bars ()
  "Capacity inverts the displayed percentage while retaining usage severity."
  (let ((agent-shell-quota-display 'capacity))
    (let ((bar (agent-shell-quota--format-bar 90)))
      (should (equal (substring-no-properties bar) "[█░░░░░░░░░]  10%"))
      (should (eq (get-text-property 0 'face bar)
                  'agent-shell-quota-critical-face)))
    (should (string-suffix-p "100%" (agent-shell-quota--format-bar 0)))
    (should (string-suffix-p "  0%" (agent-shell-quota--format-bar 120)))
    (should (equal (substring-no-properties (agent-shell-quota--format-bar nil))
                   "---"))))

(ert-deftest test-agent-shell-quota/toggle-display ()
  "Toggling updates report rows and headers without fetching new data."
  (let ((agent-shell-quota-display 'usage)
        (agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota-providers '("codex")))
    (setf (map-elt agent-shell-quota--cache "codex")
          (agent-shell-quota--parse-output
           "codex" test-agent-shell-quota--codex-json 1000.0))
    (with-temp-buffer
      (agent-shell-quota-mode)
      (let ((agent-shell-quota-buffer-name (buffer-name)))
        (should (equal mode-name "quota"))
        (agent-shell-quota--redraw-if-live)
        (goto-char (point-min))
        (forward-line 1)
        (let ((row (tabulated-list-get-id)))
          (agent-shell-quota-toggle-display)
          (should (eq agent-shell-quota-display 'capacity))
          (should (equal (car (aref tabulated-list-format 2)) "Capacity"))
          (should (equal (tabulated-list-get-id) row))
          (should (string-suffix-p " 70%" (aref (tabulated-list-get-entry) 2)))
          (should (= (agent-shell-quota-window-used-percent
                      (car (agent-shell-quota-entry-windows
                            (map-elt agent-shell-quota--cache "codex"))))
                     60.0))
          (agent-shell-quota-toggle-display)
          (should (eq agent-shell-quota-display 'usage))
          (should (equal (car (aref tabulated-list-format 2)) "Usage")))))))

(ert-deftest test-agent-shell-quota/format-age ()
  "Refresh ages handle unknown, seconds, minutes, hours, days and clock skew."
  (should (equal (agent-shell-quota--format-age nil 1000) "never"))
  (should (equal (agent-shell-quota--format-age 1010 1000) "0s"))
  (should (equal (agent-shell-quota--format-age 990 1000) "10s"))
  (should (equal (agent-shell-quota--format-age 880 1000) "2m"))
  (should (equal (agent-shell-quota--format-age 1000 8200) "2h"))
  (should (equal (agent-shell-quota--format-age 1000 173800) "2d")))

(ert-deftest test-agent-shell-quota/cache-age-and-capacity-tooltip ()
  "Age uses the oldest successful configured provider, including during refresh."
  (let ((agent-shell-quota-display 'capacity)
        (agent-shell-quota-providers '("codex" "claude"))
        (agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota--inflight (make-hash-table :test #'equal)))
    (setf (map-elt agent-shell-quota--cache "codex")
          (agent-shell-quota--parse-output
           "codex" test-agent-shell-quota--codex-json 1000.0))
    (setf (map-elt agent-shell-quota--cache "claude")
          (agent-shell-quota--parse-output
           "claude" test-agent-shell-quota--claude-enterprise-json 1060.0))
    (setf (map-elt agent-shell-quota--inflight "codex") '(fake-process))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1120.0)))
      (should (equal (agent-shell-quota--cache-age) "2m"))
      (should (string-match-p "Capacity.*Cdx:40% Cld:65%"
                              (get-text-property
                               0 'help-echo (agent-shell-quota--hud-segment)))))
    (setf (agent-shell-quota-entry-status
           (map-elt agent-shell-quota--cache "codex")) 'error)
    (should (equal (agent-shell-quota--cache-age 1120.0) "1m"))))

(ert-deftest test-agent-shell-quota/bindings-and-refresh-line ()
  "Report bindings expose the menu and dispatch refreshes to the current provider."
  (should (eq (keymap-lookup agent-shell-quota-mode-map "g")
              #'agent-shell-quota-refresh))
  (should (eq (keymap-lookup agent-shell-quota-mode-map "r")
              #'agent-shell-quota-refresh-at-point))
  (should (eq (keymap-lookup agent-shell-quota-mode-map "t")
              #'agent-shell-quota-toggle-display))
  (should (eq (keymap-lookup agent-shell-quota-mode-map "?")
              #'agent-shell-quota-dispatch))
  (with-temp-buffer
    (agent-shell-quota-mode)
    (setq tabulated-list-entries '((("codex" . 0) ["codex" "Session" "60%" "" ""])))
    (tabulated-list-print)
    (goto-char (point-min))
    (let (refreshed)
      (cl-letf (((symbol-function 'agent-shell-quota-refresh-provider)
                 (lambda (provider) (setq refreshed provider))))
        (agent-shell-quota-refresh-at-point)
        (should (equal refreshed "codex"))))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (should-error (agent-shell-quota-refresh-at-point) :type 'user-error)))

(ert-deftest test-agent-shell-quota/dispatch-and-hud-action ()
  "The modeline action opens the quota transient."
  (should (get 'agent-shell-quota-dispatch 'transient--prefix))
  (let (opened)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (prefix &rest _) (setq opened prefix))))
      (agent-shell-quota-dispatch)
      (should (eq opened 'agent-shell-quota-dispatch))))
  (require 'test-helper)
  (let ((keys (transient-test/collect-keys 'agent-shell-quota-dispatch)))
    (should (equal keys '("r" "g" "t" "o")))
    (should-not (transient-test/duplicate-keys keys)))
  (let (action)
    (cl-letf (((symbol-function 'hud-modeline--register-segment) #'ignore)
              ((symbol-function 'hud-modeline--make-segment) #'list)
              ((symbol-function 'hud-modeline-set-segment-action)
               (lambda (_key command &optional _help) (setq action command))))
      (agent-shell-quota-hud-enable)
      (should (eq action #'agent-shell-quota-dispatch)))))

(provide 'test-agent-shell-quota)
;;; test-agent-shell-quota.el ends here
