;;; agent-shell-queue-core.el --- Headless queue engine for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (alert "1.2"))

;;; Commentary:

;; Headless core engine for agent-shell-queue.  Provides the queue data model,
;; state machine, background task execution, turn subscriptions, delay
;; management, stall checkers, and persistence integration without any UI
;; dependencies.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sprite-future nil t)
(require 'agent-shell)
(require 'alert nil t)
(require 'savehist)

(declare-function agent-shell-queue-buffer-refresh "agent-shell-queue-ui")
(declare-function agent-shell-queue--open-capture "agent-shell-queue-ui")
(declare-function agent-shell-queue--open-elisp-capture "agent-shell-queue-ui")
(declare-function agent-shell-queue--buffer-state-label "agent-shell-queue-ui")
(declare-function agent-shell-queue--activity-state "agent-shell-queue-ui")
(declare-function agent-shell-queue--active-item-count "agent-shell-queue-ui")
(declare-function agent-shell-queue-capture-cancel "agent-shell-queue-ui")
(declare-function agent-shell-queue-capture-confirm "agent-shell-queue-ui")
(declare-function agent-shell-queue-enqueue "agent-shell-queue-ui")
(declare-function annotated-completing-read "annotated-completing-read")


;;; Configuration and Defvars



;;; Configuration

(defun agent-shell-queue--default-instance-name ()
  "Return a string identifying the current Emacs instance."
  (let ((d (daemonp)))
    (cond ((eq d t) "primary")
          (d d)
          (t (system-name)))))

(defvar agent-shell-queue-instance-name #'agent-shell-queue--default-instance-name
  "Instance identifier written into archive records.
May be a string or a zero-argument function that returns a string.
Defaults to the daemon name or system hostname.  Override in config:
  (setq agent-shell-queue-instance-name \"<name>\")
  (setq agent-shell-queue-instance-name \\='get-instance-name")

(defcustom agent-shell-queue-default-pause-delay 30.0
  "Default pause duration in seconds between tasks.
Set to 0 or nil for no delay."
  :type '(choice (const :tag "No delay" 0)
                 (integer :tag "Seconds")
                 (float :tag "Float seconds"))
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-alert-on-pause-start nil
  "When non-nil, send an alert notification when a task pause or delay starts.
The notification message includes the duration of the pause."
  :type 'boolean
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-alert-before-pause-end nil
  "Duration in seconds before a pause ends to send an alert notification.
When non-nil and less than the total pause duration, an alert is sent when
`(total-pause-duration - alert-before-pause-end)` seconds elapses."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")
                 (float :tag "Float seconds"))
  :group 'agent-shell-queue)

(defvar agent-shell-queue--store)
(defvar agent-shell-queue--idle-timer nil
  "Timer for idle dispatch.")

(defvar agent-shell-queue--idle-flush-timer nil
  "Timer for idle persistence flush.")

(defvar agent-shell-queue--subscriptions nil
  "List of active event subscription callbacks.")

(defvar agent-shell-queue--loaded nil
  "Non-nil when queue has been loaded from storage.")

(defvar agent-shell-queue-serialization-format 'plist
  "Format used to persist queue state to disk.
One of:
  `plist' — s-expression with keyword-keyed plists (default; no extra deps)
  `json'  — JSON via built-in `json-serialize'/`json-parse-string' (Emacs 27+)
  `yaml'  — YAML via `yaml-encode'/`yaml-parse-string' from the `yaml' package
  `org'   — Org-mode file backend via `agent-shell-queue-org'")

(defvar agent-shell-queue-idle-delay 60.0
  "Idle delay in seconds for the backup auto-send timer.
Primary draining happens via `shell-maker-finish-output' advice; this timer
is only a safety net for buffers that become idle outside that path.")

(defvar agent-shell-queue-background-prefix '((omp . "/background ") (t . "/background "))
  "Alist mapping `<agent-shell-identifier>` to background prefix string.")

(defvar agent-shell-queue-clear-command '((omp . "/fresh") (t . "/clear"))
  "Alist mapping `<agent-shell-identifier>` to clear command string.")

(defun agent-shell-queue--get-background-prefix (buf)
  "Resolve the background prefix for BUF based on its `agent-shell' configuration."
  (let* ((buffer (get-buffer buf))
         (config (and buffer (fboundp 'agent-shell-get-config) (agent-shell-get-config buffer)))
         (ident (and config (map-elt config :identifier)))
         (val agent-shell-queue-background-prefix))
    (cond
     ((functionp val) (funcall val ident))
     ((listp val) (or (cdr (assq ident val)) (cdr (assq t val)) "/background "))
     (t val))))

(defun agent-shell-queue--get-clear-command (buf)
  "Resolve the clear command for BUF based on its `agent-shell' configuration."
  (let* ((buffer (get-buffer buf))
         (config (and buffer (fboundp 'agent-shell-get-config) (agent-shell-get-config buffer)))
         (ident (and config (map-elt config :identifier)))
         (val agent-shell-queue-clear-command))
    (cond
     ((functionp val) (funcall val ident))
     ((listp val) (or (cdr (assq ident val)) (cdr (assq t val)) "/clear"))
     (t val))))

(defvar agent-shell-queue-done-log-file nil
  "File path for appending completed queue items as JSON lines.
When nil (the default), completed items are not logged to disk.")

(defvar agent-shell-queue-state-file-function #'agent-shell-queue--default-state-file
  "Function returning the path to the queue state file.")

(defvar agent-shell-queue-pick-buffer-function #'agent-shell-queue--default-pick-buffer
  "Function called with a PROMPT string to pick an `agent-shell' buffer.")

(defvar agent-shell-queue-archive-enabled nil
  "When non-nil, completed items can be archived.
Controls whether `agent-shell-queue-buffer-archive' is active.
The destination path is controlled separately by
`agent-shell-queue-archive-file-function'.")

(defvar agent-shell-queue-archive-file-function #'agent-shell-queue--default-archive-file
  "Function returning the JSONL archive file path.  Called with no arguments.
Override to store the archive at a custom location.  Only consulted when
`agent-shell-queue-archive-enabled' is non-nil.")

(defcustom agent-shell-queue-response-max-length 8192
  "Maximum length (in characters) of captured response text to store.
Responses longer than this are truncated with a \"…[truncated]\" suffix.

This prevents very large responses from bloating the queue state file.
Set to nil to disable truncation and store full responses.

Default: 8192 (8KB) — balances completeness with file size."
  :type '(choice (integer :tag "Max length in characters")
                 (const :tag "No limit (store full responses)" nil))
  :group 'agent-shell-queue)

(defconst agent-shell-queue-response-max-length-absolute 1048576
  "Absolute maximum length (1MB) for response text, regardless of configuration.
This hard limit prevents pathological cases from consuming excessive memory
or creating unmanageable state files.  Applies even when
`agent-shell-queue-response-max-length' is nil.")

(defvar agent-shell-queue--last-flush-time nil
  "Float-time of the most recent queue state write to disk.")

(defvar agent-shell-queue--next-flush-time nil
  "Time of the next scheduled auto-flush, or nil if none is pending.")

(defvar agent-shell-queue-auto-flush-interval 300
  "Seconds between automatic queue flushes.  Set to nil to disable.")

(defvar agent-shell-queue--stale-item-ids nil
  "List of item IDs that failed dispatch due to struct mismatch after code reload.
These are automatically deferred and their buffers paused.")

(defvar agent-shell-queue-before-reload-hook nil
  "Hook run just before code and state are reloaded.
Queue is paused and flushed to disk before this hook fires.")

(defvar agent-shell-queue-after-reload-hook nil
  "Hook run after code and state have been reloaded from disk.")

(defvar agent-shell-queue--wait-timers nil
  "Alist of (ITEM-ID . TIMER) for active wait-until items.
Timers are cancelled automatically when items are removed or the queue reloads.")

(defvar agent-shell-queue--compact-running nil
  "List of (BUF-NAME . ITEM-ID) pairs for compact items currently dispatched.
Used by `agent-shell-queue-mark-done' to clean up session-pause state.")

(defvar agent-shell-queue--remove-all-confirmed nil
  "When non-nil, skip per-item confirmation in remove commands.
Set to t when user answers \\='a\\=' (all) at a removal prompt.")

(defvar agent-shell-queue--response-start-positions nil
  "Alist of (ITEM-ID . BUFFER-POSITION) recording response start.
Records where in the shell buffer each dispatched LLM prompt begins.
Used to capture the response text on turn-complete and store it in
the item's response field.")
(defvar agent-shell-queue-save-function nil
  "When non-nil, called instead of the default file-based save logic.
The function is called with no arguments and must persist the current
queue items to a durable store.
Used by backends such as `agent-shell-queue-db' to bypass file I/O.")

(defvar agent-shell-queue-load-function nil
  "When non-nil, called instead of the default file-based load logic.
The function is called with no arguments and must populate
the queue items from a durable store.
Used by backends such as `agent-shell-queue-db' to bypass file I/O.")

(defvar agent-shell-queue-safe-save nil
  "When non-nil, write a versioned backup before each queue state save.
Backups are written to `agent-shell-queue-safe-save-directory' using the
format selected by `agent-shell-queue-safe-save-format'.
Has no effect when `agent-shell-queue-save-function' is set.")

(defvar agent-shell-queue-safe-save-directory nil
  "Directory for versioned queue backups written when safe-save is non-nil.
Nil means use a subdirectory of variable `temporary-file-directory' named
\"emacs-<instance>\" where <instance> comes from
`agent-shell-queue-instance-name'.")

(defvar agent-shell-queue-safe-save-format nil
  "Serialization format for safe-save backups.
When nil, use `agent-shell-queue-serialization-format'.")

(defvar agent-shell-queue-safe-save-max-files nil
  "Maximum number of versioned backup files to keep in the safe-save directory.
When non-nil and the backup count exceeds this limit, the oldest file is
deleted after each save — one file at a time so lowering the limit converges
gradually.  Requires `agent-shell-queue-safe-save'.")

(defvar agent-shell-queue-idle-flush-delay nil
  "Seconds of Emacs idle time after which the queue state is flushed to disk.
Set to nil to disable idle-triggered saves (default).")

(defvar agent-shell-queue-stall-timeout 180
  "Seconds after dispatch before a still-running item is reported as stalled.
The ACP/shell-maker layer has no watchdog of its own: a wedged Lisp event
loop or a desynced busy flag leaves a dispatched item showing `running'
with no further user-visible feedback, indefinitely.  This is a one-shot
check, not a retry loop — it only surfaces the condition via `alert', it
does not cancel or resend the item.  Set to nil to disable.")

(defcustom agent-shell-queue-strict-buffer-assignment nil
  "When non-nil, signal `user-error' when no compatible live buffer exists.
When nil (default), fall through to nil/unassigned assignment instead."
  :type 'boolean
  :group 'agent-shell-queue)

(require 'agent-shell-queue-persistence)



(defconst agent-shell-queue--unassigned-key "(unassigned)"
  "Alist bucket key for items not yet assigned to any shell.")

(defconst agent-shell-queue--dir-prefix "dir:"
  "Prefix string identifying a directory-scoped queue bucket.")

(defun agent-shell-queue--canonicalize-dir (dir)
  "Return expanded, canonicalized directory path for DIR."
  (file-name-as-directory (expand-file-name (or dir default-directory))))

(defun agent-shell-queue--dir-bucket-p (bucket-name)
  "Return non-nil if BUCKET-NAME is a directory queue bucket string."
  (and (stringp bucket-name)
       (string-prefix-p agent-shell-queue--dir-prefix bucket-name)))

(defun agent-shell-queue--dir-from-bucket (bucket-name)
  "Extract directory path from BUCKET-NAME string.
Returns nil if BUCKET-NAME is not a directory bucket."
  (when (agent-shell-queue--dir-bucket-p bucket-name)
    (substring bucket-name (length agent-shell-queue--dir-prefix))))

(defun agent-shell-queue--bucket-for-dir (dir)
  "Return the directory queue bucket string for DIR."
  (concat agent-shell-queue--dir-prefix (agent-shell-queue--canonicalize-dir dir)))

(defun agent-shell-queue--pick-shell-for-directory (dir item-id)
  "Create an `agent-shell' buffer for DIR associated with ITEM-ID.
Creates a new shell in DIR via `agent-shell-new-shell' and appends `-ITEM-ID'
to its buffer name."
  (let* ((canon-dir (agent-shell-queue--canonicalize-dir dir))
         (before-bufs (agent-shell-buffers))
         (default-directory canon-dir)
         (buf (agent-shell-new-shell)))
    (unless (and (bufferp buf) (buffer-live-p buf))
      (setq buf (seq-find (lambda (b) (not (memq b before-bufs))) (agent-shell-buffers))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when item-id
          (rename-buffer (concat (buffer-name buf) "-" item-id) t))))
    buf))

(defun agent-shell-queue--resurrect-shell (target-shell &optional default-directory-override)
  "Resurrect or spawn target shell buffer TARGET-SHELL.
If TARGET-SHELL is live, returns it directly.
Otherwise, creates a new `agent-shell' buffer in DEFAULT-DIRECTORY-OVERRIDE
\(or target-shell's associated directory if target-shell is a directory bucket\)
and renames it to match TARGET-SHELL."
  (if (and target-shell (get-buffer target-shell) (buffer-live-p (get-buffer target-shell)))
      (get-buffer target-shell)
    (let* ((dir (or default-directory-override
                    (when (and target-shell (agent-shell-queue--dir-bucket-p target-shell))
                      (agent-shell-queue--dir-from-bucket target-shell))
                    default-directory))
           (canonical-dir (agent-shell-queue--canonicalize-dir dir))
           (buf (when (fboundp 'agent-shell-new-shell)
                  (let ((default-directory canonical-dir))
                    (agent-shell-new-shell)))))
      (when (and buf (buffer-live-p buf) target-shell (not (agent-shell-queue--dir-bucket-p target-shell)))
        (with-current-buffer buf
          (rename-buffer target-shell t)))
      buf)))

(declare-function shell-maker-busy "shell-maker")
(declare-function markdown-mode "markdown-mode")
(declare-function yaml-encode "yaml")
(declare-function yaml-parse-string "yaml")
(declare-function yaml-mode "yaml-mode")
(declare-function json-pretty-print-buffer "json")
(declare-function agent-shell-menu--session-shell-buffer "agent-shell-menu")

;; Macros

(defmacro with-agent-shell-queue (&rest body)
  "Evaluate BODY inside the queue load/save/refresh lifecycle.
Ensures the queue is loaded before BODY runs, then persists state and
refreshes the queue display after BODY completes.  Returns the value of
BODY's last form.  Does not protect against errors — if BODY signals,
the save and refresh are skipped."
  (declare (indent defun))
  `(progn
     (agent-shell-queue--ensure-loaded)
     (prog1
       (progn ,@body)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer))))

(defmacro agent-shell-queue--defstruct (type-name &rest field-specs)
  "Define a `cl-defstruct' TYPE-NAME with FIELD-SPECS and serializers.
Supported options:
  :no-serialize t      — skip this field in to-plist and from-plist
  :alias KEYWORD       — also try KEYWORD when reading from plist
  :to-plist FUNC       — call (FUNC raw-value) when writing to a plist
  :from-plist FUNC     — call (FUNC plist-value) when reading from a plist
Generates constructor TYPE-NAME--make plus:
  TYPE-NAME-to-plist   — struct → keyword-keyed plist
  TYPE-NAME-from-plist — keyword-keyed plist → struct"
  (declare (indent 1))
  (let* ((sname (symbol-name type-name))
         (ctor (intern (concat sname "--make")))
         (to-fn (intern (concat sname "-to-plist")))
         (from-fn (intern (concat sname "-from-plist")))
         (parsed (seq-map (lambda (spec)
                            (if (symbolp spec)
                                (list spec nil nil nil nil)
                              (let ((sym (car spec))
                                    (opts (cdr spec)))
                                (list sym
                                      (plist-get opts :no-serialize)
                                      (plist-get opts :alias)
                                      (plist-get opts :to-plist)
                                      (plist-get opts :from-plist)))))
                          field-specs))
         (field-names (seq-map #'car parsed))
         (serializable (seq-remove (lambda (p) (pcase-let ((`(,_ ,no-ser . ,_) p)) no-ser)) parsed)))
    `(progn
       (cl-defstruct (,type-name (:constructor ,ctor) (:copier nil))
         ,@field-names)
       (defun ,to-fn (item)
         ,(format "Convert %s ITEM to a keyword-keyed plist." sname)
         (list ,@(cl-mapcan
                  (lambda (p)
                    (pcase-let ((`(,f ,_ ,_ ,to-plist-fn ,_) p))
                      (let* ((kw (intern (concat ":" (symbol-name f))))
                             (raw `(,(intern (concat sname "-" (symbol-name f))) item)))
                        (list kw (if to-plist-fn `(,to-plist-fn ,raw) raw)))))
                  serializable)))
       (defun ,from-fn (plist)
         ,(format "Reconstruct a %s from keyword-keyed PLIST." sname)
         (,ctor ,@(cl-mapcan
                   (lambda (p)
                     (pcase-let ((`(,f ,_ ,alias ,_ ,from-plist-fn) p))
                       (let* ((kw (intern (concat ":" (symbol-name f))))
                              (raw (if alias
                                       `(or (plist-get plist ,kw) (plist-get plist ,alias))
                                     `(plist-get plist ,kw))))
                         (list kw (if from-plist-fn `(,from-plist-fn ,raw) raw)))))
                   serializable))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Registries

(cl-defstruct (agent-shell-queue-executor
               (:constructor agent-shell-queue-executor--make)
               (:copier nil))
  "Registry entry pairing a serializable NAME with an EXECUTOR function,
an optional CAPTURE function, and an optional CREATE function.
EXECUTOR: (item args) — called by `agent-shell-queue-send-item' to dispatch.
CAPTURE:  ()         — called during item creation to produce the args value;
                       nil means fall back to the standard text-capture buffer.
CREATE:   ()         — called by `agent-shell-queue-buffer-open-shell' when the
                       associated buffer is dead; should create and return a new
                       buffer of the same type, or nil to decline."
  name executor capture create)

(defvar agent-shell-queue--executors nil
  "List of `agent-shell-queue-executor' entries.
Only executors present here survive serialization.  Register entries with
`agent-shell-queue-register-executor'.")

(defun agent-shell-queue-register-executor (name executor &optional capture create)
  "Register EXECUTOR (and optional CAPTURE and CREATE) under NAME.
NAME may be a string or symbol; it is coerced to a string.  Re-registering
an existing name replaces the entry.  The name is written into serialized
queue state, so it must be stable across Emacs restarts.
CREATE, when provided, is a zero-arg function called by
`agent-shell-queue-buffer-open-shell' when the associated buffer is dead; it
should create and return a new buffer of the same type.
Returns EXECUTOR."
  (let ((name-str (cond
		   ((symbolp name) (symbol-name name))
		   ((stringp name) name)
		   (t (user-error "Impossible type for %s" name)))))
    (setq agent-shell-queue--executors
          (cons (agent-shell-queue-executor--make
                 :name name-str
		 :executor executor
		 :capture capture
		 :create create)
                (seq-remove (lambda (e)
			      (equal name-str (agent-shell-queue-executor-name e)))
                            agent-shell-queue--executors)))
    executor))

(defun agent-shell-queue--find-executor (name)
  "Return the `agent-shell-queue-executor' entry for NAME, or nil."
  (seq-find (lambda (e) (equal name (agent-shell-queue-executor-name e)))
            agent-shell-queue--executors))

(defun agent-shell-queue--executor-name (fn)
  "Return the registry name for FN, or nil if not registered."
  (when-let* ((e (seq-find (lambda (e) (eq fn (agent-shell-queue-executor-executor e)))
                           agent-shell-queue--executors)))
    (agent-shell-queue-executor-name e)))

(defun agent-shell-queue--executor-from-plist (name)
  "Deserialize executor NAME via the registry.
Returns nil (kind-dispatch) when NAME is nil or not found; emits a
warning for non-nil names that have no registry entry."
  (when name
    (if-let* ((e (agent-shell-queue--find-executor name)))
        (agent-shell-queue-executor-executor e)
      (progn
        (message "agent-shell-queue: unknown executor %S — item will use kind dispatch" name)
        nil))))

(cl-defstruct (agent-shell-queue-item-type
               (:constructor agent-shell-queue-item-type--make)
               (:copier nil))
  "Registry entry describing a queue item kind with its capabilities.
KIND: symbol — the :kind field value for items of this type.
LABEL: string — display name (Kind column and menus).
BUFFER-PRED: (lambda (buf)) → bool | nil means any buffer including unassigned.
DISPATCH-FN: (lambda (item buf-name)) — executes the item when dispatched.
INPUT-SPEC: plist describing how to collect user input:
  (:kind capture :mode MODE)   open capture buffer in MODE (nil → capture-mode)
  (:kind read :prompt P :fn F) single read via function F called with P
  (:kind none)                 no user input; args will be empty
  (:kind special :fn F)        zero-arg interactive function F handles everything"
  kind label buffer-pred dispatch-fn input-spec)

(defvar agent-shell-queue--item-types nil
  "List of `agent-shell-queue-item-type' entries.
Register entries with `agent-shell-queue-register-item-type'.
Built-in registrations are added at the end of this file.")


(cl-defun agent-shell-queue-register-item-type (&key kind label buffer-pred dispatch-fn input-spec)
  "Register item type with KIND, LABEL, BUFFER-PRED, DISPATCH-FN, and INPUT-SPEC.
KIND is a symbol; re-registering an existing KIND replaces the entry."
  (setq agent-shell-queue--item-types
        (cons (agent-shell-queue-item-type--make
               :kind kind :label label :buffer-pred buffer-pred
               :dispatch-fn dispatch-fn :input-spec input-spec)
              (seq-remove (lambda (e) (eq kind (agent-shell-queue-item-type-kind e)))
                          agent-shell-queue--item-types))))

(defun agent-shell-queue--type-for-kind (kind)
  "Return the `agent-shell-queue-item-type' for KIND symbol, or nil."
  (seq-find (lambda (e) (eq kind (agent-shell-queue-item-type-kind e)))
            agent-shell-queue--item-types))

(defun agent-shell-queue--types-for-buffer (buf)
  "Return item types compatible with BUF.
nil BUF (unassigned) accepts all types."
  (if (null buf)
      agent-shell-queue--item-types
    (seq-filter (lambda (type)
                  (let ((pred (agent-shell-queue-item-type-buffer-pred type)))
                    (or (null pred) (funcall pred buf))))
                agent-shell-queue--item-types)))

(defun agent-shell-queue--validate-kind-for-buffer (kind buf-or-nil)
  "Signal `user-error' when KIND is incompatible with BUF-OR-NIL.
nil BUF-OR-NIL (unassigned) always accepts any kind.  Returns t on success."
  (when-let* ((buf buf-or-nil)
              (type (agent-shell-queue--type-for-kind kind))
              (pred (agent-shell-queue-item-type-buffer-pred type)))
    (unless (funcall pred buf)
      (user-error "Item kind '%s' cannot be assigned to buffer '%s'"
                  (agent-shell-queue-item-type-label type)
                  (buffer-name buf))))
  t)

(defun agent-shell-queue--kind-needs-session-p (kind)
  "Return non-nil when KIND requires `agent-shell' session-mode compatibility."
  (when-let* ((type (agent-shell-queue--type-for-kind kind)))
    (eq (agent-shell-queue-item-type-buffer-pred type)
        #'agent-shell-queue--agent-shell-buffer-p)))

;; Data model

(agent-shell-queue--defstruct agent-shell-queue-item
  id
  args
  status
  kind
  background
  created
  dispatched
  completed
  response
  outcome
  directory
  (executor
   :to-plist agent-shell-queue--executor-name
   :from-plist agent-shell-queue--executor-from-plist)
  ;; Interjection fields — v1: one interjection per item.
  ;; interjection-prompt: text user typed; interjection-result: agent reply.
  (interjection-prompt :no-serialize t)
  (interjection-result :no-serialize t)
  ;; Re-enqueue tracking: reenqueued-from is the ID of the item this was
  ;; cloned from; reenqueued-as is a list of IDs created by re-enqueueing this.
  reenqueued-from
  reenqueued-as
  delay-before
  delay-after)

(defun agent-shell-queue--item-well-formed-p (item)
  "Return non-nil when ITEM has the minimum shape the queue UI requires.
Guards against a malformed persisted item (nil id/args/status) reaching
`agent-shell-queue-buffer-refresh', which errors on `split-string' with a
non-string ARGS."
  (and (agent-shell-queue-item-id item)
       (stringp (agent-shell-queue-item-args item))
       (agent-shell-queue-item-status item)))

(defun agent-shell-queue--migrate-item-if-stale (item)
  "Return ITEM or a current-layout copy with missing slots defaulted to nil.
Compares the vector length of ITEM against a freshly constructed default
instance; if shorter, copies the available slots into the new struct by index.
New trailing slots are left at nil.  Handles future field additions without
modification.  Returns nil (logging via `message') when ITEM cannot be
migrated to something matching `agent-shell-queue--item-well-formed-p' --
e.g. a persisted item so truncated that no fields overlap the current layout."
  (let* ((current (agent-shell-queue-item--make))
         (old-len (length item))
         (new-len (length current))
         (migrated (if (= old-len new-len)
                       item
                     (dotimes (i (1- (min old-len new-len)))
                       (aset current (1+ i) (aref item (1+ i))))
                     current)))
    (if (agent-shell-queue--item-well-formed-p migrated)
        migrated
      (message "agent-shell-queue: dropping malformed item during migration: %S" migrated)
      nil)))

(defun agent-shell-queue--migrate-all-stale-items ()
  "Upgrade every in-memory item to the current struct layout.
Replaces old-format items (missing the outcome slot) in the live store with
freshly constructed equivalents, dropping any that come out malformed.  Safe
to call repeatedly; up-to-date items are returned unchanged by
`agent-shell-queue--migrate-item-if-stale'."
  (seq-do (lambda (bucket)
            (setcdr bucket (seq-keep #'agent-shell-queue--migrate-item-if-stale (cdr bucket))))
          (agent-shell-queue-store-items agent-shell-queue--store)))

(defun agent-shell-queue--migrate-deferred-statuses ()
  "Convert any remaining `deferred' items to `blocked.skip' after load."
  (seq-do (lambda (bucket)
            (seq-do (lambda (item)
                      (when (eq (agent-shell-queue-item-status item) 'deferred)
                        (setf (agent-shell-queue-item-status item) 'blocked.skip)))
                    (cdr bucket)))
          (agent-shell-queue-store-items agent-shell-queue--store)))

(defun agent-shell-queue--blocked-status-p (status)
  "Return non-nil if STATUS is any blocked.* symbol."
  (and status (string-prefix-p "blocked." (symbol-name status))))

(defun agent-shell-queue--blocked-p (item)
  "Return non-nil if ITEM has any blocked.* status."
  (agent-shell-queue--blocked-status-p (agent-shell-queue-item-status item)))

(cl-defstruct (agent-shell-queue-store
               (:constructor agent-shell-queue--make-store)
               (:copier nil))
  "Queue state bundle: items, serialization format, and file path."
  items    ; (BUFFER-NAME . ITEM-LIST) alist
  format   ; symbol: plist | json | yaml
  file)    ; string: absolute path to state file

(defvar agent-shell-queue--items nil
  "Items alist used by format-specific serializers (e.g. org).
Bound dynamically by `agent-shell-queue--serialize-items' methods
before calling the format's serialize helper.")

(defvar agent-shell-queue--store
  (agent-shell-queue--make-store :items nil :format 'plist :file nil)
  "Live queue store.  Items are loaded from disk by --load, written by --save.")

(defun agent-shell-queue--sanitize-bucket (pair)
  "Return PAIR with any malformed items removed, logging drops."
  (let* ((before (cdr pair))
         (after (seq-filter #'agent-shell-queue--item-well-formed-p before))
         (dropped (- (length before) (length after))))
    (when (> dropped 0)
      (message "agent-shell-queue: dropped %d malformed item%s from bucket %s"
               dropped (if (= dropped 1) "" "s") (car pair)))
    (cons (car pair) after)))

(defun agent-shell-queue--restore-store-items (items)
  "Set the live store's items to ITEMS, dropping malformed ones.
Called by the persistence layer after deserializing from disk.  Defined here
so that the setf on the store struct slot stays in the same file as the struct."
  (setf (agent-shell-queue-store-items agent-shell-queue--store)
        (seq-map #'agent-shell-queue--sanitize-bucket items)))

(defun agent-shell-queue--normalize-running-item (item)
  "Reset ITEM's status from `running' to `active' for cross-session reload.
Defined here so setf on item struct slots stays in the same file as the struct."
  (setf (agent-shell-queue-item-status item) 'active)
  (setf (agent-shell-queue-item-dispatched item) nil))

 (cl-defstruct (agent-shell-queue-queue
                (:constructor agent-shell-queue-queue--make)
                (:copier nil))
   "Queue runtime state and reference to the active store."
   (store 'agent-shell-queue--store) ; symbol naming the live store variable
   (session-paused nil) ; list of buffer names paused from dispatch
   (editing-ids nil) ; list of item IDs currently open in an edit buffer
   (interjection-pending nil) ; boolean: blocks dispatch while interjection is in progress
   (halted-sessions nil)) ; list of buffer/bucket names halted on task abort/interrupt

(defvar agent-shell-queue--queue
  (agent-shell-queue-queue--make)
  "The active queue object.  Persisted via `savehist-additional-variables'.")

(defun agent-shell-queue--halted-on-abort-p (name)
  "Return non-nil when bucket or buffer NAME is halted due to task abort/interrupt."
  (and agent-shell-queue--queue
       name
       (member (if (stringp name) name (symbol-name name))
               (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue))))

(defun agent-shell-queue--mark-halted-on-abort (name)
  "Mark buffer or bucket NAME as halted on abort."
  (when name
    (let ((sname (if (stringp name) name (symbol-name name))))
      (cl-pushnew sname
                  (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue)
                  :test #'equal))))

(defun agent-shell-queue--clear-halted-on-abort (name)
  "Clear halted on abort status for buffer or bucket NAME."
  (when name
    (let ((sname (if (stringp name) name (symbol-name name))))
      (setf (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue)
            (delete sname
                    (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue))))))

(defun agent-shell-queue--response-has-question-p (response-text)
  "Return non-nil if RESPONSE-TEXT ends with an open question prompt."
  (when response-text
    (let ((trimmed (string-trim response-text)))
      (or (string-match-p "\\?\\s-*\\'" trimmed)
          (string-match-p "\\[y/N\\]\\s-*\\'" trimmed)
          (string-match-p "\\(Would you like\\|Do you want\\|Should I\\).*\\?\\s-*\\'" trimmed)))))

(defun agent-shell-queue--verify-recovery (buf item response-text)
  "Verify whether ITEM turn on BUF satisfies the 3 recovery criteria:
1. Uninterrupted (status is done, outcome is not aborted/interrupted).
2. No question (response-text does not end with open question).
3. Not in plan mode (buf mode-id not in blocked session modes).
Returns non-nil when all three criteria are satisfied."
  (and item
       (eq (agent-shell-queue-item-status item) 'done)
       (not (memq (agent-shell-queue-item-outcome item) '(aborted interrupted)))
       (not (agent-shell-queue--response-has-question-p response-text))
       (not (agent-shell-queue--session-mode-blocked-p buf))))

(defun agent-shell-queue-session-paused-p ()
  "Return non-nil when the current buffer's session queue dispatch is paused."
  (and agent-shell-queue--queue
       (member (buffer-name (current-buffer))
               (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))

(defun agent-shell-queue--session-pause-name (name)
  "Add NAME to the session-paused list and mark its active items blocked.
Shared by `agent-shell-queue-session-pause' (single buffer),
`agent-shell-queue-pause' (batch, every known buffer), and
`agent-shell-queue--on-interrupt'."
  (agent-shell-queue--cancel-pause-timer name)
  (cl-pushnew name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
  (seq-do (lambda (item)
            (when (eq (agent-shell-queue-item-status item) 'active)
              (setf (agent-shell-queue-item-status item) 'blocked.runner)))
          (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store)))))

 (defun agent-shell-queue--session-unpause-name (name)
   "Remove NAME from paused and halted lists, marking items active.
Shared by `agent-shell-queue-unpause-all-sessions' and
`agent-shell-queue-session-resume'."
   (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
         (seq-remove (lambda (n) (equal n name))
                     (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
   (agent-shell-queue--clear-halted-on-abort name)
   (seq-do (lambda (item)
             (when (eq (agent-shell-queue-item-status item) 'blocked.runner)
               (setf (agent-shell-queue-item-status item) 'active)))
           (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store)))))

;;;###autoload
(defun agent-shell-queue-add-directory (prompt dir &optional background delay-before delay-after)
  "Add a new active item for PROMPT in directory queue DIR.
Optional BACKGROUND, DELAY-BEFORE, and DELAY-AFTER configure task settings."
  (interactive
   (list (read-string "Prompt: ")
         (read-directory-name "Directory queue: ")
         current-prefix-arg))
  (with-agent-shell-queue
    (let* ((canon-dir (agent-shell-queue--canonicalize-dir dir))
           (bucket (agent-shell-queue--bucket-for-dir canon-dir))
           (item (agent-shell-queue--make-item prompt background 'prompt delay-before delay-after)))
      (setf (agent-shell-queue-item-directory item) canon-dir)
      (agent-shell-queue--add-item-to-bucket bucket item)
      item)))

;;; Execution Control

;;;###autoload
(defun agent-shell-queue-pause ()
  "Pause dispatch for every known session (batch session-pause)."
  (interactive)
  (with-agent-shell-queue
    (seq-do (lambda (bucket)
              (agent-shell-queue--session-pause-name (car bucket)))
            (agent-shell-queue-store-items agent-shell-queue--store))
    (message "agent-shell-queue: all sessions PAUSED")))

;;;###autoload
(defun agent-shell-queue-resume ()
  "Resume dispatch for every known session.
Alias for `agent-shell-queue-unpause-all-sessions'."
  (interactive)
  (agent-shell-queue-unpause-all-sessions))

 (defun agent-shell-queue-unpause-all-sessions ()
   "Clear the per-session pause list and halted-on-abort list."
   (interactive)
   (with-agent-shell-queue
     (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) nil)
     (setf (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue) nil)
     (seq-do (lambda (item)
               (when (eq (agent-shell-queue-item-status item) 'blocked.runner)
                 (setf (agent-shell-queue-item-status item) 'active)))
             (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store)))
     (message "agent-shell-queue: all session pauses and halts cleared"))
  (seq-do (lambda (bucket)
            (when-let* ((buf (get-buffer (car bucket)))
                        (_ (buffer-live-p buf)))
              (agent-shell-queue--send-next-for-buffer buf)))
          (agent-shell-queue-store-items agent-shell-queue--store)))

(defun agent-shell-queue-session-pause (&optional buf)
  "Pause dispatch for BUF (default: current `agent-shell' session)."
  (interactive
   (list (agent-shell-queue--pick-shell-with-state "Pause dispatch for: ")))
  (when-let* ((name (buffer-name buf)))
    (with-agent-shell-queue
      (agent-shell-queue--session-pause-name name)
      (message "agent-shell-queue: %s PAUSED" name))))

(defun agent-shell-queue-session-resume (&optional buf)
  "Resume dispatch for BUF (default: current `agent-shell' session).
Any running `pause' or `compact' item for BUF is marked done automatically."
  (interactive
   (list (agent-shell-queue--pick-shell-with-state "Resume dispatch for: ")))
  (when-let* ((name (buffer-name buf)))
    (with-agent-shell-queue
      (setq agent-shell-queue--compact-running
            (seq-remove (lambda (it) (equal (car it) name)) agent-shell-queue--compact-running))
      (seq-do (lambda (it)
                (when (and (eq (agent-shell-queue-item-status it) 'running)
                           (memq (agent-shell-queue-item-kind it) '(pause compact)))
                  (setf (agent-shell-queue-item-status it) 'done)
                  (setf (agent-shell-queue-item-completed it) (float-time))
                  (agent-shell-queue--append-done-log name it)
                  (run-hook-with-args 'agent-shell-queue-item-done-hook name it)))
              (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store))))
      (agent-shell-queue--session-unpause-name name)
      (message "agent-shell-queue: %s resumed" name))
    (agent-shell-queue--send-next-for-buffer buf)))

(defun agent-shell-queue--poll-for-idle-and-resume (buf attempt)
  "Poll BUF until idle, then call `agent-shell-queue-session-resume'.
ATTEMPT tracks retry count up to 20 times (~10 seconds total).
Retries every 0.5 seconds up to 20 times (~10 seconds total).
Called by `agent-shell-queue-recover-stuck-shell'."
  (cond
   ((not (buffer-live-p buf))
    (message "agent-shell-queue: recovery abandoned — buffer was killed"))
   ((> attempt 20)
    (message "agent-shell-queue: %s still busy after 10s — call `agent-shell-queue-session-resume' manually"
             (buffer-name buf)))
   ((with-current-buffer buf (not (shell-maker-busy)))
    (agent-shell-queue-session-resume buf))
   (t
    (run-with-timer 0.5 nil #'agent-shell-queue--poll-for-idle-and-resume buf (1+ attempt)))))

(defun agent-shell-queue-recover-stuck-shell (&optional buf)
  "Interrupt stuck shell BUF and auto-resume queue dispatch when it becomes idle.
Marks any running item as aborted, sends an interrupt to the shell, then
polls until the shell is no longer busy before resuming dispatch.
Use this when the shell is frozen with no prompt appearing after the last turn."
  (interactive
   (list (agent-shell-queue--pick-shell-with-state "Recover stuck shell: ")))
  (when-let* ((buf-name (buffer-name buf))
              (_ (buffer-live-p buf)))
    (with-agent-shell-queue
      (seq-do (lambda (item)
                (when (eq (agent-shell-queue-item-status item) 'running)
                  (setf (agent-shell-queue-item-status item) 'aborted)
                  (setf (agent-shell-queue-item-completed item) (float-time))
                  (setf (agent-shell-queue-item-outcome item) 'interrupted)))
              (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (with-current-buffer buf
        (agent-shell-interrupt)))
    (message "agent-shell-queue: recovering %s — waiting for shell to become idle..." buf-name)
    (agent-shell-queue--poll-for-idle-and-resume buf 0)))
;;;###autoload
(defun agent-shell-queue-flush ()
  "Force-save queue state to disk immediately."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue--save)
  (message "agent-shell-queue: state saved to disk"))

;;;###autoload
(defun agent-shell-queue-reload ()
  "Pause, flush, reload source code, and reload state from disk.
Stops the idle timer, drops all turn-complete subscriptions, reloads
`agent-shell-queue.el' from source, re-reads queue state from disk, and
reinstates subscriptions for buffers with active/running items.
Every known session is paused before reload — the session-paused list
survives the reload (it lives on `agent-shell-queue--queue', which `defvar'
does not reset) — so nothing dispatches until `agent-shell-queue-resume'
is called."
  (interactive)
  (seq-do (lambda (bucket)
            (agent-shell-queue--session-pause-name (car bucket)))
          (agent-shell-queue-store-items agent-shell-queue--store))
  (agent-shell-queue--save)
  (when agent-shell-queue--idle-timer
    (cancel-timer agent-shell-queue--idle-timer)
    (setq agent-shell-queue--idle-timer nil))
  (when agent-shell-queue--idle-flush-timer
    (cancel-timer agent-shell-queue--idle-flush-timer)
    (setq agent-shell-queue--idle-flush-timer nil))

  (seq-do (lambda (pair) (cancel-timer (cdr pair))) agent-shell-queue--wait-timers)
  (setq agent-shell-queue--wait-timers nil)
  (seq-do (lambda (it) (agent-shell-queue--drop-subscription (car it)))
          (copy-sequence agent-shell-queue--subscriptions))
  (run-hooks 'agent-shell-queue-before-reload-hook)
  (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
  (setq agent-shell-queue--loaded nil
        agent-shell-queue--subscriptions nil)
  (if-let* ((lib (locate-library "agent-shell-queue"))
             (src (if (string-suffix-p ".elc" lib)
                      (concat (file-name-sans-extension lib) ".el")
                    lib))
             (_ (file-exists-p src)))
      (load-file src)
    (error "Agent-shell-queue-reload: cannot locate source file"))
  (agent-shell-queue--load)
  (setq agent-shell-queue--loaded t)
  (seq-do (lambda (it)
            (when-let* ((buf (get-buffer (car it)))
                        (_ (buffer-live-p buf))
                        (_ (with-current-buffer buf (derived-mode-p 'agent-shell-mode)))
                        (_ (seq-some (lambda (item)
                                       (memq (agent-shell-queue-item-status item) '(active running)))
                                     (cdr it))))
              (agent-shell-queue--ensure-subscription buf)))
          (agent-shell-queue-store-items agent-shell-queue--store))

  (run-hooks 'agent-shell-queue-after-reload-hook)
  (agent-shell-queue--refresh-buffer)

  (when-let* ((buf (get-buffer "*agent-shell-queue*")))
    (with-current-buffer buf
      (force-mode-line-update)))
  (message "agent-shell-queue: reloaded from disk — still PAUSED (M-x agent-shell-queue-resume to run)"))

;;;###autoload
(defun agent-shell-queue-clear-unparsable ()
  "Remove items whose struct fields cannot be read; print each to *Messages*.
Useful after a code reload that left in-memory structs with mismatched layouts.
When called interactively, prompts y/n/a for each candidate before removing it.
Affected buffer queues are paused and the queue state is saved."
  (interactive "P")
  (agent-shell-queue--ensure-loaded)
  (let ((candidates (thread-last
		      (agent-shell-queue-store-items agent-shell-queue--store)
                      (seq-mapcat
                       (lambda (pair)
                         (thread-last (cdr pair)
                                      (seq-map (lambda (it)
                                                 (condition-case _
                                                     (ignore (agent-shell-queue-item-id it)
                                                             (agent-shell-queue-item-args it)
                                                             (agent-shell-queue-item-status it))
                                                   (error (cons (car pair) it)))))
                                      (seq-filter #'identity))))))
        removed
        (accept-all current-prefix-arg))
    (cond
     ((null candidates)
      (message "agent-shell-queue: no unparsable items found"))
     (t
      (seq-do (lambda (it)
                (let ((buf-name (car it))
                      (item (cdr it)))
                  (message "agent-shell-queue: unparsable item in %s: %S" buf-name item)
                  (when (or accept-all (not (called-interactively-p 'any))
                            (let ((ch (read-char-choice
                                       (format "Remove from %s? (y)es (n)o (a)ll: " buf-name)
                                       '(?y ?n ?a))))
                              (cond ((eq ch ?a) (setq accept-all t))
                                    ((eq ch ?n) nil)
                                    (t t))))
                    (when-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
                      (setcdr cell (seq-remove (lambda (it) (eq it item)) (cdr cell))))
                    (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
                    (push it removed))))
              candidates)
      (cond
       ((null removed)
        (message "agent-shell-queue: no items removed"))
       (t
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (message "agent-shell-queue: removed %d unparsable item(s); affected queues paused"
                 (length removed))))))))

(defun agent-shell-queue--revert-disk-view (file _ignore-auto _noconfirm)
  "Re-read FILE into the disk-state view buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents file)
    (goto-char (point-min))))

;;;###autoload
(defun agent-shell-queue-show-disk-state ()
  "Display the on-disk queue state file in a read-only popup buffer."
  (interactive)
  (if-let* ((file (agent-shell-queue--state-file))
             (_ (file-exists-p file))
             (buf (get-buffer-create "*agent-shell-queue-disk*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (goto-char (point-min)))
        (setq buffer-read-only t)
        (setq-local revert-buffer-function
                    (lambda (ignore-auto noconfirm)
                      (agent-shell-queue--revert-disk-view file ignore-auto noconfirm)))
        (set-visited-file-name nil t)
        (rename-buffer "*agent-shell-queue-disk*" t)
        (pcase (file-name-extension file)
          ("el" (emacs-lisp-mode))
          ("json" (when (fboundp 'json-mode) (json-mode)))
          ((or "yaml" "yml") (when (fboundp 'yaml-mode) (yaml-mode))))
        (read-only-mode 1)
        (display-buffer buf '(display-buffer-below-selected (window-height . 0.4))))
    (user-error "Queue state file does not exist: %s" file)))


(defun agent-shell-queue--on-interrupt (&optional _force)
  "Flag session and bucket as halted-on-abort on interrupt.
Installed as :before advice on `agent-shell-interrupt'."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((_ (derived-mode-p 'agent-shell-mode))
              (buf-name (buffer-name)))
    (agent-shell-queue--session-pause-name buf-name)
    (agent-shell-queue--mark-halted-on-abort buf-name)
    (when-let* ((running-item (seq-find (lambda (it) (eq (agent-shell-queue-item-status it) 'running))
                                        (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))))
		(dir (agent-shell-queue-item-directory running-item)))
      (agent-shell-queue--mark-halted-on-abort (agent-shell-queue--bucket-for-dir dir)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

(advice-add 'agent-shell-interrupt :before #'agent-shell-queue--on-interrupt)


(defun agent-shell-queue--default-pick-buffer (prompt)
  "Pick a live `agent-shell' buffer using PROMPT via `completing-read'."
  (when-let* ((bufs (agent-shell-buffers)))
    (get-buffer (completing-read prompt (seq-map #'buffer-name bufs) nil t))))

(defun agent-shell-queue--pick-shell-with-state (prompt)
  "Select a live `agent-shell' buffer via PROMPT using queue-state annotations."
  (let* ((bufs (or (agent-shell-buffers)
                   (user-error "No live agent-shell buffers")))
         (table (seq-map (lambda (buf)
                           (cons (buffer-name buf)
                                 (agent-shell-queue--buffer-state-label
                                  (buffer-name buf))))
                         bufs)))
    (get-buffer
     (annotated-completing-read table
                                :prompt prompt
                                :category 'agent-shell-buffer
                                :require-match t))))

(defvar agent-shell-queue--loaded nil
  "Non-nil after the on-disk state has been read into memory.")

(defvar agent-shell-queue--idle-timer nil
  "Idle timer for auto-sending active queue items.")

(defvar agent-shell-queue--idle-flush-timer nil
  "Idle timer that saves queue state after a period of user inactivity.")

(defvar agent-shell-queue--subscriptions nil
  "Alist of (BUF-NAME . TOKEN) for active `turn-complete' subscriptions.
Each entry is registered when the first item is queued for that buffer and
removed when the queue for that buffer empties or the buffer is killed.")

(defvar agent-shell-queue-blocked-session-modes '("dontAsk" "plan")
  "Session mode IDs that block queue dispatch.
When a target shell is in one of these modes the item is not sent and
the session queue is paused until the mode changes.")

(defun agent-shell-queue--gen-id ()
  "Generate a short unique item ID: q + one digit + four alphanumeric chars."
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (concat "q"
            (number-to-string (random 10))
            (apply #'string
                   (seq-map (lambda (_it) (aref chars (random 36))) (make-list 4 nil))))))

(defun agent-shell-queue--clean-args (args)
  "Remove trailing whitespace from every line of ARGS."
  (string-join
   (thread-last
    (split-string args "\n")
    (seq-map #'string-trim-right))
   "\n"))

(defun agent-shell-queue--make-item (prompt &optional background kind delay-before delay-after)
  "Return new active item for PROMPT, BACKGROUND, KIND, and delays."
  (agent-shell-queue-item--make
   :id (agent-shell-queue--gen-id)
   :args (agent-shell-queue--clean-args prompt)
   :status 'active
   :kind (or kind 'prompt)
   :background background
   :created (float-time)
   :delay-before delay-before
   :delay-after delay-after))

;; Local utilities

(defun agent-shell-queue--state-file ()
  "Return the path to the on-disk queue state file."
  (funcall agent-shell-queue-state-file-function))

(defun agent-shell-queue--current-store ()
  "Return the live store, ensuring format and file reflect current config."
  (setf (agent-shell-queue-store-format agent-shell-queue--store) agent-shell-queue-serialization-format)
  (setf (agent-shell-queue-store-file agent-shell-queue--store)(agent-shell-queue--state-file))
  agent-shell-queue--store)

;; Buffer predicates

(defun agent-shell-queue--agent-shell-buffer-p (buf)
  "Return non-nil when BUF is a live `agent-shell' session buffer."
  (and (buffer-live-p buf)
       (with-current-buffer buf (derived-mode-p 'agent-shell-mode))))

(defun agent-shell-queue--eshell-buffer-p (buf)
  "Return non-nil when BUF is a live eshell buffer."
  (and (buffer-live-p buf)
       (with-current-buffer buf (derived-mode-p 'eshell-mode))))

(defun agent-shell-queue--eat-buffer-p (buf)
  "Return non-nil when BUF is a live eat buffer."
  (and (buffer-live-p buf)
       (with-current-buffer buf (derived-mode-p 'eat-mode))))

(defun agent-shell-queue--pick-buffer (prompt)
  "Pick a live `agent-shell' buffer using PROMPT."
  (funcall agent-shell-queue-pick-buffer-function prompt))

(defun agent-shell-queue--candidate-buffers-for-kind (kind)
  "Return live buffers compatible with KIND, or nil when kind accepts any.
When the kind has no buffer-pred (any-buffer), returns all live buffers."
  (when-let* ((type (agent-shell-queue--type-for-kind kind))
              (pred (agent-shell-queue-item-type-buffer-pred type)))
    (seq-filter (lambda (b) (and (buffer-live-p b) (funcall pred b)))
                (buffer-list))))

(defun agent-shell-queue--annotation (text max-width)
  "Return TEXT truncated to MAX-WIDTH with ellipsis for annotation."
  (truncate-string-to-width (or text "") max-width nil nil "…"))

(defun agent-shell-queue--pick-buffer-for-kind (kind &optional prompt)
  "Pick a buffer compatible with KIND via ACR using PROMPT, offering unassigned.
Returns a live buffer, or nil meaning the unassigned bucket.
When no compatible buffers exist: falls through to nil unless
`agent-shell-queue-strict-buffer-assignment' is non-nil."
  (let* ((type (agent-shell-queue--type-for-kind kind))
         (pred (when type (agent-shell-queue-item-type-buffer-pred type)))
         (candidates (if pred
                         (seq-filter (lambda (b) (and (buffer-live-p b) (funcall pred b)))
                                     (buffer-list))
                       (agent-shell-buffers)))
         (prompt (or prompt "Target: ")))
    (cond
     ((null candidates)
      (when agent-shell-queue-strict-buffer-assignment
        (user-error "No live buffer compatible with kind '%s'" kind))
      nil)
     (t
      (let* ((rows (seq-map (lambda (buf)
                              (cons (buffer-name buf)
                                    (agent-shell-queue--annotation
                                     (agent-shell-queue--buffer-state-label
                                      (buffer-name buf))
                                     60)))
                            candidates))
             (table (cons (cons agent-shell-queue--unassigned-key "defer — assign later")
                          rows))
             (choice (annotated-completing-read table
                                                :prompt prompt
                                                :category 'agent-shell-buffer
                                                :require-match t)))
        (if (equal choice agent-shell-queue--unassigned-key)
            nil
          (get-buffer choice)))))))

(defun agent-shell-queue--format-age (delta)
  "Format DELTA time-value as a short relative age string."
  (let ((s (float-time delta)))
    (cond ((< s 60) (format "%ds" (truncate s)))
          ((< s 3600) (format "%dm" (truncate (/ s 60))))
          ((< s 86400) (format "%dh" (truncate (/ s 3600))))
          (t (format "%dd" (truncate (/ s 86400)))))))

(defun agent-shell-queue--ensure-loaded ()
  "Load queue state from disk on first call.
Queue state is loaded lazily when first accessed, not at Emacs startup.
This ensures the queue package is fully initialized before loading."
  (agent-shell-queue--migrate-queue-struct)
  (unless agent-shell-queue--loaded
    (agent-shell-queue--load)
    (setq agent-shell-queue--loaded t)
    (let ((count (length (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store)))))
      (when (> count 0)
        (message "agent-shell-queue: loaded %d item%s from disk"
                 count (if (= count 1) "" "s"))))))

(defun agent-shell-queue--save-on-exit ()
  "Persist queue state at Emacs exit, but only if it was loaded this session.
Avoids creating or clobbering the state file for sessions that never touched
the queue, such as a batch process that merely requires this file."
  (when agent-shell-queue--loaded
    (agent-shell-queue--save)))

(add-hook 'kill-emacs-hook #'agent-shell-queue--save-on-exit)

;; Store predicates

(defun agent-shell-queue--item-id-matches-p (id item)
  "Return non-nil when ITEM's id equals ID."
  (equal (agent-shell-queue-item-id item) id))

(defun agent-shell-queue--bucket-empty-p (pair)
  "Return non-nil when PAIR is a bucket cell whose item list is empty."
  (null (cdr pair)))

(defun agent-shell-queue--wait-timer-id-matches-p (id pair)
  "Return non-nil when PAIR is a wait-timer cell keyed by ID."
  (equal (car pair) id))

;; Queue operations

(defun agent-shell-queue--item-by-id (id)
  "Return (BUF-NAME . ITEM) for the item with ID, or nil."
  (thread-last
    ;; intput
    (agent-shell-queue-store-items agent-shell-queue--store)
    ;; pipeline handlers
    (seq-mapcat (lambda (pair)
                  (seq-map (lambda (item) (cons (car pair) item)) (cdr pair))))
    (seq-find (lambda (it) (equal (agent-shell-queue-item-id (cdr it)) id)))))

(defun agent-shell-queue-get-item-by-id (id)
  "Return (BUF-NAME . ITEM) for the queue item with ID, or nil.
Ensures the queue is loaded before searching.  Public API wrapper around
the internal `agent-shell-queue--item-by-id'."
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue--item-by-id id))

(defun agent-shell-queue-find-item (&optional prompt)
  "Interactively pick a queue item and return its (BUF-NAME . ITEM) pair.
Uses `annotated-completing-read' with item IDs and prompts as annotations.
PROMPT overrides the default completion prompt.  Useful for debugging."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((choices (thread-last
                    (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-mapcat (lambda (bucket)
                                  (seq-map (lambda (item) (cons (car bucket) item))
                                           (cdr bucket))))
                    (seq-map (lambda (pair)
                               (let* ((item (cdr pair))
                                      (id (agent-shell-queue-item-id item))
                                      (status (symbol-name (agent-shell-queue-item-status item)))
                                      (preview (agent-shell-queue--annotation
                                                (agent-shell-queue-item-args item) 50)))
                                 (cons id (format "[%s] %s  %s" status (car pair) preview)))))))
         (choice (annotated-completing-read
                  choices
                  :prompt (or prompt "Queue item: ")
                  :category 'agent-shell-queue-item
                  :require-match t)))
    (when choice
      (agent-shell-queue--item-by-id choice))))

(defun agent-shell-queue--add-item-to-bucket (bucket-name item)
  "Append ITEM to the BUCKET-NAME bucket in the live store items."
  (if-let* ((pair (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
      ;; then
      (setcdr pair (append (cdr pair) (list item)))
    ;; else
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (append (agent-shell-queue-store-items agent-shell-queue--store) (list (list bucket-name item))))))

(defun agent-shell-queue-add (prompt buf &optional background delay-before delay-after)
  "Add a new active item for PROMPT destined for BUF.  Save and refresh.
When BACKGROUND is non-nil the item is flagged for sub-agent execution.
Optional DELAY-BEFORE and DELAY-AFTER specify per-task pre-dispatch and
post-completion delays in seconds.
Registers a `turn-complete' subscription on BUF if one is not already active."
  (with-agent-shell-queue
    (let ((item (agent-shell-queue--make-item prompt background 'prompt delay-before delay-after)))
      (setf (agent-shell-queue-item-directory item)
            (buffer-local-value 'default-directory buf))
      (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
      (agent-shell-queue--ensure-subscription buf)
      item)))
;; Prompt capture helpers

(defun agent-shell-queue-add-unassigned (prompt &optional background delay-before delay-after)
  "Add a new item for PROMPT to the unassigned bucket.
Optional BACKGROUND, DELAY-BEFORE, and DELAY-AFTER configure task settings.
Unassigned items display in blue and sort after all shell-assigned items."
  (with-agent-shell-queue
    (let ((item (agent-shell-queue--make-item prompt background 'prompt delay-before delay-after)))
      (agent-shell-queue--add-item-to-bucket agent-shell-queue--unassigned-key item)
      item)))

(defun agent-shell-queue-remove (id)
  "Remove the item with ID from the queue.  Save.
Drops the `turn-complete' subscription for any bucket that becomes empty.
Cancels any pending wait timer for the item.
Always logs the removed item's prompt to *Messages*."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((pair (assoc id agent-shell-queue--wait-timers)))
    (cancel-timer (cdr pair))
    (setq agent-shell-queue--wait-timers
          (seq-remove (lambda (pair) (agent-shell-queue--wait-timer-id-matches-p id pair))
                      agent-shell-queue--wait-timers)))
  (when-let* ((found (agent-shell-queue--item-by-id id)))
    (message "agent-shell-queue: removed %s [%s]: %s"
             id (car found)
             (agent-shell-queue-item-args (cdr found))))
  (let ((before-names (seq-map #'car (agent-shell-queue-store-items agent-shell-queue--store))))
    (seq-do (lambda (it)
              (setcdr it (seq-remove (lambda (item) (agent-shell-queue--item-id-matches-p id item)) (cdr it))))
            (agent-shell-queue-store-items agent-shell-queue--store))
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))
    (seq-do #'agent-shell-queue--drop-subscription
            (seq-remove (lambda (it) (assoc it (agent-shell-queue-store-items agent-shell-queue--store)))
                        before-names)))
  (agent-shell-queue--save))

(defun agent-shell-queue--confirm-remove (item)
  "Prompt the user to confirm removing ITEM.
Returns t to proceed, nil to skip.  When user answers \\='a\\=', sets
`agent-shell-queue--remove-all-confirmed' so future calls return t immediately."
  (or agent-shell-queue--remove-all-confirmed
      (pcase (read-char-choice
              (format "Remove [%s]? (y)es (n)o (a)ll: "
                      (truncate-string-to-width
                       (agent-shell-queue-item-args item) 60 nil nil "..."))
              '(?y ?n ?a ?Y ?N ?A))
              ((or ?y ?Y) t)
              ((or ?a ?A) (setq agent-shell-queue--remove-all-confirmed t) t)
              (_ nil))))

(defun agent-shell-queue-defer (id)
  "Toggle status of item ID between `active' and `blocked.skip'.  Save."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (setf (agent-shell-queue-item-status item)
          (if (eq (agent-shell-queue-item-status item) 'active)
              'blocked.skip
            'active))
    (agent-shell-queue--save)))

(defun agent-shell-queue-unblock (id)
  "Unblock item ID: set blocked.task/blocked.skip to active.
For blocked.task, cascades active to subsequent blocked.dep items."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair))
              (item (cdr pair)))
    (pcase (agent-shell-queue-item-status item)
      ('blocked.task
       (setf (agent-shell-queue-item-status item) 'active)
       (when-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))
                   (items (cdr cell))
                   (idx (cl-position id items :key #'agent-shell-queue-item-id :test #'equal)))
         (seq-do (lambda (it)
                   (when (eq (agent-shell-queue-item-status it) 'blocked.dep)
                     (setf (agent-shell-queue-item-status it) 'active)))
                 (seq-take-while
                  (lambda (it) (not (eq (agent-shell-queue-item-status it) 'blocked.task)))
                  (seq-drop items (1+ idx))))))
      ((pred agent-shell-queue--blocked-status-p)
       (setf (agent-shell-queue-item-status item) 'active))
      (_ (user-error "Item %s is not blocked" id)))
    (agent-shell-queue--save)))

(defun agent-shell-queue-edit (id new-prompt)
  "Replace the args of item ID with NEW-PROMPT.  Save."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-args (cdr pair)) (agent-shell-queue--clean-args new-prompt))
    (agent-shell-queue--save)))

(defun agent-shell-queue-set-background-task (id flag)
  "Set the background flag of item ID to FLAG.  Save."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (setf (agent-shell-queue-item-background (cdr pair)) flag)
    (agent-shell-queue--save)))

(defun agent-shell-queue--move (id delta)
  "Shift item ID by DELTA positions within its buffer's list."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (cell (assoc (car pair) (agent-shell-queue-store-items agent-shell-queue--store)))
              (items (cdr cell))
              (idx (cl-position id items :key #'agent-shell-queue-item-id :test #'equal))
              (new-idx (+ idx delta))
              (_ (>= new-idx 0))
              (_ (< new-idx (length items))))
    (let ((new-items (copy-sequence items)))
      (cl-rotatef (nth idx new-items) (nth new-idx new-items))
      (setcdr cell new-items)
      (agent-shell-queue--save))))

(defun agent-shell-queue-move-up (id)
  "Move item ID one position earlier in its buffer's queue."
  (agent-shell-queue--move id -1))

(defun agent-shell-queue-move-down (id)
  "Move item ID one position later in its buffer's queue."
  (agent-shell-queue--move id 1))

(defun agent-shell-queue--has-running-item-p (buf-name)
  "Return non-nil if BUF-NAME's queue has any item with status `running'."
  (seq-some (lambda (item)
              (eq (agent-shell-queue-item-status item) 'running))
            (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))))

(defun agent-shell-queue--copy-item-to-end (buf-name item)
  "Append a fresh copy of ITEM to the end of BUF-NAME's queue.
The copy gets a new ID, status `active', and a fresh creation timestamp.
Args, kind, background, executor, and directory are carried over.
Returns the new item's ID."
  (agent-shell-queue--ensure-loaded)
  (let* ((new-id (agent-shell-queue--gen-id))
         (copy (agent-shell-queue-item--make
                :id new-id
                :args (agent-shell-queue-item-args item)
                :status 'active
                :kind (agent-shell-queue-item-kind item)
                :background (agent-shell-queue-item-background item)
                :created (float-time)
                :directory (agent-shell-queue-item-directory item)
                :executor (agent-shell-queue-item-executor item))))
    (if-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
        (setcdr cell (append (cdr cell) (list copy)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (append (agent-shell-queue-store-items agent-shell-queue--store)
                    (list (list buf-name copy)))))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    new-id))

(defun agent-shell-queue--insert-item-after (buf-name item ref-id)
  "Insert ITEM into BUF-NAME queue immediately after the item with REF-ID.
Returns the new item's ID."
  (agent-shell-queue--ensure-loaded)
  (when-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
    (let* ((items (cdr cell))
           (idx (cl-position ref-id items :key #'agent-shell-queue-item-id :test #'equal))
           (new-items (if idx
                          (append (seq-take items (1+ idx))
                                  (list item)
                                  (seq-drop items (1+ idx)))
                        (append items (list item)))))
      (setf (cdr cell) new-items)))
  (agent-shell-queue-item-id item))


(defun agent-shell-queue--insert-resume-task (buf-name aborted-item)
  "Insert a blocked.task resume item after ABORTED-ITEM in BUF-NAME.
The item asks to resume from the aborted task's arguments."
  (let* ((prev-args (agent-shell-queue-item-args aborted-item))
         (resume-args (format "resume work on previous task:\n\n```\n%s\n```" prev-args))
         (new-item (agent-shell-queue-item--make
                    :id (agent-shell-queue--gen-id)
                    :args resume-args
                    :status 'blocked.task
                    :kind 'prompt
                    :created (float-time)
                    :executor (agent-shell-queue-item-executor aborted-item))))
    (agent-shell-queue--insert-item-after
     buf-name new-item (agent-shell-queue-item-id aborted-item))
    (let ((new-id (agent-shell-queue-item-id new-item)))
      (when-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))
                  (items (cdr cell))
                  (idx (cl-position new-id items :key #'agent-shell-queue-item-id :test #'equal)))
        (seq-do (lambda (it)
                  (when (eq (agent-shell-queue-item-status it) 'active)
                    (setf (agent-shell-queue-item-status it) 'blocked.dep)))
                (seq-take-while
                 (lambda (it) (not (eq (agent-shell-queue-item-status it) 'blocked.task)))
                 (seq-drop items (1+ idx))))))))

(defun agent-shell-queue--assign-item (id new-buf-name)
  "Move the item with ID to the NEW-BUF-NAME bucket.
NEW-BUF-NAME may be a live buffer name or `agent-shell-queue--unassigned-key'.
Drops the subscription on the old bucket if it empties; ensures one on the new."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (old-name (car pair))
              (item (cdr pair))
              (_ (not (equal old-name new-buf-name))))
    (let ((old-cell (assoc old-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (setf (cdr old-cell)
            (seq-remove (lambda (item) (agent-shell-queue--item-id-matches-p id item)) (cdr old-cell))))

    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (seq-remove #'agent-shell-queue--bucket-empty-p (agent-shell-queue-store-items agent-shell-queue--store)))

    (unless (assoc old-name (agent-shell-queue-store-items agent-shell-queue--store))
      (agent-shell-queue--drop-subscription old-name))

    (if-let* ((new-cell (assoc new-buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
        (setcdr new-cell (append (cdr new-cell) (list item)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (append (agent-shell-queue-store-items agent-shell-queue--store) (list (list new-buf-name item)))))

    (when-let* ((_ (not (equal new-buf-name agent-shell-queue--unassigned-key)))
                (new-buf (get-buffer new-buf-name)))
      (agent-shell-queue--ensure-subscription new-buf))

    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue--pause-and-save (buf-name)
  "Add BUF-NAME to the paused-sessions list, persist state, and refresh the buffer."
  (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue--alert (message &rest args)
  "Send an alert notification with MESSAGE and ARGS using `alert`.
Intercepts and logs any notification backend error."
  (condition-case err
      (apply #'alert message args)
    (error
     (message "agent-shell-queue: alert error: %s" (error-message-string err)))))

(defun agent-shell-queue--redirect-dead-target (id buf-name)
  "Alert and pause BUF-NAME's session queue when its target buffer is gone.
Emits a high-severity persistent alert referencing ID, adds BUF-NAME to the
session-paused list, persists state, and returns nil."
  (agent-shell-queue--ensure-loaded)
  (agent-shell-queue--alert (format "Queue for '%s' paused — target buffer is gone (item %s)" buf-name id)
                            :title (format "Queue → %s" buf-name)
                            :category 'agent-shell-queue
                            :severity 'high
                            :persistent t)
  (agent-shell-queue--pause-and-save buf-name)
  nil)

(defun agent-shell-queue--handle-stale-item (id buf-name err)
  "Pause BUF-NAME and defer item ID after a struct access error ERR.
Called when dispatching item ID raises an error, which indicates the item
was built against an older struct definition before a code reload.
Migrates all in-memory items to the current struct layout before saving so
that the subsequent --save does not fail on other stale items."
  (cl-pushnew id agent-shell-queue--stale-item-ids :test #'equal)
  (agent-shell-queue--migrate-all-stale-items)
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (condition-case nil
        (setf (agent-shell-queue-item-status (cdr pair)) 'blocked.skip)
      (error nil)))

  (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)

  (message "agent-shell-queue: item %s in %s appears stale after code reload; blocked and queue paused (%s)"
           id buf-name err)

  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue--complete-item (item buf-name)
  "Mark ITEM in BUF-NAME as done and trigger the next dispatch cycle.
Records completion time, appends to the done log, persists state,
refreshes the queue buffer, fires the empty-queue alert if warranted,
and dispatches the next item for BUF-NAME if the buffer is still live."
  (setf (agent-shell-queue-item-completed item) (float-time))
  (setf (agent-shell-queue-item-status item) 'done)
  (agent-shell-queue--append-done-log buf-name item)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)
  (agent-shell-queue--alert-if-empty)
  (when-let* ((buf (get-buffer buf-name)))
    (agent-shell-queue--send-next-for-buffer buf)))

(defun agent-shell-queue--wait-timer-fire (id)
  "Handle expiry of the wait timer for item ID."
  (setq agent-shell-queue--wait-timers
        (seq-remove (lambda (pair) (agent-shell-queue--wait-timer-id-matches-p id pair))
                    agent-shell-queue--wait-timers))
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (buf-name (car pair)))
    (agent-shell-queue--complete-item item buf-name)))

(defun agent-shell-queue--default-executor (item args &optional target-buf-name)
  "Default dispatch for ITEM using TARGET-BUF-NAME.
Send ARGS to the item's target shell buffer.
Fires an alert with the truncated ARGS text, then calls `agent-shell-insert'
to submit the text.  Background items are prefixed with
resolved background prefix.  Records the buffer position after
insertion so response capture can find the reply."
  (let* ((pair (agent-shell-queue--item-by-id (agent-shell-queue-item-id item)))
         (bucket-name (car pair))
         (buf-name (or (and target-buf-name
                            (not (agent-shell-queue--dir-bucket-p target-buf-name))
                            target-buf-name)
                       (if (agent-shell-queue--dir-bucket-p bucket-name)
                           (let ((dir (or (agent-shell-queue-item-directory item)
                                          (agent-shell-queue--dir-from-bucket bucket-name))))
                             (when-let* ((b (agent-shell-queue--pick-shell-for-directory
                                             dir (agent-shell-queue-item-id item))))
                               (buffer-name b)))
                         bucket-name)))
         (buf (and buf-name (get-buffer buf-name))))
    (agent-shell-queue--alert (truncate-string-to-width args 80 nil nil "...")
           :title (format "Queue → %s" (or buf-name bucket-name))
           :category 'agent-shell-queue
           :severity 'low)
    (when (and buf (buffer-live-p buf))
      (agent-shell-insert
       :text (if (agent-shell-queue-item-background item)
                 (concat (agent-shell-queue--get-background-prefix buf) args)
               args)
       :submit t :no-focus t :shell-buffer buf)
      ;; Record after insert so start-pos is past the submitted prompt.
      (push (cons (agent-shell-queue-item-id item) (with-current-buffer buf (point-max)))
            agent-shell-queue--response-start-positions))))

(agent-shell-queue-register-executor
 (symbol-name 'agent-shell-queue--default-executor)
 #'agent-shell-queue--default-executor
 nil)

(defun agent-shell-queue--check-stall (id)
  "Alert if item ID is still `running' after stall timeout.
Fires once; does not cancel, resend, or otherwise touch the item — this is
purely a user-visible signal for a turn that never produced completion
feedback (see `agent-shell-queue-stall-timeout')."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (buf-name (car pair))
              (_ (eq (agent-shell-queue-item-status item) 'running)))
    (agent-shell-queue--alert (format "no completion signal after %ss (shell busy: %s)"
                    agent-shell-queue-stall-timeout
                    (when-let* ((buf (get-buffer buf-name)))
                      (with-current-buffer buf (shell-maker-busy))))
           :title (format "agent-shell-queue: %s stalled" buf-name)
           :category 'agent-shell-queue
           :severity 'high
           :persistent t)))

(defun agent-shell-queue--schedule-stall-check (id)
  "Schedule a one-shot stall check for item ID."
  (when agent-shell-queue-stall-timeout
    (run-with-timer agent-shell-queue-stall-timeout nil
                     #'agent-shell-queue--check-stall id)))

(defun agent-shell-queue-send-item (id)
  "Send item with ID to target buffer, marking it as running.
Items flagged as background are wrapped with
`agent-shell-queue-background-prefix'.  The item transitions to done when
the buffer's turn-complete event fires.  Running and done items are not
persisted across sessions.  If the item has a non-nil executor field, it
is called as (funcall executor item args) instead of normal kind dispatch."
  (when-let* ((pair (agent-shell-queue--item-by-id id)))
    (let* ((bucket-name (car pair))
           (item (cdr pair))
           (dir-queue-p (agent-shell-queue--dir-bucket-p bucket-name))
           (target-dir (or (agent-shell-queue-item-directory item)
                           (agent-shell-queue--dir-from-bucket bucket-name)))
           (buf (if dir-queue-p
                    (agent-shell-queue--pick-shell-for-directory target-dir id)
                  (get-buffer bucket-name)))
           (target-buf-name (and (buffer-live-p buf) (buffer-name buf))))
      (cond
       ((not (buffer-live-p buf))
        (message "agent-shell-queue: cannot dispatch — target shell %s is gone; use t/T to reassign"
                 bucket-name))
       ((and (null (agent-shell-queue-item-executor item))
             (agent-shell-queue--kind-needs-session-p (agent-shell-queue-item-kind item))
             (agent-shell-queue--session-mode-blocked-p buf))
        (cl-pushnew target-buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
        (message "agent-shell-queue: dispatch blocked — session %s is in mode %s"
                 target-buf-name
                 (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                 '(:session :mode-id))))
       (t
        (condition-case err
            (progn
              (setf (agent-shell-queue-item-status item) 'running)
              (setf (agent-shell-queue-item-dispatched item) (float-time))
              (when dir-queue-p
                (agent-shell-queue--ensure-subscription buf))
              (agent-shell-queue--schedule-stall-check id)
              (agent-shell-queue--save)
              (agent-shell-queue--refresh-buffer)
              (if (agent-shell-queue-item-executor item)
                  (funcall (agent-shell-queue-item-executor item)
                           item
                           (agent-shell-queue-item-args item))
                (if-let* ((type (agent-shell-queue--type-for-kind
                                 (agent-shell-queue-item-kind item))))
                    (funcall (agent-shell-queue-item-type-dispatch-fn type) item (or target-buf-name bucket-name))
                  (agent-shell-queue--default-executor item (agent-shell-queue-item-args item)))))
          (error
           (agent-shell-queue--handle-stale-item id (or target-buf-name bucket-name) err))))))))
(defun agent-shell-queue--collect-visible-response-text (sbuf start-pos)
  "Walk SBUF from START-POS to the shell-maker end-of-output boundary.
Skips invisible regions (collapsed tool calls, thinking blocks).  Multi-line
labeled blocks have their first (title) line stripped.  Returns the joined
visible prose as a string, or nil if nothing was collected.

START-POS must be a buffer position in SBUF captured after the prompt was
echoed.  If the position is still inside the field=input region, the walk
advances to the next field boundary before collecting."
  (with-current-buffer sbuf
    (save-excursion
      (let* (;; Turn boundary: shell-maker end-of-output marker.
             (end-marker (progn
                           (goto-char (point-max))
                           (text-property-search-backward 'field 'boundary t)))
             (end-pos (if (and end-marker
                               (> (prop-match-beginning end-marker) start-pos))
                          (prop-match-beginning end-marker)
                        (point-max)))
             ;; If start-pos is still inside the echoed field=input region,
             ;; skip forward to where field changes (the model response).  If
             ;; shell-maker already advanced past field=input before the position
             ;; was recorded (the common case), use start-pos directly — a
             ;; next-single-property-change call here would return the field
             ;; change at the very end of the response, making response-start
             ;; equal to end-pos and collecting nothing.
             (response-start
              (if (eq (get-text-property start-pos 'field) 'input)
                  (or (next-single-property-change start-pos 'field nil end-pos)
                      start-pos)
                start-pos))
             (pos response-start)
             (segments nil))
        (while (< pos end-pos)
          (let* ((state (get-text-property pos 'agent-shell-ui-state))
                 (block-end (or (next-single-property-change
                                 pos 'agent-shell-ui-state nil end-pos)
                                end-pos)))
            (if (text-property-any pos block-end 'invisible t)
                ;; Hidden body (collapsed tool call, thinking, etc.) — skip.
                (setq pos block-end)
              ;; Visible content — accumulate.  When a labeled block (state
              ;; non-nil, multi-line) is expanded its first line is the block
              ;; title — discard it.  Single-line spans and spans without state
              ;; are included verbatim.
              (let* ((full-seg (buffer-substring-no-properties pos block-end))
                     (seg (if (and state (string-match-p "\n" full-seg))
                              (string-join (cdr (split-string full-seg "\n")) "\n")
                            full-seg))
                     (trimmed (string-trim seg)))
                (unless (string-empty-p trimmed)
                  (push trimmed segments))
                (setq pos block-end)))))
        (when segments
          (string-join (nreverse segments) "\n\n"))))))

(defun agent-shell-queue--capture-response (id buf-name)
  "Capture visible response text for item ID from BUF-NAME."
  (let ((pos-pair (assoc id agent-shell-queue--response-start-positions)))
    (setq agent-shell-queue--response-start-positions
          (seq-remove (lambda (it) (equal (car it) id)) agent-shell-queue--response-start-positions))
    (when-let* (pos-pair
                (start-pos (cdr pos-pair))
                (sbuf (get-buffer buf-name))
                (pair (agent-shell-queue--item-by-id id)))
      (let* ((raw (agent-shell-queue--collect-visible-response-text sbuf start-pos))
             (text (when raw
                     (let ((t1 (replace-regexp-in-string
                                (regexp-quote "<shell-maker-end-of-prompt>") "" raw)))
                       (string-trim (replace-regexp-in-string
                                     "[a-zA-Z0-9_-]+>\\s-*\\'" "" t1))))))
        (if (and text (not (string-empty-p text)))
            (let* ((cleaned (agent-shell-queue--clean-args text))
                   (max-length (if agent-shell-queue-response-max-length
                                   (min agent-shell-queue-response-max-length
                                        agent-shell-queue-response-max-length-absolute)
                                 agent-shell-queue-response-max-length-absolute))
                   (truncated (> (length cleaned) max-length))
                   (stored (if truncated
                               (concat (substring cleaned 0 max-length) "\n\n…[truncated]")
                             cleaned)))
              (setf (agent-shell-queue-item-response (cdr pair)) stored)
              (message "agent-shell-queue: captured response for %s (%d chars%s)"
                       id (length cleaned)
                       (if truncated ", truncated" "")))
          (message "agent-shell-queue: no response captured for %s" id))))))

(defvar agent-shell-queue--pause-timers nil
  "Alist of (BUF-NAME . PLIST) for active pause/delay timers.")

(defvar agent-shell-queue--pre-dispatch-waited-ids nil
  "List of item IDs whose pre-dispatch delay has already completed.")

(defun agent-shell-queue--format-duration (seconds)
  "Format SECONDS as a human-readable string."
  (if (integerp seconds)
      (number-to-string seconds)
    (format "%.1f" seconds)))

(defun agent-shell-queue--cancel-pause-timer (buf-name)
  "Cancel any active pause or delay timers for BUF-NAME."
  (when-let* ((entry (assoc buf-name agent-shell-queue--pause-timers)))
    (let ((plist (cdr entry)))
      (when-let* ((t1 (plist-get plist :main-timer)))
        (cancel-timer t1))
      (when-let* ((t2 (plist-get plist :pre-end-timer)))
        (cancel-timer t2)))
    (setq agent-shell-queue--pause-timers
          (seq-remove (lambda (elt) (equal (car elt) buf-name))
                      agent-shell-queue--pause-timers))))

(defun agent-shell-queue--start-pause-delay (buf-name duration reason on-complete-fn)
  "Start a timed pause or delay of DURATION seconds for BUF-NAME with REASON.
ON-COMPLETE-FN is called when the delay expires.
Fires start alert if `agent-shell-queue-alert-on-pause-start' is non-nil.
Fires pre-end alert if `agent-shell-queue-alert-before-pause-end' is set."
  (agent-shell-queue--cancel-pause-timer buf-name)
  (if (or (null duration) (<= duration 0))
      (when on-complete-fn (funcall on-complete-fn))
    (when agent-shell-queue-alert-on-pause-start
      (agent-shell-queue--alert (format "%s started (%s s)" (or reason "Pause") (agent-shell-queue--format-duration duration))
             :title (format "Queue → %s" buf-name)
             :category 'agent-shell-queue
             :severity 'normal))
    (let* ((alert-before agent-shell-queue-alert-before-pause-end)
           (pre-end-timer
            (when (and alert-before (numberp alert-before) (> alert-before 0) (< alert-before duration))
              (let ((pre-end-delay (- duration alert-before)))
                (run-with-timer pre-end-delay nil
                                (lambda ()
                                  (agent-shell-queue--alert (format "%s ending in %s s"
                                                 (or reason "Pause")
                                                 (agent-shell-queue--format-duration alert-before))
                                         :title (format "Queue → %s" buf-name)
                                         :category 'agent-shell-queue
                                         :severity 'normal))))))
           (main-timer
            (run-with-timer duration nil
                            (lambda ()
                              (agent-shell-queue--cancel-pause-timer buf-name)
                              (when on-complete-fn
                                (funcall on-complete-fn))))))
      (push (cons buf-name (list :main-timer main-timer
                                 :pre-end-timer pre-end-timer
                                 :duration duration
                                 :start-time (float-time)
                                 :reason reason))
            agent-shell-queue--pause-timers))))
(defvar agent-shell-queue-item-done-hook nil
  "Hook run when a queue item transitions to done status.
Each function is called with two arguments: BUF-NAME and ITEM.")

(defun agent-shell-queue--mark-item-done (buf-name item outcome)
  "Record ITEM in BUF-NAME as done with OUTCOME and run the done hook."
  (setf (agent-shell-queue-item-completed item) (float-time))
  (setf (agent-shell-queue-item-status item) 'done)
  (setf (agent-shell-queue-item-outcome item) outcome)
  (agent-shell-queue--append-done-log buf-name item)
  (run-hook-with-args 'agent-shell-queue-item-done-hook buf-name item))

(defun agent-shell-queue--mark-running-done (buf-name)
  "Mark running items for BUF-NAME as done, recording completion time.
Also handles `interjecting' items: captures the interjection response,
stores it, and finalises the item.
If any item is already aborted or incomplete, pauses the session queue.
Only fires empty-queue alert when at least one item was marked done.
Returns the list of items marked done."
  (let (marked-items marked halted)
    (seq-do (lambda (item)
              (cond
               ((eq (agent-shell-queue-item-status item) 'running)
                (unless (memq (agent-shell-queue-item-kind item) '(pause compact context))
                  (agent-shell-queue--capture-response
                   (agent-shell-queue-item-id item) buf-name))
                (agent-shell-queue--mark-item-done buf-name item 'success)
                (push item marked-items)
                (setq marked t))
               ((eq (agent-shell-queue-item-status item) 'interjecting)
                (agent-shell-queue--capture-response
                 (agent-shell-queue-item-id item) buf-name)
                (let ((result (agent-shell-queue-item-response item)))
                  (setf (agent-shell-queue-item-interjection-result item) result)
                  (when (and result (string-suffix-p "…[truncated]" result))
                    (message "agent-shell-queue: interjection response for %s was truncated"
                             (agent-shell-queue-item-id item))))
                (agent-shell-queue--mark-item-done buf-name item 'success)
                (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) nil)
                ;; Clear the session pause so the next item dispatches normally.
                (agent-shell-queue--session-unpause-name buf-name)
                (push item marked-items)
                (setq marked t))
               ((memq (agent-shell-queue-item-status item) '(aborted incomplete))
                (setq halted t))))
            (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
    (if halted
        (agent-shell-queue--pause-and-save buf-name)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer))
    (when marked
      (agent-shell-queue--alert-if-empty))
    (nreverse marked-items)))
(defun agent-shell-queue--mark-running-incomplete (buf-name)
  "Mark any running items for BUF-NAME as incomplete and pause the session queue.
Called when the shell buffer exits or is killed while a task was in flight.
The queue must be manually resumed via `agent-shell-queue-session-resume'."
  (when (thread-last (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))
                     (seq-map (lambda (it)
                                (when (eq (agent-shell-queue-item-status it) 'running)
                                  (setf (agent-shell-queue-item-completed it) (float-time))
                                  (setf (agent-shell-queue-item-status it) 'incomplete)
                                  (setf (agent-shell-queue-item-outcome it) 'interrupted)
                                  t)))
                     (seq-filter #'identity))
    (cl-pushnew buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal))
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue--session-mode-blocked-p (buf)
  "Return non-nil if BUF mode is in `agent-shell-queue-blocked-session-modes'."
  (when-let* ((_ (buffer-live-p buf))
              (mode-id (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                       '(:session :mode-id))))
    (member mode-id agent-shell-queue-blocked-session-modes)))

;; Auto-send subscriptions

(defun agent-shell-queue--alert-if-empty ()
  "Send a persistent alert when no active or running items remain in any queue."
  (unless (thread-last
	    (agent-shell-queue-store-items agent-shell-queue--store)
            (seq-mapcat #'cdr)
            (seq-some (lambda (item)
                        (memq (agent-shell-queue-item-status item) '(active running)))))
    (agent-shell-queue--alert "All queued tasks complete"
           :title "Agent Queue"
           :category 'agent-shell-queue
           :severity 'normal
           :persistent t)))

(defun agent-shell-queue--next-dispatchable-item (items)
  "Return the first item in ITEMS eligible for dispatch, or nil."
  (seq-find (lambda (it)
              (and (eq (agent-shell-queue-item-status it) 'active)
                   (not (member (agent-shell-queue-item-id it)
                                (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))))
            items))

(defun agent-shell-queue--dispatch-if-ready (buf)
  "Send the next dispatchable item for BUF if all conditions are met."
  (when (and (buffer-live-p buf)
             (not (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
             (not (agent-shell-queue--halted-on-abort-p (buffer-name buf)))
             (not (assoc (buffer-name buf) agent-shell-queue--pause-timers)))
    (with-current-buffer buf
      (when-let* ((_ (not (shell-maker-busy)))
                  (buf-name (buffer-name))
                  (item (agent-shell-queue--next-dispatchable-item
                         (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))
        (let ((delay-before (agent-shell-queue-item-delay-before item)))
          (if (and delay-before (> delay-before 0)
                   (not (member (agent-shell-queue-item-id item) agent-shell-queue--pre-dispatch-waited-ids)))
              (progn
                (push (agent-shell-queue-item-id item) agent-shell-queue--pre-dispatch-waited-ids)
                (agent-shell-queue--start-pause-delay
                 buf-name delay-before "Pre-dispatch delay"
                 (lambda ()
                   (when (buffer-live-p buf)
                     (agent-shell-queue-send-item (agent-shell-queue-item-id item))))))
            (agent-shell-queue-send-item (agent-shell-queue-item-id item))))))))
(defun agent-shell-queue--send-next-for-buffer (buf)
  "Attempt to send the first active queue item for BUF.
Deferred via a zero-delay timer to let the current event complete before
submitting the next prompt.  Deferred items are skipped.
No-op when BUF's session is paused."
  (run-with-timer 0 nil #'agent-shell-queue--dispatch-if-ready buf))

;; Registry dispatch functions

(defun agent-shell-queue--dispatch-to-session (item buf-name)
  "Dispatch ITEM to `agent-shell' session BUF-NAME via the default executor."
  (agent-shell-queue--default-executor item (agent-shell-queue-item-args item) buf-name))

(defun agent-shell-queue--dispatch-emacs-lisp (item buf-name)
  "Dispatch an emacs-lisp ITEM for BUF-NAME by evaluating its args as a Lisp form."
  (condition-case err
      (eval (read (agent-shell-queue-item-args item)) t)
    (error (message "agent-shell-queue: emacs-lisp %s error: %s"
                    (agent-shell-queue-item-id item) err)))
  (agent-shell-queue--complete-item item buf-name))

(defun agent-shell-queue--dispatch-emacs-command (item buf-name)
  "Dispatch an emacs-command ITEM for BUF-NAME by invoking it interactively."
  (condition-case err
      (call-interactively (intern (agent-shell-queue-item-args item)))
    (error (message "agent-shell-queue: emacs-command %s error: %s"
                    (agent-shell-queue-item-id item) err)))
  (agent-shell-queue--complete-item item buf-name))

(defun agent-shell-queue--dispatch-pause-compact (item buf-name)
  "Dispatch pause or compact ITEM for BUF-NAME: pause queue or alert."
  (cl-pushnew (cons buf-name (agent-shell-queue-item-id item))
              agent-shell-queue--compact-running :test #'equal)
  (let ((duration (or (agent-shell-queue-item-delay-after item)
                      (agent-shell-queue-item-delay-before item))))
    (if (and duration (> duration 0))
        (agent-shell-queue--start-pause-delay
         buf-name duration "Pause item"
         (lambda ()
           (agent-shell-queue-mark-done (agent-shell-queue-item-id item))))
      (agent-shell-queue--pause-and-save buf-name)
      (agent-shell-queue--alert (if (eq (agent-shell-queue-item-kind item) 'pause)
                                    (format "Queue for %s paused — human action required" buf-name)
                                  (format "Manual work required: %s" (agent-shell-queue-item-args item)))
                                :title (format "Queue → %s" buf-name)
                                :category 'agent-shell-queue
                                :severity 'high
                                :persistent t))))

(defun agent-shell-queue--dispatch-wait (item _buf-name)
  "Dispatch a wait ITEM: arm a timer to fire at the target time."
  (let* ((id (agent-shell-queue-item-id item))
         (target (date-to-time (agent-shell-queue-item-args item)))
         (delay (max 0 (float-time (time-subtract target (current-time)))))
         (wait-timer (run-with-timer delay nil #'agent-shell-queue--wait-timer-fire id)))
    (push (cons id wait-timer) agent-shell-queue--wait-timers)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

(declare-function eshell-insert-and-send "esh-mode")

(defun agent-shell-queue--dispatch-shell-eshell (item buf-name)
  "Dispatch a shell-eshell ITEM for BUF-NAME by inserting and sending in eshell."
  (if-let* ((buf (get-buffer buf-name)))
      (progn
        (with-current-buffer buf
          (eshell-insert-and-send (agent-shell-queue-item-args item)))
        (agent-shell-queue--complete-item item buf-name))
    (message "agent-shell-queue: eshell buffer %s gone for item %s"
             buf-name (agent-shell-queue-item-id item))))

(declare-function eat-term-send-string "eat")

(defun agent-shell-queue--dispatch-shell-eat (item buf-name)
  "Dispatch a shell-eat ITEM for BUF-NAME via `eat-term-send-string'."
  (if-let* ((buf (get-buffer buf-name)))
      (progn
        (with-current-buffer buf
          (when (bound-and-true-p eat-terminal)
            (eat-term-send-string eat-terminal
                                  (concat (agent-shell-queue-item-args item) "\n"))))
        (agent-shell-queue--complete-item item buf-name))
    (message "agent-shell-queue: eat buffer %s gone for item %s"
             buf-name (agent-shell-queue-item-id item))))

;; Registry input helpers

(defun agent-shell-queue--enqueue-args (args kind buf)
  "Enqueue ARGS as a KIND item targeting BUF (nil = unassigned bucket)."
  (with-agent-shell-queue
    (let ((item (agent-shell-queue--make-item args nil kind)))
      (if buf
          (progn
            (setf (agent-shell-queue-item-directory item)
                  (buffer-local-value 'default-directory buf))
            (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
            (agent-shell-queue--ensure-subscription buf))
        (agent-shell-queue--add-item-to-bucket agent-shell-queue--unassigned-key item)))))

(defun agent-shell-queue--invoke-input-for-type (type buf)
  "Collect user input for TYPE and enqueue the resulting item targeting BUF."
  (let* ((kind (agent-shell-queue-item-type-kind type))
         (spec (agent-shell-queue-item-type-input-spec type))
         (input-kind (plist-get spec :kind)))
    (pcase input-kind
      ('capture
       (let ((mode (plist-get spec :mode)))
         (if (eq mode 'emacs-lisp-mode)
             (if buf
                 (agent-shell-queue--open-elisp-capture buf)
               (message "agent-shell-queue: emacs-lisp requires a target buffer"))
           (agent-shell-queue--open-capture buf nil nil kind mode))))
      ('read
       (let* ((prompt (plist-get spec :prompt))
              (fn (plist-get spec :fn))
              (result (funcall fn prompt))
              (args (if (symbolp result) (symbol-name result) result)))
         (agent-shell-queue--enqueue-args args kind buf)))
      ('none
       (agent-shell-queue--enqueue-args "" kind buf))
      ('special
       (funcall (plist-get spec :fn) buf)))))


(defun agent-shell-queue--drop-subscription (buf-name)
  "Unsubscribe from `turn-complete' events for BUF-NAME and remove from registry.
Safe to call with a dead buffer — the subscription token is merely discarded."
  (when-let* ((pair (assoc buf-name agent-shell-queue--subscriptions))
              (buf (get-buffer buf-name))
              (_ (buffer-live-p buf))
              (_ (with-current-buffer buf (derived-mode-p 'agent-shell-mode))))
    (ignore-errors
      (with-current-buffer buf
        (agent-shell-unsubscribe :subscription (cdr pair)))))

  (setq agent-shell-queue--subscriptions
        (seq-remove (lambda (it) (equal (car it) buf-name)) agent-shell-queue--subscriptions)))

(defun agent-shell-queue--on-turn-complete (buf buf-name _event)
  "Handle a turn-complete event for BUF (named BUF-NAME)."
  (let* ((marked-items (agent-shell-queue--mark-running-done buf-name))
         (last-item (car (last marked-items)))
         (response-text (and last-item (agent-shell-queue-item-response last-item)))
         (dir-bucket (and last-item (agent-shell-queue-item-directory last-item)
                          (agent-shell-queue--bucket-for-dir (agent-shell-queue-item-directory last-item)))))
    (when (or (agent-shell-queue--halted-on-abort-p buf-name)
              (and dir-bucket (agent-shell-queue--halted-on-abort-p dir-bucket)))
      (if (agent-shell-queue--verify-recovery buf last-item response-text)
          (progn
            (agent-shell-queue--clear-halted-on-abort buf-name)
            (when dir-bucket (agent-shell-queue--clear-halted-on-abort dir-bucket))
            (message "agent-shell-queue: session %s recovered from halt-on-abort" buf-name))
        (message "agent-shell-queue: session %s turn complete but remains halted-on-abort" buf-name)))
    (let ((delay-after (if last-item
                           (or (agent-shell-queue-item-delay-after last-item)
                               agent-shell-queue-default-pause-delay
                               0)
                         (or agent-shell-queue-default-pause-delay 0))))
      (if (and delay-after (> delay-after 0))
          (agent-shell-queue--start-pause-delay
           buf-name delay-after "Task pause"
           (lambda ()
             (when (buffer-live-p buf)
               (agent-shell-queue--send-next-for-buffer buf))))
        (agent-shell-queue--send-next-for-buffer buf)))))
(defun agent-shell-queue--on-clean-up (buf-name _event)
  "Handle a clean-up event for BUF-NAME (shell buffer killed).
Running item → aborted with auto-resume task; active items → blocked.runner."
  (agent-shell-queue--ensure-loaded)
  (let ((items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))))
    (seq-do (lambda (item)
              (pcase (agent-shell-queue-item-status item)
                ('running
                 (setf (agent-shell-queue-item-completed item) (float-time))
                 (setf (agent-shell-queue-item-status item) 'aborted)
                 (setf (agent-shell-queue-item-outcome item) 'interrupted)
                 (agent-shell-queue--insert-resume-task buf-name item))
                ('active
                 (setf (agent-shell-queue-item-status item) 'blocked.runner))))
            items))
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)
  (setq agent-shell-queue--subscriptions
        (seq-remove (lambda (it) (equal (car it) buf-name))
                    agent-shell-queue--subscriptions)))

(defun agent-shell-queue--ensure-subscription (buf)
  "Subscribe to `turn-complete' events on BUF if no subscription exists yet.
Also subscribes to `clean-up' so the registry is updated when BUF is killed."
  (when-let* ((buf-name (buffer-name buf))
              (_ (not (assoc buf-name agent-shell-queue--subscriptions))))
    (push (cons buf-name
                (agent-shell-subscribe-to
                 :shell-buffer buf
                 :event 'turn-complete
                 :on-event (lambda (event)
                             (agent-shell-queue--on-turn-complete buf buf-name event))))
          agent-shell-queue--subscriptions)
    (agent-shell-subscribe-to
     :shell-buffer buf
     :event 'clean-up
     :on-event (lambda (event)
                 (agent-shell-queue--on-clean-up buf-name event)))))

(defun agent-shell-queue--auto-send ()
  "Backup scan: send first active item for each idle buffer or directory bucket.
Runs infrequently; deferred items are always skipped.
Primary draining is handled by per-buffer `turn-complete' subscriptions.
Session-paused and halted-on-abort buckets are skipped."
  (when (and agent-shell-queue--loaded (agent-shell-queue-store-items agent-shell-queue--store))
    (agent-shell-queue--migrate-all-stale-items)
    (seq-do (lambda (it)
              (when-let* ((bucket-name (car it))
                          (_ (not (member bucket-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
                          (_ (not (agent-shell-queue--halted-on-abort-p bucket-name)))
                          (item (agent-shell-queue--next-dispatchable-item (cdr it))))
                (cond
                 ((agent-shell-queue--dir-bucket-p bucket-name)
                  (agent-shell-queue-send-item (agent-shell-queue-item-id item)))
                 (t
                  (when-let* ((buf (get-buffer bucket-name))
                              (_ (buffer-live-p buf))
                              (_ (not (with-current-buffer buf (shell-maker-busy)))))
                    (agent-shell-queue-send-item (agent-shell-queue-item-id item)))))))
            (copy-sequence (agent-shell-queue-store-items agent-shell-queue--store)))))

(defun agent-shell-queue--idle-flush ()
  "Save queue state to disk on idle.  No-op when the queue has not been loaded."
  (when agent-shell-queue--loaded
    (agent-shell-queue--save)))

(defun agent-shell-queue--setup-hooks ()
  "Start backup idle-scan timer and optional idle-flush timer.
Per-buffer draining is registered lazily via
`agent-shell-queue--ensure-subscription' when items are first added."
  (setq agent-shell-queue--idle-timer
        (or agent-shell-queue--idle-timer
            (run-with-idle-timer agent-shell-queue-idle-delay t #'agent-shell-queue--auto-send)))
  (when (and agent-shell-queue-idle-flush-delay
             (not agent-shell-queue--idle-flush-timer))
    (setq agent-shell-queue--idle-flush-timer
          (run-with-idle-timer agent-shell-queue-idle-flush-delay t
                               #'agent-shell-queue--idle-flush))))


(defun agent-shell-queue--status-string (item &optional buf-name next-p)
  "Return a status string for ITEM in BUF-NAME.
NEXT-P, when non-nil, marks the item as the next to be dispatched."
  (car (agent-shell-queue--item-display item buf-name next-p)))

(defun agent-shell-queue--item-kind-string (item)
  "Return the Kind column display string for ITEM."
  (pcase (agent-shell-queue-item-kind item)
    ('prompt "agent-shell-prompt")
    ((or 'emacs 'emacs-lisp) "emacs-lisp")
    ('emacs-command "emacs-command")
    ('context "context")
    ('wait "wait")
    ('pause "pause")
    ('compact "compact")
    (other (symbol-name other))))

(defun agent-shell-queue--item-display (item buf-name &optional _next-p)
  "Return (STATUS-STRING . FACE) for ITEM in BUF-NAME.
NEXT-P, when non-nil, marks the item as the next to be dispatched."
  (let* ((status (agent-shell-queue-item-status item))
         (kind (agent-shell-queue-item-kind item))
         (bg (agent-shell-queue-item-background item))
         (editing (member (agent-shell-queue-item-id item) (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))
         (blocked (and buf-name (member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
         (halted (and buf-name (or (agent-shell-queue--halted-on-abort-p buf-name)
                                   (and (agent-shell-queue-item-directory item)
                                        (agent-shell-queue--halted-on-abort-p
                                         (agent-shell-queue--bucket-for-dir (agent-shell-queue-item-directory item)))))))
         (unassigned (equal buf-name agent-shell-queue--unassigned-key))
         (detached (and buf-name
                        (not unassigned)
                        (not (agent-shell-queue--dir-bucket-p buf-name))
                        (not (buffer-live-p (get-buffer buf-name)))))
         (done (eq status 'done))
         (running (eq status 'running))
         (aborted (eq status 'aborted))
         (status-str
          (cond ((eq status 'invalid) "invalid")
                ((eq status 'pending-fork) "pending-fork")
                ((eq status 'incomplete) "incomplete")
                (done "done")
                (aborted "aborted")
                (running (if (memq kind '(pause compact))
                             "running.blocked"
                           (if bg "running.active.bg" "running.active")))
                (editing "editing")
                ((agent-shell-queue--blocked-status-p status)
                 (if bg (concat (symbol-name status) ".bg") (symbol-name status)))
                ;; Legacy: old deferred items not yet migrated show as blocked.skip
                ((eq status 'deferred) (if bg "blocked.skip.bg" "blocked.skip"))
                ((and (eq status 'active) halted) "halted.abort")
                ((and (eq status 'active) blocked) "blocked.runner")
                ((and detached (eq status 'active)) "detached")
                ((eq status 'draft) "draft")
                (bg "scheduled.bg")
                (t "scheduled")))
         (face
          (cond ((eq status 'invalid) 'font-lock-warning-face)
                ((eq status 'pending-fork) 'agent-shell-queue-pending-fork-face)
                ((eq status 'incomplete) 'font-lock-warning-face)
                (done 'shadow)
                (aborted 'font-lock-warning-face)
                (running (if (memq kind '(pause compact))
                             'agent-shell-queue-blocked-face
                           'italic))
                ((eq status 'draft) 'agent-shell-queue-draft-face)
                ((or (agent-shell-queue--blocked-status-p status)
                     (eq status 'deferred)) 'agent-shell-queue-blocked-face)
                (detached 'agent-shell-queue-detached-face)
                (unassigned 'agent-shell-queue-unassigned-face)
                ((eq kind 'compact) 'agent-shell-queue-compact-face)
                ((memq kind '(emacs emacs-lisp emacs-command)) 'font-lock-function-name-face)
                ((eq kind 'wait) 'font-lock-string-face)
                ((or blocked halted (memq kind '(pause context))) 'agent-shell-queue-blocked-face)
                (t nil))))
    (cons status-str face)))

(defun agent-shell-queue--refresh-buffer ()
  "Refresh the *agent-shell-queue* buffer if it is visible."
  (when-let* ((buf (get-buffer "*agent-shell-queue*"))
              (_ (buffer-live-p buf)))
    (with-current-buffer buf
      (when (derived-mode-p 'agent-shell-queue-mode)
	(agent-shell-queue-buffer-refresh)))))

;; Scope / narrowing

(defvar-local agent-shell-queue--display-scope nil
  "Current display scope for the queue buffer.

Narrowing semantics:
- nil (global): all buckets and items are visible; no scope indicator
  in the tab-line.
- \\='(buffer . BUF-NAME): only items for that one shell buffer are
  visible; the tab-line shows \"Buffer: BUF-NAME\".
- \\='(directory . DIR): only items for shell buffers whose
  `default-directory' is under DIR are visible; tab-line shows
  \"Scope: DIR\".

The Buffer column in the tabulated list is controlled independently by
`agent-shell-queue-show-buffer-column' (toggle with db in the queue menu).
Narrowing and the Buffer column are orthogonal: narrowing filters which rows
appear; the Buffer column controls whether a per-row buffer-name cell
is shown.

Use `agent-shell-queue-set-scope' (N) to narrow and
`agent-shell-queue-scope-global' (W) to widen back to global.")

(defun agent-shell-queue--send-now (id)
  "Dispatch item ID with running-queue awareness.
When no item is currently running for the same buffer, dispatches ID
immediately (normal path).  When a running item exists for that buffer:
  running       → append an active copy to the end of the queue for replay
  active        → signal user-error (already scheduled)
  blocked.*     → unblock so it runs after the current item finishes
  done/aborted  → re-enqueue as a new copy via `agent-shell-queue-reenqueue'"
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair))
              (item (cdr pair))
              (status (agent-shell-queue-item-status item)))
    (if (not (agent-shell-queue--has-running-item-p buf-name))
        (progn
          (agent-shell-queue--assert-not-running item)
          (agent-shell-queue-send-item id))
      (pcase status
        ('running
         (agent-shell-queue--copy-item-to-end buf-name item)
         (message "agent-shell-queue: copy enqueued for replay after current run"))
        ('active
         (user-error "Item is already scheduled; another task is running for %s" buf-name))
        ((pred agent-shell-queue--blocked-status-p)
         (agent-shell-queue-unblock id)
         (agent-shell-queue--refresh-buffer)
         (message "agent-shell-queue: item unblocked for dispatch after current run"))
        ((or 'done 'aborted)
         (when (y-or-n-p "Re-enqueue this completed item? ")
           (agent-shell-queue-reenqueue id)))
        (_
         (user-error "Cannot dispatch %s item while %s queue is running" status buf-name))))))

(defun agent-shell-queue-untrack-running (id)
  "Remove the running item ID from queue tracking without interrupting it.
The underlying shell process continues; only queue bookkeeping is dropped.
Unlike `agent-shell-queue-buffer-abort', no interrupt signal is sent and the
session queue is not paused."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (or (eq (agent-shell-queue-item-status item) 'running)
                     (user-error "Item %s is not running" id))))
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-enqueue-running-copy (id)
  "Append an active copy of the running item ID to the end of its queue.
The current run continues unaffected; the copy will dispatch when it finishes."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair))
              (item (cdr pair))
              (_ (or (eq (agent-shell-queue-item-status item) 'running)
                     (user-error "Item %s is not running" id))))
    (agent-shell-queue--copy-item-to-end buf-name item)))

(defun agent-shell-queue-reenqueue (id)
  "Create a new active queue item from the done item with ID.
The new item's `reenqueued-from' field is set to ID; ID's `reenqueued-as'
list is updated with the new item's ID.  The original item's `response'
field is not modified.  When the original target buffer is dead, prompts
for a live replacement."
  (when-let* ((pair (or (agent-shell-queue--item-by-id id)
                        (user-error "No queue item with id %s" id)))
              (old-item (cdr pair))
              (buf (or (get-buffer (car pair))
                       (or (agent-shell-queue--pick-buffer
                            (format "Buffer '%s' is gone. Re-enqueue to: " (car pair)))
                           (user-error "No live agent-shell buffers available")))))
    (unless (memq (agent-shell-queue-item-status old-item) '(done aborted))
      (user-error "Item %s is not done or aborted; cannot re-enqueue" id))
    (let* ((new-item (agent-shell-queue--make-item
                      (agent-shell-queue-item-args old-item)
                      (agent-shell-queue-item-background old-item)
                      (agent-shell-queue-item-kind old-item)))
           (new-id (agent-shell-queue-item-id new-item)))
      (setf (agent-shell-queue-item-reenqueued-from new-item) id)
      (setf (agent-shell-queue-item-reenqueued-as old-item)
            (append (agent-shell-queue-item-reenqueued-as old-item) (list new-id)))
      (setf (agent-shell-queue-item-directory new-item)
            (agent-shell-queue-item-directory old-item))
      (agent-shell-queue--add-item-to-bucket (buffer-name buf) new-item)
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      new-id)))

(defun agent-shell-queue--serialize-single-item (item target format)
  "Serialize ITEM from TARGET bucket to a string in FORMAT."
  (pcase format
    ('plist
     (with-temp-buffer
       (pp (list :buffer target :item (agent-shell-queue-item-to-plist item))
           (current-buffer))
       (buffer-string)))
    ('json
     (with-temp-buffer
       (insert (json-serialize (list :buffer target
                                     :item (agent-shell-queue--item-to-json item))))
       (when (fboundp 'json-pretty-print-buffer)
         (json-pretty-print-buffer))
       (buffer-string)))
    ('yaml
     (unless (fboundp 'yaml-encode)
       (user-error "Yaml-encode not available; install the `yaml' package"))
     (yaml-encode
      (map-into (list (cons "buffer" target)
                      (cons "item" (agent-shell-queue--item-to-yaml item)))
                '(hash-table :test equal))))
    (_ (user-error "Unknown inspect format: %S" format))))

;; Running guard

(defun agent-shell-queue--assert-not-running (item)
  "Signal `user-error' if ITEM status is `running'.
Running items may only be interrupted via the abort command."
  (when (eq (agent-shell-queue-item-status item) 'running)
    (user-error "Cannot modify a running item; abort it first")))

(defun agent-shell-queue--parse-yaml-item (item-h snapshot)
  "Validate ITEM-H against SNAPSHOT; return (item . errors) or (nil . errors)."
  (let* ((id (map-elt item-h "id"))
         (prompt (or (map-elt item-h "args") (map-elt item-h "prompt")))
         (status-str (map-elt item-h "status" "active"))
         (kind-str (map-elt item-h "kind" "prompt"))
         (bg (map-elt item-h "background"))
         (created (map-elt item-h "created"))
         (dispatched (map-elt item-h "dispatched"))
         (completed (map-elt item-h "completed"))
         (status (condition-case nil (intern status-str) (error nil)))
         (kind (condition-case nil (intern kind-str) (error nil)))
         (orig (and id snapshot (map-elt snapshot id)))
         (errors nil))
    (when (or (null prompt)
              (and (stringp prompt) (string-empty-p (string-trim prompt))))
      (push (format "item '%s': missing or empty prompt" (or id "new")) errors))
    (unless (or (memq status '(active draft invalid))
                (agent-shell-queue--blocked-status-p status))
      (push (format "item '%s': invalid status '%s'" (or id "?") status-str) errors))
    (unless (memq kind '(prompt pause context emacs wait compact))
      (push (format "item '%s': invalid kind '%s'" (or id "?") kind-str) errors))
    (when orig
      (when (and created
                 (not (equal (float created)
                             (float (agent-shell-queue-item-created orig)))))
        (push (format "item '%s': 'created' is immutable" id) errors))
      (when-let* ((od (agent-shell-queue-item-dispatched orig))
                  (_ (and dispatched (not (equal (float dispatched) (float od))))))
        (push (format "item '%s': 'dispatched' is immutable" id) errors))
      (when-let* ((oc (agent-shell-queue-item-completed orig))
                  (_ (and completed (not (equal (float completed) (float oc))))))
        (push (format "item '%s': 'completed' is immutable" id) errors))
      (when (eq (agent-shell-queue-item-status orig) 'done)
        (push (format "item '%s': completed item status cannot change" id) errors)))
    (if errors
        (cons nil errors)
      (let* ((final-id (or id (agent-shell-queue--gen-id)))
             (final-created (or created
                                (and orig (agent-shell-queue-item-created orig))
                                (float-time)))
             (final-dispatched (or dispatched (and orig (agent-shell-queue-item-dispatched orig))))
             (final-completed (or completed (and orig (agent-shell-queue-item-completed orig)))))
        (cons (agent-shell-queue-item--make
               :id final-id
               :args (string-trim prompt)
               :status (or status 'active)
               :kind (or kind 'prompt)
               :background (eq t bg)
               :created final-created
               :dispatched final-dispatched
               :completed final-completed
               :directory (or (map-elt item-h "directory")
                              (and orig (agent-shell-queue-item-directory orig))))
              nil)))))

(defun agent-shell-queue--yaml-buckets (parsed)
  "Normalize PARSED (vector or list) to a list of bucket hash-tables."
  (cond
   ((vectorp parsed) (append parsed nil))
   ((listp parsed) parsed)
   (t (list parsed))))

(defvar agent-shell-queue-fork-default-mode 'new
  "Default mode for creating new sessions when forking a queue.
`new' creates a clean new session via `agent-shell-new-shell'.
`fork' uses the ACP fork session option via `agent-shell-fork'.")

(defmacro agent-shell-queue-with-paused-session (buf &rest body)
  "Execute BODY with BUF's queue session paused, then always resume it.
BUF can be a buffer object or buffer name string.
Directly manipulates the session-paused list to avoid spurious messages
during setup.  Always resumes and saves even if BODY signals an error."
  (declare (indent 1))
  (let ((bname (make-symbol "bname")))
    `(let ((,bname (if (bufferp ,buf) (buffer-name ,buf) ,buf)))
       (cl-pushnew ,bname
                   (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                   :test #'equal)
       (unwind-protect
           (progn ,@body)
         (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
               (delete ,bname
                       (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
         (agent-shell-queue--save)
         (agent-shell-queue--refresh-buffer)))))

(defun agent-shell-queue--fork-eligible-status-p (item)
  "Return non-nil if ITEM's status is eligible for fork collection."
  (let ((status (agent-shell-queue-item-status item)))
    (or (memq status '(active draft))
        (agent-shell-queue--blocked-status-p status))))

(defun agent-shell-queue--fork-collect-items (buf-name from-id)
  "Return active items from BUF-NAME's queue at or after FROM-ID.
If FROM-ID is nil, returns all active/blocked/draft items.
Returns a list of items; does not modify the queue."
  (let* ((items (cdr (assoc buf-name
                            (agent-shell-queue-store-items agent-shell-queue--store)))))
    (if (null from-id)
        (seq-filter #'agent-shell-queue--fork-eligible-status-p items)
      ;; Search for from-id in the full list (it may be running/done, not just eligible)
      ;; then filter eligible items from that position onward.
      (when-let* ((pos (cl-position from-id items
                                    :key #'agent-shell-queue-item-id
                                    :test #'equal)))
        (seq-filter #'agent-shell-queue--fork-eligible-status-p
                    (nthcdr pos items))))))

(defun agent-shell-queue--fork-create-worktree (source-buf worktree-branch worktree-path)
  "Create a git worktree for a fork operation.
SOURCE-BUF provides the repo root (via `default-directory').
WORKTREE-BRANCH is the new branch name (auto-generated if nil).
WORKTREE-PATH is the worktree directory (auto-generated if nil).
Returns the worktree path string on success, nil on failure."
  (let* ((source-dir (if (buffer-live-p source-buf)
                         (buffer-local-value 'default-directory source-buf)
                       default-directory))
         (repo-root (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --show-toplevel 2>/dev/null"
                              (shell-quote-argument source-dir)))))
         (branch (or worktree-branch
                     (format "queue-fork-%s"
                             (format-time-string "%Y%m%d-%H%M%S"))))
         (wt-path (or worktree-path
                      (expand-file-name branch (temporary-file-directory)))))
    (cond
     ((string-empty-p repo-root)
      (message "agent-shell-queue: not in a git repo, cannot create worktree")
      nil)
     ((file-exists-p wt-path)
      (message "agent-shell-queue: worktree path already exists: %s" wt-path)
      nil)
     (t
      (let ((exit-code (call-process "git" nil nil nil
                                     "-C" repo-root
                                     "worktree" "add" "-b" branch wt-path "HEAD")))
        (if (= exit-code 0)
            wt-path
          (message "agent-shell-queue: git worktree add failed (exit %d)" exit-code)
          nil))))))

(defun agent-shell-queue--fork-create-session (source-buf fork-mode target-dir)
  "Create a new `agent-shell' session and return the new buffer.
SOURCE-BUF is the session being forked (used for directory and fork mode).
FORK-MODE is `new' (agent-shell-new-shell) or `fork' (agent-shell-fork).
TARGET-DIR, when non-nil, overrides the working directory.
Returns the newly created buffer on success, nil if none detected."
  (let* ((before-bufs (agent-shell-buffers))
         (dir (or target-dir
                  (and (buffer-live-p source-buf)
                       (buffer-local-value 'default-directory source-buf))
                  default-directory)))
    (pcase fork-mode
      ('fork
       (if (buffer-live-p source-buf)
           (with-current-buffer source-buf
             (let ((default-directory dir))
               (call-interactively #'agent-shell-fork)))
         (user-error "agent-shell-queue: `fork' mode requires a live source buffer")))
      (_
       (let ((default-directory dir))
         (call-interactively #'agent-shell-new-shell))))
    (sit-for 0.1)
    (let ((after-bufs (agent-shell-buffers)))
      (seq-find (lambda (it) (not (memq it before-bufs))) after-bufs))))

(defun agent-shell-queue--fork-elisp-form (buf-name opts)
  "Return an Emacs Lisp form string for a fork-queue Emacs item.
BUF-NAME is the source session; OPTS is the fork options plist.
The form calls `agent-shell-queue--fork-session-from-running-emacs' at
dispatch time to dynamically determine which items to fork."
  (format "(agent-shell-queue--fork-session-from-running-emacs %S %S)"
          buf-name opts))

(defun agent-shell-queue--fork-session-from-running-emacs (buf-name opts)
  "Fork items after the currently-running Emacs item in BUF-NAME's queue.
Called at dispatch time by an emacs-kind fork item to determine from-id
dynamically — handles reorderings that happened after the item was inserted.
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (let* ((items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (running-idx (cl-position-if
                       (lambda (it) (eq (agent-shell-queue-item-status it) 'running))
                       items))
         (from-id (when running-idx
                    (let ((next (nth (1+ running-idx) items)))
                      (and next (agent-shell-queue-item-id next))))))
    (when-let* ((source-buf (get-buffer buf-name)))
      (agent-shell-queue-fork-session source-buf from-id opts))))

;;;###autoload
(defun agent-shell-queue-fork-session (source-buf &optional from-id opts)
  "Fork the queue for SOURCE-BUF starting at FROM-ID into a new session.

Items at or after FROM-ID (by queue position among active/deferred/draft)
are moved to the new session.  When FROM-ID is nil, all eligible items
are moved.  The original session is paused during session creation.

OPTS is a plist with these keys:
  :fork-mode       Symbol `new' (default) or `fork'.
  :use-worktree    Non-nil — create a git worktree for the new session.
  :worktree-path   String — explicit worktree path (auto-generated when nil).
  :worktree-branch String — new branch name for the worktree.
  :capture-pending Non-nil — mark items at/after FROM-ID as `pending-fork'
                   in the original session instead of moving them, then
                   leave the session paused so new items can be inserted."
  (interactive
   (list (agent-shell-queue--pick-buffer "Fork queue for session: ")))
  (agent-shell-queue--ensure-loaded)
  (let* ((source-name (buffer-name source-buf))
         (fork-mode (or (plist-get opts :fork-mode)
                        agent-shell-queue-fork-default-mode))
         (use-worktree (plist-get opts :use-worktree))
         (worktree-path (plist-get opts :worktree-path))
         (worktree-branch (plist-get opts :worktree-branch))
         (capture-pending (plist-get opts :capture-pending))
         (items-to-fork (agent-shell-queue--fork-collect-items source-name from-id))
         ;; Track whether we should resume source after the fork.
         ;; capture-pending intentionally leaves the source paused.
         (should-resume t))
    (unless items-to-fork
      (user-error "Agent-shell-queue: no eligible items to fork in %s" source-name))
    ;; Pause source session while we create the new one.
    (cl-pushnew source-name
                (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                :test #'equal)
    (unwind-protect
        (let* ((target-dir (when use-worktree
                             (agent-shell-queue--fork-create-worktree
                              source-buf worktree-branch worktree-path)))
               (_ (when (and use-worktree (null target-dir))
                    (user-error "Agent-shell-queue: worktree creation failed")))
               (new-buf (agent-shell-queue--fork-create-session
                         source-buf fork-mode target-dir)))
          (unless new-buf
            (user-error "Agent-shell-queue: could not detect new session after creation"))
          (let ((new-name (buffer-name new-buf))
                (fork-ids (seq-map #'agent-shell-queue-item-id items-to-fork)))
            (if capture-pending
                ;; Mark affected items as pending-fork; leave them in source.
                ;; Keep session paused so the user can insert tasks before them.
                (progn
                  (seq-do (lambda (it) (setf (agent-shell-queue-item-status it) 'pending-fork))
                        items-to-fork)
                  (setq should-resume nil)
                  (agent-shell-queue--save)
                  (agent-shell-queue--refresh-buffer)
                  (message "agent-shell-queue: %d item(s) marked pending-fork in %s; new session %s created"
                           (length fork-ids) source-name new-name))
              ;; Normal mode: move items to the new session.
              (seq-do (lambda (it) (agent-shell-queue--assign-item it new-name))
                      fork-ids)
              (agent-shell-queue--ensure-subscription new-buf)
              (agent-shell-queue--save)
              (agent-shell-queue--refresh-buffer)
              (message "agent-shell-queue: forked %d item(s) from %s → %s"
                       (length fork-ids) source-name new-name))
            new-buf))
      ;; Always clean up pause state unless capture-pending requested it stays.
      (when should-resume
        (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
              (delete source-name
                      (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)))))

;;;###autoload
(defun agent-shell-queue-release-pending-fork (&optional buf)
  "Release all pending-fork items in BUF back to active status and resume dispatch.
BUF defaults to the current `agent-shell' session when called from one."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Release pending-fork items in: "))))
  (agent-shell-queue--ensure-loaded)
  (when buf
    (let* ((buf-name (buffer-name buf))
           (released 0))
      (seq-do (lambda (it)
                (when (eq (agent-shell-queue-item-status it) 'pending-fork)
                  (setf (agent-shell-queue-item-status it) 'active)
                  (cl-incf released)))
              (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (when (> released 0)
        (agent-shell-queue-session-resume buf))
      (message "agent-shell-queue: released %d pending-fork item(s) in %s" released buf-name))))

(defun agent-shell-queue--fork-insert-at (buf-name item idx)
  "Insert ITEM into BUF-NAME's queue at position IDX (0-based).
IDX nil or out-of-range appends to the end."
  (if-let* ((cell (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
      (let* ((items (cdr cell))
             (len (length items))
             (pos (if (and idx (>= idx 0) (< idx len)) idx len)))
        (setcdr cell (append (cl-subseq items 0 pos)
                             (list item)
                             (cl-subseq items pos))))
    (agent-shell-queue--add-item-to-bucket buf-name item)))

;;;###autoload
(defun agent-shell-queue-insert-fork-before (buf &optional item-id opts)
  "Insert a fork task into BUF's queue immediately before ITEM-ID.
When ITEM-ID is nil, appends to the end of the queue.
When the fork task is dispatched (as an Emacs item), it forks the queue
starting at the item that follows the fork task in the queue at dispatch time.
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-queue-mode)
                  (when-let* ((id (tabulated-list-get-id))
                              (pair (agent-shell-queue--item-by-id id)))
                    (get-buffer (car pair))))
             (agent-shell-queue--pick-buffer "Insert fork-before in: "))
         (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id))
         nil))
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (form (agent-shell-queue--fork-elisp-form buf-name opts))
         (fork-item (agent-shell-queue--make-item form nil 'emacs))
         (items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (idx (when item-id
                (cl-position item-id items
                             :key #'agent-shell-queue-item-id :test #'equal))))
    (agent-shell-queue--fork-insert-at buf-name fork-item idx)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: fork task inserted before %s in %s"
             (or item-id "end") buf-name)
    fork-item))

;;;###autoload
(defun agent-shell-queue-insert-fork-after (buf &optional item-id opts)
  "Insert a fork task into BUF's queue immediately after ITEM-ID.
When ITEM-ID is nil, appends to the end of the queue.
When the fork task is dispatched, it forks the queue starting at the next
item after the fork task (determined dynamically at dispatch time).
OPTS is the fork options plist (see `agent-shell-queue-fork-session')."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-queue-mode)
                  (when-let* ((id (tabulated-list-get-id))
                              (pair (agent-shell-queue--item-by-id id)))
                    (get-buffer (car pair))))
             (agent-shell-queue--pick-buffer "Insert fork-after in: "))
         (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id))
         nil))
  (agent-shell-queue--ensure-loaded)
  (let* ((buf-name (buffer-name buf))
         (form (agent-shell-queue--fork-elisp-form buf-name opts))
         (fork-item (agent-shell-queue--make-item form nil 'emacs))
         (items (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
         (idx (when item-id
                (when-let* ((pos (cl-position item-id items
                                              :key #'agent-shell-queue-item-id
                                              :test #'equal)))
                  (1+ pos)))))
    (agent-shell-queue--fork-insert-at buf-name fork-item idx)
    (agent-shell-queue--ensure-subscription buf)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: fork task inserted after %s in %s"
             (or item-id "end") buf-name)
    fork-item))

;;;###autoload
(defun agent-shell-queue-insert-pause (&optional buf position duration)
  "Insert a pause item into BUF's queue, optionally at 1-based POSITION.
If DURATION is specified (seconds), pause auto-resumes after DURATION.
When called interactively, prompts for target buffer (and duration with
prefix arg)."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Insert pause for: "))
         nil
         (when current-prefix-arg
           (read-number "Pause duration in seconds: "))))
  (when-let* ((_ buf)
              (item (progn
                      (agent-shell-queue--ensure-loaded)
                      (agent-shell-queue-item--make
                       :id (agent-shell-queue--gen-id)
                       :args (if duration
                                 (format "[PAUSE — %s s]" (agent-shell-queue--format-duration duration))
                               "[PAUSE — waiting for human]")
                       :status 'active
                       :kind 'pause
                       :delay-after duration
                       :created (float-time))))
              (id (agent-shell-queue-item-id item))
              (buf-name (buffer-name buf)))
    (agent-shell-queue--add-item-to-bucket buf-name item)
    (when (and position (> position 0))
      (dotimes (_ (max 0 (- (length (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))
                            position)))
        (agent-shell-queue--move id -1)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "Pause%s inserted into %s queue"
             (if duration (format " (%s s)" (agent-shell-queue--format-duration duration)) "")
             buf-name)))

(defun agent-shell-queue-set-item-delay-before (id delay)
  "Set pre-dispatch DELAY (in seconds) for queue item ID."
  (interactive
   (let* ((item (agent-shell-queue-find-item "Set delay-before for item: "))
          (id (agent-shell-queue-item-id item))
          (cur (or (agent-shell-queue-item-delay-before item) 0))
          (val (read-number (format "Delay before dispatch (seconds, current %s): " cur) cur)))
     (list id (if (<= val 0) nil val))))
  (when-let* ((item (cdr (agent-shell-queue--item-by-id id))))
    (setf (agent-shell-queue-item-delay-before item) delay)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-set-item-delay-after (id delay)
  "Set post-completion DELAY (in seconds) for queue item ID."
  (interactive
   (let* ((item (agent-shell-queue-find-item "Set delay-after for item: "))
          (id (agent-shell-queue-item-id item))
          (cur (or (agent-shell-queue-item-delay-after item) 0))
          (val (read-number (format "Delay after complete (seconds, current %s): " cur) cur)))
     (list id (if (<= val 0) nil val))))
  (when-let* ((item (cdr (agent-shell-queue--item-by-id id))))
    (setf (agent-shell-queue-item-delay-after item) delay)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)))

;;;###autoload
(defun agent-shell-queue-insert-clear-context (prompt &optional buf)
  "Insert a context-drop item with PROMPT into BUF's queue.
When called interactively, prompts for target buffer and context text."
  (interactive
   (let* ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                   (agent-shell-queue--pick-buffer "Context drop for: ")))
          (prompt (read-string "Context: ")))
     (list prompt buf)))
  (when (and prompt buf (not (string-empty-p prompt)))
    (agent-shell-queue--ensure-loaded)
    (let ((buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name (agent-shell-queue--make-item prompt nil 'context))
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Context drop inserted into %s queue" buf-name))))

;;;###autoload
(defun agent-shell-queue-insert-wait (buf)
  "Insert a wait-until item into BUF's queue.
Prompts for a target date/time; uses `org-read-date' when available,
otherwise reads a string parseable by `date-to-time'
\(e.g. \"2026-05-16 14:30\").  When dispatched the item blocks the queue until
the target time is reached, then marks itself done and advances to the
next item automatically."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Wait in queue for: "))))
  (when buf
    (agent-shell-queue--ensure-loaded)
    (let* ((target (if (fboundp 'org-read-date)
                       (org-read-date t t nil "Wait until: ")
                     (date-to-time
                      (read-from-minibuffer "Wait until (YYYY-MM-DD HH:MM): "))))
           (display (format-time-string "%Y-%m-%d %H:%M:%S" target))
           (item (agent-shell-queue--make-item display nil 'wait))
           (buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name item)
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Wait until %s inserted into %s queue" display buf-name))))

(defun agent-shell-queue-insert-compact (prompt &optional buf)
  "Insert a compact (non-LLM manual) item with PROMPT into BUF's queue.
When dispatched the item pauses the queue and alerts; use
`agent-shell-queue-mark-done' to complete it and advance the queue."
  (interactive
   (let ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                  (agent-shell-queue--pick-buffer "Compact item for: "))))
     (list (read-string "Manual task: ") buf)))
  (when (and prompt buf (not (string-empty-p prompt)))
    (agent-shell-queue--ensure-loaded)
    (let ((buf-name (buffer-name buf)))
      (agent-shell-queue--add-item-to-bucket buf-name (agent-shell-queue--make-item prompt nil 'compact))
      (agent-shell-queue--ensure-subscription buf)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "Compact item inserted into %s queue" buf-name))))

(defun agent-shell-queue-mark-done (id)
  "Mark item ID as done without dispatching it through the LLM.
If the item is a compact item that paused a session, the session is resumed
and the queue advances to the next item."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (buf-name (car pair)))

    (when (eq (agent-shell-queue-item-status item) 'done)
      (user-error "Item %s is already done" id))

    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--mark-item-done buf-name item 'manual)

    (when (member (cons buf-name id) agent-shell-queue--compact-running)
      (setq agent-shell-queue--compact-running
            (seq-remove (lambda (it) (equal it (cons buf-name id))) agent-shell-queue--compact-running))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
            (seq-remove (lambda (it) (equal it buf-name))
                        (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))

    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)

    (when-let* ((buf (get-buffer buf-name)))
      (agent-shell-queue--send-next-for-buffer buf))))

;;; Initialization and Registration




;; Initialize on load

(agent-shell-queue--setup-hooks)

;; Built-in item type registrations

(agent-shell-queue-register-item-type
 :kind 'prompt
 :label "agent-shell-prompt"
 :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
 :dispatch-fn #'agent-shell-queue--dispatch-to-session
 :input-spec '(:kind capture))

(agent-shell-queue-register-item-type
 :kind 'compact
 :label "compact"
 :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
 :dispatch-fn #'agent-shell-queue--dispatch-pause-compact
 :input-spec '(:kind capture))

(agent-shell-queue-register-item-type
 :kind 'context
 :label "context"
 :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
 :dispatch-fn #'agent-shell-queue--dispatch-to-session
 :input-spec '(:kind none))

(agent-shell-queue-register-item-type
 :kind 'emacs-lisp
 :label "emacs-lisp"
 :buffer-pred nil
 :dispatch-fn #'agent-shell-queue--dispatch-emacs-lisp
 :input-spec '(:kind capture :mode emacs-lisp-mode))

(agent-shell-queue-register-item-type
 :kind 'emacs-command
 :label "emacs-command"
 :buffer-pred nil
 :dispatch-fn #'agent-shell-queue--dispatch-emacs-command
 :input-spec '(:kind read :prompt "Emacs command: " :fn read-command))

(agent-shell-queue-register-item-type
 :kind 'pause
 :label "pause"
 :buffer-pred nil
 :dispatch-fn #'agent-shell-queue--dispatch-pause-compact
 :input-spec '(:kind none))

(agent-shell-queue-register-item-type
 :kind 'wait
 :label "wait"
 :buffer-pred nil
 :dispatch-fn #'agent-shell-queue--dispatch-wait
 :input-spec '(:kind special :fn agent-shell-queue-insert-wait))

(agent-shell-queue-register-item-type
 :kind 'shell-eshell
 :label "shell-eshell"
 :buffer-pred #'agent-shell-queue--eshell-buffer-p
 :dispatch-fn #'agent-shell-queue--dispatch-shell-eshell
 :input-spec '(:kind capture :mode sh-mode))

(agent-shell-queue-register-item-type
 :kind 'shell-eat
 :label "shell-eat"
 :buffer-pred #'agent-shell-queue--eat-buffer-p
 :dispatch-fn #'agent-shell-queue--dispatch-shell-eat
 :input-spec '(:kind capture :mode sh-mode))

(provide 'agent-shell-queue-core)

;;; agent-shell-queue-core.el ends here
