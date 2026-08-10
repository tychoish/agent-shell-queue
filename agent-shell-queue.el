;;; agent-shell-queue.el --- Persistent prompt queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/dot-emacs
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell "0.1") (alert "1.2"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implements a persistent prompt queue for agent-shell sessions, supporting
;; multi-session dispatch with pause, resume, and archive lifecycle management.
;; Queue state is serialized to plist, JSON, or YAML for session persistence
;; across Emacs restarts.  Interactive capture, edit, and item-view buffers
;; allow queue manipulation without leaving Emacs.  Fork operations split a
;; queue across multiple sessions for parallel workloads.

;;; Code:

(require 'cl-lib)
(require 'agent-shell)
(require 'annotated-completing-read)
(require 'transient)
(require 'alert)

(require 'agent-shell-queue-persistence)

(defface agent-shell-queue-blocked-face
  '((t :foreground "darkorange3"))
  "Face for queue items that are paused, deferred, or blocked.")

(defface agent-shell-queue-unassigned-face
  '((t :foreground "cornflowerblue"))
  "Face for queue items not yet assigned to any shell.")

(defface agent-shell-queue-detached-face
  '((t :foreground "orange" :slant italic))
  "Face for queue items whose target shell buffer no longer exists.")

(defface agent-shell-queue-compact-face
  '((t :foreground "steelblue3"))
  "Face for compact (non-LLM manual) work items.")

(defface agent-shell-queue-draft-face
  '((t :foreground "gray50" :slant italic))
  "Face for queue items saved as drafts (not yet queued for dispatch).")

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
  "Extract directory path from BUCKET-NAME string, or nil if not a directory bucket."
  (when (agent-shell-queue--dir-bucket-p bucket-name)
    (substring bucket-name (length agent-shell-queue--dir-prefix))))

(defun agent-shell-queue--bucket-for-dir (dir)
  "Return the directory queue bucket string for DIR."
  (concat agent-shell-queue--dir-prefix (agent-shell-queue--canonicalize-dir dir)))

(declare-function shell-maker-busy "shell-maker")
(declare-function markdown-mode "markdown-mode")
(declare-function yaml-encode "yaml")
(declare-function yaml-parse-string "yaml")
(declare-function yaml-mode "yaml-mode")
(declare-function json-pretty-print-buffer "json")
(declare-function agent-shell-menu--session-shell-buffer "agent-shell-menu")

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

;;; Configuration

(defcustom agent-shell-queue-default-pause-delay 0
  "Default pause duration in seconds after one task completes before the next task runs.
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
(defvar agent-shell-queue-serialization-format 'plist
  "Format used to persist queue state to disk.
One of:
  `plist' — s-expression with keyword-keyed plists (default; no extra deps)
  `json'  — JSON via built-in `json-serialize'/`json-parse-string' (Emacs 27+)
  `yaml'  — YAML via `yaml-encode'/`yaml-parse-string' from the `yaml' package")

(defvar agent-shell-queue-idle-delay 60.0
  "Idle delay in seconds for the backup auto-send timer.
Primary draining happens via `shell-maker-finish-output' advice; this timer
is only a safety net for buffers that become idle outside that path.")
(defvar agent-shell-queue-background-prefix '((omp . "/background ") (t . "/background "))
  "Alists of symbols mapping `<agent-shell-identifier>` to background command prefix string, or a function/value.")

(defvar agent-shell-queue-clear-command '((omp . "/fresh") (t . "/clear"))
  "Alists of symbols mapping `<agent-shell-identifier>` to clear command string, or a function/value.")

(defun agent-shell-queue--get-background-prefix (buf)
  "Resolve the background prefix for BUF based on its agent-shell configuration."
  (let* ((buffer (get-buffer buf))
         (config (and buffer (fboundp 'agent-shell-get-config) (agent-shell-get-config buffer)))
         (ident (and config (map-elt config :identifier)))
         (val agent-shell-queue-background-prefix))
    (cond
     ((functionp val) (funcall val ident))
     ((listp val) (or (cdr (assq ident val)) (cdr (assq t val)) "/background "))
     (t val))))

(defun agent-shell-queue--get-clear-command (buf)
  "Resolve the clear command for BUF based on its agent-shell configuration."
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
  "Function called with a PROMPT string to pick an agent-shell buffer.")

(defvar agent-shell-queue-archive-enabled nil
  "When non-nil, completed items can be archived and `agent-shell-queue-buffer-archive'
is active.  The destination path is controlled separately by
`agent-shell-queue-archive-file-function'.  Set to t to enable archiving.")

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
  "Alist of (ITEM-ID . BUFFER-POSITION) recording where in the shell buffer
each dispatched LLM prompt begins.  Used to capture the response text on
turn-complete and store it in the item's response field.")

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
  "Directory for versioned queue backups written when `agent-shell-queue-safe-save' is non-nil.
Nil means use a subdirectory of `temporary-file-directory' named
\"emacs-<instance>\" where <instance> comes from `agent-shell-queue-instance-name'.")

(defvar agent-shell-queue-safe-save-format nil
  "Serialization format for safe-save backups, or nil to use `agent-shell-queue-serialization-format'.")

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

;;; macros

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
  "Define a cl-defstruct TYPE-NAME with FIELD-SPECS and generate plist serializers.
Each spec in FIELD-SPECS is either a plain symbol or (SYMBOL &rest OPTIONS).
Supported options:
  :no-serialize t      — skip this field in to-plist and from-plist
  :alias KEYWORD       — also try KEYWORD when reading from plist (backward compat)
  :to-plist FUNC       — call (FUNC raw-value) when writing this field to a plist
  :from-plist FUNC     — call (FUNC plist-value) when reading this field from a plist
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

;;; Registries

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
		   (t (user-error "impossible type for %s" name)))))
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

(defcustom agent-shell-queue-strict-buffer-assignment nil
  "When non-nil, signal `user-error' when no compatible live buffer exists.
When nil (default), fall through to nil/unassigned assignment instead."
  :type 'boolean
  :group 'agent-shell-queue)

(cl-defun agent-shell-queue-register-item-type (&key kind label buffer-pred dispatch-fn input-spec)
  "Register an item type in `agent-shell-queue--item-types'.
KIND is a symbol; re-registering an existing KIND replaces the entry.
See `agent-shell-queue-item-type' for field documentation."
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
  "Return non-nil when KIND requires agent-shell session-mode compatibility."
  (when-let* ((type (agent-shell-queue--type-for-kind kind)))
    (eq (agent-shell-queue-item-type-buffer-pred type)
        #'agent-shell-queue--agent-shell-buffer-p)))

;;; Data model

(agent-shell-queue--defstruct agent-shell-queue-item
  id
  (args :alias :prompt)
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
  (delay-before :alias :delay-before-dispatch)
  (delay-after :alias :delay-after-complete))

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
3. Not in plan mode (buf mode-id not in `agent-shell-queue-blocked-session-modes`).
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
  "Add NAME to the session-paused list and mark its active items blocked.runner.
Shared by `agent-shell-queue-session-pause' (single buffer), `agent-shell-queue-pause'
\(batch, every known buffer), and `agent-shell-queue--on-interrupt'."
  (agent-shell-queue--cancel-pause-timer name)
  (cl-pushnew name (agent-shell-queue-queue-session-paused agent-shell-queue--queue) :test #'equal)
  (seq-do (lambda (item)
            (when (eq (agent-shell-queue-item-status item) 'active)
              (setf (agent-shell-queue-item-status item) 'blocked.runner)))
          (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store)))))

 (defun agent-shell-queue--session-unpause-name (name)
   "Remove NAME from session-paused and halted-sessions lists, marking its items active.
 Shared by `agent-shell-queue-unpause-all-sessions' and `agent-shell-queue-session-resume'."
   (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
         (seq-remove (lambda (n) (equal n name))
                     (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
   (agent-shell-queue--clear-halted-on-abort name)
   (seq-do (lambda (item)
             (when (eq (agent-shell-queue-item-status item) 'blocked.runner)
               (setf (agent-shell-queue-item-status item) 'active)))
           (cdr (assoc name (agent-shell-queue-store-items agent-shell-queue--store)))))
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
  "Pause dispatch for BUF (default: current agent-shell session)."
  (interactive
   (list (agent-shell-queue--pick-shell-with-state "Pause dispatch for: ")))
  (when-let* ((name (buffer-name buf)))
    (with-agent-shell-queue
      (agent-shell-queue--session-pause-name name)
      (message "agent-shell-queue: %s PAUSED" name))))

(defun agent-shell-queue-session-resume (&optional buf)
  "Resume dispatch for BUF (default: current agent-shell session).
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
  "Poll BUF until `shell-maker-busy' is nil, then call `agent-shell-queue-session-resume'.
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
  "Interrupt a stuck shell and auto-resume queue dispatch when it becomes idle.
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

(defun agent-shell-queue--on-interrupt (&optional _force)
  "Flag the session and its bucket as halted-on-abort when `agent-shell-interrupt' is called.
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

(defvar-local agent-shell-queue-intercept-mode nil
  "When non-nil in an agent-shell buffer, capture user-typed turns as queue items.")

(defvar-local agent-shell-queue-input-mode 'default
  "Current input routing mode for this agent-shell buffer.
One of `default' (normal shell input), `queue-intercept' (capture user
input as queue items while still submitting), or `queue-only' (no prompt;
all input routed through the queue).  Set via `agent-shell-queue-set-input-mode'.")

(defface agent-shell-queue-intercept-face
  '((t :foreground "orange" :weight bold))
  "Face for the [intercept] indicator appended to the prompt in queue-intercept mode.")

(defvar-local agent-shell-queue--intercept-overlay nil)
(defvar-local agent-shell-queue--intercept-sub-prompt nil)

(defun agent-shell-queue--on-submit-intercept (&rest _)
  "Capture user-typed shell turn as a queue item when intercept mode is active.
Installed as :before advice on `shell-maker-submit'."
  (when-let* ((_ agent-shell-queue-intercept-mode)
              (_ (called-interactively-p 'interactive))
              (_ (derived-mode-p 'agent-shell-mode))
              (buf-name (buffer-name (current-buffer)))
              (input (save-excursion
                       (goto-char (point-max))
                       (when (re-search-backward comint-prompt-regexp nil t)
                         (string-trim
                          (buffer-substring-no-properties (match-end 0) (point-max))))))
              (_ (not (string-empty-p input)))
              (_ (or (agent-shell-queue--ensure-loaded) t))
              (item (agent-shell-queue--make-item input nil 'prompt)))
  (setf (agent-shell-queue-item-status item) 'running)
  (setf (agent-shell-queue-item-dispatched item) (float-time))
  (agent-shell-queue--add-item-to-bucket buf-name item)
  (agent-shell-queue--ensure-subscription (current-buffer))
  (push (cons (agent-shell-queue-item-id item) (point-max))
        agent-shell-queue--response-start-positions)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)))

(advice-add 'shell-maker-submit :before #'agent-shell-queue--on-submit-intercept)

(declare-function shell-maker-busy "shell-maker")

(defun agent-shell-queue--intercept-clear ()
  (when (overlayp agent-shell-queue--intercept-overlay)
    (delete-overlay agent-shell-queue--intercept-overlay)
    (setq agent-shell-queue--intercept-overlay nil)))

(defun agent-shell-queue--intercept-show (_event)
  (when (and (eq agent-shell-queue-input-mode 'queue-intercept)
             (derived-mode-p 'agent-shell-mode))
    (agent-shell-queue--intercept-clear)
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'after-string
                   (propertize " [intercept]" 'face 'agent-shell-queue-intercept-face))
      (overlay-put ov 'agent-shell-queue-intercept t)
      (setq agent-shell-queue--intercept-overlay ov))))

(defun agent-shell-queue-set-input-mode (mode &optional buf)
  "Set input MODE for BUF (default: current buffer).
MODE must be one of `default', `queue-intercept', or `queue-only'.
Enforces mutual exclusivity and updates the prompt indicator."
  (unless (memq mode '(default queue-intercept queue-only))
    (user-error "Invalid input mode %s: expected default, queue-intercept, or queue-only"
                mode))
  (with-current-buffer (or buf (current-buffer))
    (setq agent-shell-queue-input-mode mode)
    (setq agent-shell-queue-intercept-mode (eq mode 'queue-intercept))
    (cond
     ((eq mode 'queue-only)
      (agent-shell-queue--intercept-clear)
      (when agent-shell-queue--intercept-sub-prompt
        (agent-shell-unsubscribe :subscription agent-shell-queue--intercept-sub-prompt)
        (setq agent-shell-queue--intercept-sub-prompt nil))
      (unless agent-shell-queue-only-mode
        (agent-shell-queue-only-mode 1)))
     ((eq mode 'queue-intercept)
      (when agent-shell-queue-only-mode
        (agent-shell-queue-only-mode -1))
      (unless agent-shell-queue--intercept-sub-prompt
        (setq agent-shell-queue--intercept-sub-prompt
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'prompt-ready
               :on-event #'agent-shell-queue--intercept-show)))
      (unless (shell-maker-busy)
        (agent-shell-queue--intercept-show nil)))
     (t
      (when agent-shell-queue-only-mode
        (agent-shell-queue-only-mode -1))
      (agent-shell-queue--intercept-clear)
      (when agent-shell-queue--intercept-sub-prompt
        (agent-shell-unsubscribe :subscription agent-shell-queue--intercept-sub-prompt)
        (setq agent-shell-queue--intercept-sub-prompt nil))))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-toggle-input-mode ()
  "Cycle input mode in the current buffer: default → queue-intercept → queue-only → default."
  (interactive)
  (agent-shell-queue-set-input-mode
   (pcase agent-shell-queue-input-mode
     ('default 'queue-intercept)
     ('queue-intercept 'queue-only)
     (_ 'default))))

(defun agent-shell-queue-toggle-intercept-mode (&optional buf)
  "Toggle queue-intercept mode for BUF; when active, user-typed turns are queued."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Toggle intercept for: "))))
  (when buf
    (with-current-buffer buf
      (agent-shell-queue-set-input-mode
       (if (eq agent-shell-queue-input-mode 'queue-intercept) 'default 'queue-intercept))
      (message "agent-shell-queue: input mode %s in %s"
               agent-shell-queue-input-mode (buffer-name buf)))))

(defun agent-shell-queue-enable-intercept-mode (&optional buf)
  "Enable queue-intercept mode in BUF so user-typed turns are captured as queue items."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Enable intercept for: "))))
  (when buf
    (with-current-buffer buf
      (agent-shell-queue-set-input-mode 'queue-intercept)
      (message "agent-shell-queue: queue-intercept ENABLED in %s" (buffer-name buf)))))

(defun agent-shell-queue-disable-intercept-mode (&optional buf)
  "Disable queue-intercept mode in BUF, returning it to default input mode."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Disable intercept for: "))))
  (when buf
    (with-current-buffer buf
      (when (eq agent-shell-queue-input-mode 'queue-intercept)
        (agent-shell-queue-set-input-mode 'default))
      (message "agent-shell-queue: queue-intercept disabled in %s" (buffer-name buf)))))

;;;###autoload
(defun agent-shell-queue-disable-intercept-mode-all ()
  "Reset all live agent-shell buffers in queue-intercept mode to default."
  (interactive)
  (let ((cleared (seq-filter (lambda (buf)
                               (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                   'queue-intercept))
                             (agent-shell-buffers))))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode 'default)))
            cleared)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: queue-intercept disabled in %d buffer(s)" (length cleared))))

(defun agent-shell-queue--default-pick-buffer (prompt)
  "Pick a live agent-shell buffer using PROMPT via `completing-read'."
  (when-let* ((bufs (agent-shell-buffers)))
    (get-buffer (completing-read prompt (seq-map #'buffer-name bufs) nil t))))

(defun agent-shell-queue--pick-shell-with-state (prompt)
  "Select a live agent-shell buffer via PROMPT using queue-state annotations."
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
  "Return a new active queue item for PROMPT, optional BACKGROUND, KIND, and delays."
  (agent-shell-queue-item--make
   :id (agent-shell-queue--gen-id)
   :args (agent-shell-queue--clean-args prompt)
   :status 'active
   :kind (or kind 'prompt)
   :background background
   :created (float-time)
   :delay-before delay-before
   :delay-after delay-after))

;;; Local utilities

(defun agent-shell-queue--state-file ()
  "Return the path to the on-disk queue state file."
  (funcall agent-shell-queue-state-file-function))

(defun agent-shell-queue--current-store ()
  "Return the live store, ensuring format and file reflect current config."
  (setf (agent-shell-queue-store-format agent-shell-queue--store) agent-shell-queue-serialization-format)
  (setf (agent-shell-queue-store-file agent-shell-queue--store)(agent-shell-queue--state-file))
  agent-shell-queue--store)

;;; Buffer predicates

(defun agent-shell-queue--agent-shell-buffer-p (buf)
  "Return non-nil when BUF is a live agent-shell session buffer."
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
  "Pick a live agent-shell buffer using PROMPT."
  (funcall agent-shell-queue-pick-buffer-function prompt))

(defun agent-shell-queue--candidate-buffers-for-kind (kind)
  "Return live buffers compatible with KIND, or nil when kind accepts any.
When the kind has no buffer-pred (any-buffer), returns all live buffers."
  (when-let* ((type (agent-shell-queue--type-for-kind kind))
              (pred (agent-shell-queue-item-type-buffer-pred type)))
    (seq-filter (lambda (b) (and (buffer-live-p b) (funcall pred b)))
                (buffer-list))))

(defun agent-shell-queue--annotation (text max-width)
  "Return TEXT truncated to MAX-WIDTH with an ellipsis, for use as a completion annotation."
  (truncate-string-to-width (or text "") max-width nil nil "…"))

(defun agent-shell-queue--pick-buffer-for-kind (kind &optional prompt)
  "Pick a buffer compatible with KIND via `annotated-completing-read', offering nil/unassigned.
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
  "Format DELTA time-value as a short relative age string.
Alternative using stdlib: (car (split-string (format-seconds \"%dd %hh %mm %ss%z\" s)))
but that yields \"0d\" for a zero delta instead of \"0s\"."
  (let ((s (float-time delta)))
    (cond ((< s 60) (format "%ds" (truncate s)))
          ((< s 3600) (format "%dm" (truncate (/ s 60))))
          ((< s 86400) (format "%dh" (truncate (/ s 3600))))
          (t (format "%dd" (truncate (/ s 86400)))))))

;;; Persistence — implementation in agent-shell-queue-persistence.el

;;;###autoload
(defun agent-shell-queue-buffer-archive ()
  "Archive the item at point to the archive file and remove it from the queue.
Archiving must be enabled via `agent-shell-queue-archive-enabled'.
The destination path is provided by `agent-shell-queue-archive-file-function'."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--write-archive (car pair) item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)
    (message "agent-shell-queue: archived %s" id)))

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

;;; Store predicates

(defun agent-shell-queue--item-id-matches-p (id item)
  "Return non-nil when ITEM's id equals ID."
  (equal (agent-shell-queue-item-id item) id))

(defun agent-shell-queue--bucket-empty-p (pair)
  "Return non-nil when PAIR is a bucket cell whose item list is empty."
  (null (cdr pair)))

(defun agent-shell-queue--wait-timer-id-matches-p (id pair)
  "Return non-nil when PAIR is a wait-timer cell keyed by ID."
  (equal (car pair) id))

;;; Queue operations

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

(defun agent-shell-queue-add (prompt buf &optional background)
  "Add a new active item for PROMPT destined for BUF.  Save and refresh.
When BACKGROUND is non-nil the item is flagged for sub-agent execution.
Registers a `turn-complete' subscription on BUF if one is not already active."
  (with-agent-shell-queue
    (let ((item (agent-shell-queue--make-item prompt background)))
      (setf (agent-shell-queue-item-directory item)
            (buffer-local-value 'default-directory buf))
      (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
      (agent-shell-queue--ensure-subscription buf)
      item)))
;;;###autoload
(defun agent-shell-queue-add-directory (prompt dir &optional background)
  "Add a new active item for PROMPT destined for directory queue DIR.  Save and refresh."
  (interactive
   (list (read-string "Prompt: ")
         (read-directory-name "Directory queue: ")
         current-prefix-arg))
  (with-agent-shell-queue
    (let* ((canon-dir (agent-shell-queue--canonicalize-dir dir))
           (bucket (agent-shell-queue--bucket-for-dir canon-dir))
           (item (agent-shell-queue--make-item prompt background)))
      (setf (agent-shell-queue-item-directory item) canon-dir)
      (agent-shell-queue--add-item-to-bucket bucket item)
      item)))

;;;###autoload
(defun agent-shell-queue-enqueue-directory (dir)
  "Open a capture buffer for directory queue DIR."
  (interactive (list (read-directory-name "Directory queue: ")))
  (let ((default-directory (agent-shell-queue--canonicalize-dir dir)))
    (agent-shell-queue-enqueue nil)))

;;;###autoload
(defun agent-shell-queue-enqueue-emacs (buf)
  "Open an Emacs Lisp capture buffer to compose a form for BUF's queue.
The capture buffer is in `emacs-lisp-mode'.  Confirm with \\`C-c C-c',
cancel with \\`C-c C-k'.  When dispatched, the form is evaluated via
`eval'; errors are reported as messages and the item is marked done.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'emacs-lisp "Target (or unassigned): ")))
  (if buf
      (agent-shell-queue--open-elisp-capture buf)
    (agent-shell-queue--open-capture nil nil nil 'emacs-lisp 'emacs-lisp-mode)))

;;;###autoload
(defun agent-shell-queue-enqueue-emacs-command (command buf)
  "Enqueue an interactive COMMAND to run in Emacs for BUF's queue.
COMMAND is selected via `read-command' (completing-read over all commands).
When dispatched, the command is invoked with `call-interactively'.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive
   (list (read-command "Emacs command: ")
         (agent-shell-queue--pick-buffer-for-kind 'emacs-command "Target (or unassigned): ")))
  (agent-shell-queue--enqueue-args (symbol-name command) 'emacs-command buf))

;;;###autoload
(defun agent-shell-queue-enqueue-shell-eshell (buf)
  "Open a shell capture buffer to compose a command for eshell BUF.
The capture buffer is in `sh-mode'.  Confirm with \\`C-c C-c'.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'shell-eshell "eshell buffer (or unassigned): ")))
  (agent-shell-queue--open-capture buf nil nil 'shell-eshell 'sh-mode))

;;;###autoload
(defun agent-shell-queue-enqueue-shell-eat (buf)
  "Open a shell capture buffer to compose a command for eat BUF.
The capture buffer is in `sh-mode'.  Confirm with \\`C-c C-c'.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'shell-eat "eat buffer (or unassigned): ")))
  (agent-shell-queue--open-capture buf nil nil 'shell-eat 'sh-mode))

(defun agent-shell-queue-add-unassigned (prompt &optional background)
  "Add a new item for PROMPT to the unassigned bucket.  Save and refresh.
Unassigned items display in blue and sort after all shell-assigned items."
  (with-agent-shell-queue
    (let ((item (agent-shell-queue--make-item prompt background)))
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
             (truncate-string-to-width
              (agent-shell-queue-item-args (cdr found)) 120 nil nil "...")))
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

(defun agent-shell-queue--default-executor (item args)
  "Default dispatch for ITEM: send ARGS to the item's target shell buffer.
Fires an alert with the truncated ARGS text, then calls `agent-shell-insert'
to submit the text. Background items are prefixed with
resolved background prefix. Records the buffer position after
insertion so response capture can find the reply."
  (let* ((pair (agent-shell-queue--item-by-id (agent-shell-queue-item-id item)))
         (buf-name (car pair))
         (buf (get-buffer buf-name)))
    (agent-shell-queue--alert (truncate-string-to-width args 80 nil nil "...")
           :title (format "Queue → %s" buf-name)
           :category 'agent-shell-queue
           :severity 'low)
    (agent-shell-insert
     :text (if (agent-shell-queue-item-background item)
               (concat (agent-shell-queue--get-background-prefix buf) args)
             args)
     :submit t :no-focus t :shell-buffer buf)
    ;; Record after insert so start-pos is past the submitted "Claude> [args]" line.
    (push (cons (agent-shell-queue-item-id item) (with-current-buffer buf (point-max)))
          agent-shell-queue--response-start-positions)))

(agent-shell-queue-register-executor
 (symbol-name 'agent-shell-queue--default-executor)
 #'agent-shell-queue--default-executor
 nil)

(defun agent-shell-queue--check-stall (id)
  "Alert if the item with ID is still `running' `agent-shell-queue-stall-timeout' after dispatch.
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
  "Schedule a one-shot stall check for item ID after `agent-shell-queue-stall-timeout'."
  (when agent-shell-queue-stall-timeout
    (run-with-timer agent-shell-queue-stall-timeout nil
                     #'agent-shell-queue--check-stall id)))

(defun agent-shell-queue-send-item (id)
  "Send the item with ID to its target buffer, marking it as running.
Items flagged as background are wrapped with `agent-shell-queue-background-prefix'.
The item transitions to done when the buffer's turn-complete event fires.
Running and done items are not persisted across sessions.
If the item has a non-nil executor field, it is called as
\(funcall executor item args) instead of the normal kind dispatch."
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
  "Capture the visible response text for item ID from BUF-NAME and clean up tracking."
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
  "Mark any running items for BUF-NAME as done, recording completion time.
Also handles `interjecting' items: captures the interjection response,
stores it, and finalises the item.
If any item is already aborted or incomplete, pauses the session queue.
Only fires the empty-queue alert when at least one item was actually marked done.
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
  "Return non-nil if BUF's session mode is in `agent-shell-queue-blocked-session-modes'."
  (when-let* ((_ (buffer-live-p buf))
              (mode-id (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                       '(:session :mode-id))))
    (member mode-id agent-shell-queue-blocked-session-modes)))

;;; Auto-send — per-buffer turn-complete subscriptions (primary) + idle timer (backup)

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
  "Backup scan: send the first active item for each idle agent-shell or directory bucket.
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
  "Start the backup idle-scan timer and the optional idle-flush timer.
Per-buffer draining is registered lazily via `agent-shell-queue--ensure-subscription'
when items are first added for a given buffer."
  (setq agent-shell-queue--idle-timer
        (or agent-shell-queue--idle-timer
            (run-with-idle-timer agent-shell-queue-idle-delay t #'agent-shell-queue--auto-send)))
  (when (and agent-shell-queue-idle-flush-delay
             (not agent-shell-queue--idle-flush-timer))
    (setq agent-shell-queue--idle-flush-timer
          (run-with-idle-timer agent-shell-queue-idle-flush-delay t
                               #'agent-shell-queue--idle-flush))))

;;; Queue buffer

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
    (error "agent-shell-queue-reload: cannot locate source file"))
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
          ("el" (when (fboundp 'emacs-lisp-mode) (emacs-lisp-mode)))
          ("json" (when (fboundp 'json-mode) (json-mode)))
          ((or "yaml" "yml") (when (fboundp 'yaml-mode) (yaml-mode))))
        (read-only-mode 1)
        (display-buffer buf '(display-buffer-below-selected (window-height . 0.4))))
    (user-error "Queue state file does not exist: %s" file)))

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

;;; Scope / narrowing
(defvar-local agent-shell-queue--display-scope nil
  "Current display scope for the queue buffer.

Narrowing semantics:
- nil (global): all buckets and items are visible; no scope indicator in the tab-line.
- \\='(buffer . BUF-NAME): only items for that one shell buffer are visible; the
  tab-line shows \"Buffer: BUF-NAME\" to indicate the active filter.
- \\='(directory . DIR): only items for shell buffers whose `default-directory' is
  under DIR are visible; the tab-line shows \"Scope: DIR\".

The Buffer column in the tabulated list is controlled independently by
`agent-shell-queue-show-buffer-column' (toggle with db in the queue menu).
Narrowing and the Buffer column are orthogonal: narrowing filters which rows
appear; the Buffer column controls whether a per-row buffer-name cell is shown.

Use `agent-shell-queue-set-scope' (N) to narrow and `agent-shell-queue-scope-global'
(W) to widen back to global.")

;;; Display configuration

(defvar agent-shell-queue-show-buffer-column t
  "Show the Buffer column in the queue buffer.
Toggle interactively with `agent-shell-queue-toggle-buffer-column' (db in the menu).")

(defvar agent-shell-queue-show-ordinal-column t
  "Show the ordinal (#) column in the queue buffer.")

(defvar agent-shell-queue-show-age-column t
  "Show the Age column in the queue buffer.")

(defvar agent-shell-queue-show-kind-column t
  "Show the Kind column in the queue buffer.")

(defvar agent-shell-queue-multiline-format nil
  "Display prompt on a second line with a separator between items.
When non-nil, `<down>' and `<up>' move by item rather than by line.")

;; Persist display preferences and queue state across sessions.
(defvar savehist-additional-variables nil)
(add-to-list 'savehist-additional-variables 'agent-shell-queue--queue)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-buffer-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-ordinal-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-age-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-kind-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-multiline-format)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-default-pause-delay)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-alert-on-pause-start)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-alert-before-pause-end)

(defun agent-shell-queue--scope-label (scope)
  "Return a short human-readable string for SCOPE."
  (pcase scope
    ('nil "global")
    (`(buffer . ,name) (format "buffer:%s" name))
    (`(directory . ,dir) (abbreviate-file-name dir))))

(defun agent-shell-queue--scope-matches-p (buf-name scope)
  "Return non-nil if BUF-NAME belongs to SCOPE.
The unassigned bucket only matches the global scope."
  (pcase scope
    ('nil t)
    (`(buffer . ,name) (equal buf-name name))
    (`(directory . ,dir)
     (and (not (equal buf-name agent-shell-queue--unassigned-key))
          (or (and (agent-shell-queue--dir-bucket-p buf-name)
                   (string-prefix-p (expand-file-name dir)
                                    (expand-file-name (agent-shell-queue--dir-from-bucket buf-name))))
              (when-let* ((buf (get-buffer buf-name)))
                (string-prefix-p (expand-file-name dir)
                                 (expand-file-name
                                  (buffer-local-value 'default-directory buf)))))))))

(defun agent-shell-queue--scope-candidates ()
  "Return an alist of (LABEL . SCOPE) covering global, directories, and buffers.
Directories are derived from live shell buffers and directory queue buckets."
  (let* ((assigned (seq-remove (lambda (it)
                                 (equal (car it) agent-shell-queue--unassigned-key))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (buf-entries (seq-map (lambda (it) (cons (car it) (cons 'buffer (car it)))) assigned))
         (live-dirs (thread-last assigned
                      (seq-filter (lambda (it) (buffer-live-p (get-buffer (car it)))))
                      (seq-map (lambda (it)
                                 (expand-file-name
                                  (buffer-local-value 'default-directory (get-buffer (car it))))))))
         (bucket-dirs (thread-last assigned
                        (seq-filter (lambda (it) (agent-shell-queue--dir-bucket-p (car it))))
                        (seq-map (lambda (it)
                                   (expand-file-name (agent-shell-queue--dir-from-bucket (car it)))))))
         (dirs (seq-uniq (append live-dirs bucket-dirs))))
    (append
     (list (cons "global (all)" nil))
     (seq-map (lambda (it) (cons (abbreviate-file-name it) (cons 'directory it)))
              (sort dirs #'string<))
     buf-entries)))

(defun agent-shell-queue--active-item-count (items)
  "Return the count of ITEMS whose status is not `done'."
  (seq-count (lambda (item)
               (not (eq (agent-shell-queue-item-status item) 'done)))
             items))

;;;###autoload
(defun agent-shell-queue-set-scope ()
  "Narrow the queue buffer view: choose global, a directory, or a specific buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((candidates (agent-shell-queue--scope-candidates))
         (table (seq-map
                 (lambda (cand)
                   (let* ((scope (cdr cand))
                          (count (apply #'+
                                        (seq-map (lambda (it)
                                                   (if (agent-shell-queue--scope-matches-p (car it) scope)
                                                       (agent-shell-queue--active-item-count (cdr it))
                                                     0))
                                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
                     (cons (car cand) (format "%d item(s)" count))))
                 candidates)))
    (let* ((label (annotated-completing-read table
                                             :prompt "queue scope => "
                                             :category 'agent-shell-queue-scope
                                             :require-match t
                                             :history 'agent-shell-queue-set-scope))
           (scope (cdr (assoc label candidates))))
      (setq-local agent-shell-queue--display-scope scope)
      (agent-shell-queue-buffer-refresh)
      (force-mode-line-update))))

;;;###autoload
(defun agent-shell-queue-scope-global ()
  "Reset the queue buffer to the global scope (show all items)."
  (interactive)
  (setq-local agent-shell-queue--display-scope nil)
  (agent-shell-queue-buffer-refresh)
  (force-mode-line-update))

(defun agent-shell-queue--yaml-str (s)
  "Format string S as a quoted YAML scalar, escaping special characters."
  (concat "\""
          (replace-regexp-in-string
           "\""
           "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
          "\""))

(defun agent-shell-queue--yaml-block (s indent)
  "Format string S as a YAML literal block scalar with INDENT prefix on content lines."
  (concat "|\n"
          (mapconcat (lambda (line)
                       (if (string-empty-p line) "" (concat indent line)))
                     (split-string s "\n") "\n")))

(defun agent-shell-queue--item-to-yaml-export (item)
  "Format ITEM as a YAML mapping string for export.
Multi-line `prompt' and `response' fields are formatted as literal block scalars."
  (with-temp-buffer
    (let* ((id (agent-shell-queue-item-id item))
           (args (agent-shell-queue-item-args item))
           (response (agent-shell-queue-item-response item))
           (status (symbol-name (agent-shell-queue-item-status item)))
           (kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
           (bg (agent-shell-queue-item-background item))
           (created (agent-shell-queue-item-created item))
           (dispatched (agent-shell-queue-item-dispatched item))
           (completed (agent-shell-queue-item-completed item)))
      (insert "  - id: " id "\n")
      (insert "    prompt: ")
      (if (string-match-p "\n" args)
          (insert (agent-shell-queue--yaml-block args "      "))
        (insert (agent-shell-queue--yaml-str args) "\n"))
      (insert "    status: " status "\n")
      (insert "    kind: " kind "\n")
      (insert "    background: " (if bg "true" "false") "\n")
      (when created (insert "    created: " (number-to-string created) "\n"))
      (when dispatched (insert "    dispatched: " (number-to-string dispatched) "\n"))
      (when completed (insert "    completed: " (number-to-string completed) "\n"))
      (when response
        (insert "    response: ")
        (if (string-match-p "\n" response)
            (insert (agent-shell-queue--yaml-block response "      "))
          (insert (agent-shell-queue--yaml-str response) "\n"))))
    (buffer-string)))

;;;###autoload
(defun agent-shell-queue-export ()
  "Export items in the current scope to a read-only YAML buffer.
Multi-line prompt and response fields are formatted as YAML literal block scalars."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((scope agent-shell-queue--display-scope)
         (out-name (format "*agent-shell-queue-export: %s*"
                           (agent-shell-queue--scope-label scope)))
         (multi-p (> (apply #'+
                            (seq-map (lambda (it) (length (cdr it)))
                                     (agent-shell-queue-store-items agent-shell-queue--store)))
                     1))
         (visible (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-filter (lambda (it) (agent-shell-queue--scope-matches-p (car it) scope)))
                    (seq-map (lambda (it)
                               (let ((items (if multi-p
                                                (seq-remove
                                                 (lambda (item)
                                                   (and (eq (agent-shell-queue-item-status item) 'done)
                                                        (memq (agent-shell-queue-item-kind item)
                                                              '(pause compact context))))
                                                 (cdr it))
                                              (cdr it))))
                                 (cons (car it) items))))
                    (seq-remove (lambda (pair) (null (cdr pair))))))
         (yaml-str
          (with-temp-buffer
            (seq-do (lambda (pair)
                      (insert "- buffer: " (agent-shell-queue--yaml-str (car pair)) "\n")
                      (insert "  items:\n")
                      (seq-do (lambda (item)
                                (insert (agent-shell-queue--item-to-yaml-export item)))
                              (cdr pair)))
                    visible)
            (buffer-string))))
    (with-current-buffer (get-buffer-create out-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if visible yaml-str ""))
        (goto-char (point-min))
        (when (fboundp 'yaml-mode)
          (yaml-mode)))
      (display-buffer (current-buffer)))))

(defvar agent-shell-queue-mode-map
  (let ((m (make-sparse-keymap)))
    ;; View / send / remove
    (define-key m (kbd "RET")      #'agent-shell-queue-buffer-view-item)
    (define-key m (kbd "C-c C-s")  #'agent-shell-queue-buffer-send)
    (define-key m (kbd "C-K")      #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "C-<DEL>")  #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "C-c C-r")  #'agent-shell-queue-buffer-reenqueue)
    (define-key m (kbd "C-A")      #'agent-shell-queue-buffer-archive)
    (define-key m (kbd "z")        #'agent-shell-queue-buffer-mark-done)
    ;; Edit / enqueue
    (define-key m (kbd "e")        #'agent-shell-queue-enqueue-dispatch)
    (define-key m (kbd "C-e")      #'agent-shell-queue-edit-task)
    ;; Pause / schedule (suspend item from auto-dispatch without removing)
    (define-key m (kbd "p")        #'agent-shell-queue-buffer-pause)
    (define-key m (kbd "r")        #'agent-shell-queue-buffer-schedule)
    ;; Background flag
    (define-key m (kbd "b")        #'agent-shell-queue-buffer-enable-background-task)
    (define-key m (kbd "B")        #'agent-shell-queue-buffer-disable-background-task)
    ;; Move / assign
    (define-key m (kbd "a")        #'agent-shell-queue-buffer-assign)
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-buffer-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-buffer-move-down)
    ;; Pause / resume session queue dispatch
    (define-key m (kbd "C-c C-p")        #'agent-shell-queue-session-pause)
    (define-key m (kbd "C-c C-r")        #'agent-shell-queue-session-resume)
    (define-key m (kbd "C-c C-x")        #'agent-shell-queue-recover-stuck-shell)
    ;; Interjection
    (define-key m (kbd "i")        #'agent-shell-queue-interject)
    ;; Insert items
    (define-key m (kbd "C-d p")    #'agent-shell-queue-insert-pause)
    (define-key m (kbd "C-d C-c")  #'agent-shell-queue-insert-clear-context)
    (define-key m (kbd "C-w")      #'agent-shell-queue-insert-wait)
    (define-key m (kbd "C-d c")    #'agent-shell-queue-insert-compact)
    ;; Capture entry points
    (define-key m (kbd "c")        #'agent-shell-queue-capture)
    (define-key m (kbd "a")        #'agent-shell-queue-buffer-capture-after)
    (define-key m (kbd "u")        #'agent-shell-queue-capture-unassigned)
    (define-key m (kbd "y")        #'agent-shell-queue-capture-from-clipboard)
    ;; Navigation / display
    (define-key m (kbd "<down>")   #'agent-shell-queue-next-item)
    (define-key m (kbd "<up>")     #'agent-shell-queue-prev-item)
    (define-key m (kbd "TAB")      #'agent-shell-queue-buffer-jump-to-next)
    (define-key m (kbd "g")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "M-r")      #'agent-shell-queue-reload)
    (define-key m (kbd "D")        #'agent-shell-queue-show-disk-state)
    ;; Scope / narrowing
    (define-key m (kbd "n")        #'agent-shell-queue-set-scope)
    (define-key m (kbd "w")        #'agent-shell-queue-scope-global)
    (define-key m (kbd "SPC")      #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "C-d x")    #'agent-shell-queue-raw-edit)
    (define-key m (kbd "C-d i")    #'agent-shell-queue-import)
    (define-key m (kbd "o")        #'agent-shell-queue-buffer-open-shell)
    (define-key m (kbd "C-d a")    #'agent-shell-queue-buffer-abort)
    (define-key m (kbd "C-v")      #'agent-shell-queue-select-columns)
    (define-key m (kbd "=")        #'agent-shell-queue-buffer-inspect-item)
    (define-key m (kbd "m")        #'agent-shell-queue-dispatch)
    (define-key m (kbd "?")        #'describe-bindings)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(defvar-local agent-shell-queue--last-column-structure nil
  "Column structure key from the last `tabulated-list-init-header' call.
A list of (show-buffer-p show-ordinal-p show-age-p) used to avoid
reinitializing headers on pure content refreshes.")

(defun agent-shell-queue--on-queue-buffer-kill ()
  "Clean up in-flight items when the queue display buffer is killed.
Running items are marked aborted; active (scheduled) items are blocked.skip.
This prevents tasks from executing without any supervisory display."
  (agent-shell-queue--ensure-loaded)
  (seq-do (lambda (bucket)
            (seq-do (lambda (item)
                      (pcase (agent-shell-queue-item-status item)
                        ('running
                         (setf (agent-shell-queue-item-status item) 'aborted)
                         (setf (agent-shell-queue-item-outcome item) 'canceled))
                        ('active
                         (setf (agent-shell-queue-item-status item) 'blocked.skip))))
                    (cdr bucket)))
          (agent-shell-queue-store-items agent-shell-queue--store))
  (agent-shell-queue--save))

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the agent-shell prompt queue."
  (setq tabulated-list-format
        (agent-shell-queue--column-format t (agent-shell-queue--prompt-width t)))
  (setq agent-shell-queue--last-column-structure
        (list t agent-shell-queue-show-kind-column agent-shell-queue-show-ordinal-column agent-shell-queue-show-age-column))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tab-line-mode 1)
  (setq tab-line-format
        '(:eval (let* ((state (agent-shell-queue--activity-state))
                       (sessions (length (agent-shell-buffers)))
                       (scope agent-shell-queue--display-scope)
                       (visible-items
                        (seq-filter
                         (lambda (pair)
                           (agent-shell-queue--scope-matches-p (car pair) scope))
                         (agent-shell-queue-store-items agent-shell-queue--store)))
                       (depth (apply #'+
                                     (seq-map (lambda (it)
                                                (agent-shell-queue--active-item-count (cdr it)))
                                              visible-items)))
                       (scope-display
                        (pcase scope
                          ('nil nil)
                          (`(buffer . ,name) (format "  |  Buffer: %s" name))
                          (_ (format "  |  Scope: %s"
                                     (agent-shell-queue--scope-label scope)))))
                       (flush-display
                        (if agent-shell-queue--last-flush-time
                            (format "%s ago" (agent-shell-queue--format-age
                                             (time-since agent-shell-queue--last-flush-time)))
                          "never"))
                       (next-display
                        (when agent-shell-queue--next-flush-time
                          (let ((remaining (float-time (time-subtract
                                                        agent-shell-queue--next-flush-time
                                                        (current-time)))))
                            (when (> remaining 0)
                              (format "  |  Next sync in %s"
                                      (agent-shell-queue--format-age
                                       (seconds-to-time remaining)))))))
)
                  (format " Queue: %s  |  Sessions: %d  |  Depth: %d%s  |  Flushed: %s%s"
                          state sessions depth
                          (or scope-display "")
                          flush-display (or next-display "")))))
  (add-hook 'kill-buffer-hook #'agent-shell-queue--on-queue-buffer-kill nil t))

(defun agent-shell-queue--activity-state ()
  "Return a propertized string describing the queue's current activity level."
  (cond
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'running))))
    (propertize "running" 'face 'success))
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'active))))
    (propertize "waiting" 'face 'font-lock-comment-face))
   (t
    (propertize "idle" 'face 'shadow))))

(defun agent-shell-queue--buffer-state-label (buf-name)
  "Return a short queue-state string for BUF-NAME's bucket.
Examples: \"paused\", \"running (2)\", \"3 pending\", \"idle\"."
  (let* ((paused-p (member buf-name
                           (agent-shell-queue-queue-session-paused
                            agent-shell-queue--queue)))
         (halted-p (agent-shell-queue--halted-on-abort-p buf-name))
         (items (cdr (assoc buf-name
                            (agent-shell-queue-store-items
                             agent-shell-queue--store))))
         (running (seq-some (lambda (it)
                              (eq (agent-shell-queue-item-status it) 'running))
                            items))
         (pending (seq-count (lambda (it)
                               (eq (agent-shell-queue-item-status it) 'active))
                             items)))
    (cond
     ((and halted-p (> pending 0)) (format "halted (abort: %d)" pending))
     (halted-p "halted (abort)")
     ((and paused-p (> pending 0)) (format "paused (%d)" pending))
     (paused-p "paused")
     ((and running (> pending 0)) (format "running (%d)" pending))
     (running "running")
     ((> pending 0) (format "%d pending" pending))
     (t "idle"))))

(defconst agent-shell-queue--status-column-width
  (- (max 6 (apply #'max
                   (seq-map #'length
                            '("invalid" "pending-fork" "done" "running.blocked"
                              "aborted" "running.active.bg" "running.active"
                              "editing" "blocked.runner" "blocked.task"
                              "blocked.dep" "blocked.cond" "blocked.pending" "blocked.skip"
                              "halted.abort" "draft" "scheduled.bg" "scheduled" "incomplete"))))
     3)
  "Width of the Status column: max(6, length of longest status display string) minus 3.")

(defconst agent-shell-queue--kind-column-width
  (apply #'max (seq-map #'length
                        '("agent-shell-prompt" "emacs-lisp" "emacs-command"
                          "context" "wait" "pause" "compact" "Kind")))
  "Width of the Kind column.")

(defun agent-shell-queue--column-format (show-buffer-p pw)
  "Build the `tabulated-list-format' vector for current display settings.
SHOW-BUFFER-P controls whether the Buffer column is included.
PW is the width allocated to the Prompt column."
  (let (cols)
    (push (list "Status" agent-shell-queue--status-column-width t) cols)
    (when agent-shell-queue-show-kind-column
      (push (list "Kind" agent-shell-queue--kind-column-width t) cols))
    (when show-buffer-p
      (push (list "Buffer" 19 t) cols))
    (when agent-shell-queue-show-ordinal-column
      (push (list "#" 4 nil) cols))
    (when agent-shell-queue-show-age-column
      (push (list "Age" 5 t) cols))
    (push (list "Prompt" pw nil) cols)
    (apply #'vector (nreverse cols))))

(defun agent-shell-queue--prompt-width (show-buffer-p)
  "Compute available width for the Prompt column.
SHOW-BUFFER-P indicates whether the Buffer column is included."
  (max 20 (- (window-width)
             (+ agent-shell-queue--status-column-width
                (if agent-shell-queue-show-kind-column (1+ agent-shell-queue--kind-column-width) 0)
                (if show-buffer-p 19 0)
                (if agent-shell-queue-show-ordinal-column 4 0)
                (if agent-shell-queue-show-age-column 5 0)
                ;; tabulated-list adds one space between columns
                (+ 1
                   (if agent-shell-queue-show-kind-column 1 0)
                   (if show-buffer-p 1 0)
                   (if agent-shell-queue-show-ordinal-column 1 0)
                   (if agent-shell-queue-show-age-column 1 0))))))

(defun agent-shell-queue--ordered-display-items ()
  "Return the display-ordered bucket list for the current scope.
Filters store items to those matching `agent-shell-queue--display-scope',
then places the unassigned bucket last."
  (let* ((scope agent-shell-queue--display-scope)
         (visible (seq-filter (lambda (it)
                                (agent-shell-queue--scope-matches-p (car it) scope))
                              (agent-shell-queue-store-items agent-shell-queue--store)))
         (unassigned (assoc agent-shell-queue--unassigned-key visible))
         (assigned (seq-remove (lambda (it) (equal (car it) agent-shell-queue--unassigned-key))
                               visible)))
    (if unassigned
        (append assigned (list unassigned))
      assigned)))

(defun agent-shell-queue-buffer-refresh ()
  "Rebuild the tabulated list from current queue state."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((ordered (seq-map #'agent-shell-queue--sanitize-bucket
                           (agent-shell-queue--ordered-display-items)))
         (show-buffer-p agent-shell-queue-show-buffer-column)
         (column-structure (list show-buffer-p
                                 agent-shell-queue-show-kind-column
                                 agent-shell-queue-show-ordinal-column
                                 agent-shell-queue-show-age-column))
         (next-id-map (seq-map (lambda (it)
                                 (cons (car it)
                                       (when-let* ((next (agent-shell-queue--next-dispatchable-item
                                                          (cdr it))))
                                         (agent-shell-queue-item-id next))))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (pw (agent-shell-queue--prompt-width show-buffer-p)))
    (unless (equal column-structure agent-shell-queue--last-column-structure)
      (setq agent-shell-queue--last-column-structure column-structure)
      (setq tabulated-list-format (agent-shell-queue--column-format show-buffer-p pw))
      (tabulated-list-init-header))
    (setq tabulated-list-entries
          (thread-last ordered
            (seq-mapcat
             (lambda (pair)
               (seq-map
                (lambda (item)
                  (let* ((id (agent-shell-queue-item-id item))
                         (next-p (equal id (cdr (assoc (car pair) next-id-map))))
                         (display (agent-shell-queue--item-display item (car pair) next-p))
                         (status-str (car display))
                         (face (cdr display))
                         (cell (lambda (str) (if face (propertize str 'face face) str)))
                         (idx (cl-position id
                                           (cdr (assoc (car pair) (agent-shell-queue-store-items agent-shell-queue--store)))
                                           :key #'agent-shell-queue-item-id :test #'equal))
                         (ordinal (if idx (1+ idx) 0))
                         (status (agent-shell-queue-item-status item))
                         (dispatched (agent-shell-queue-item-dispatched item))
                         (completed (agent-shell-queue-item-completed item))
                         (age-str (cond
                                   ((and (eq status 'done) dispatched completed)
                                    (agent-shell-queue--format-age
                                     (time-subtract completed dispatched)))
                                   ((and (eq status 'running) dispatched)
                                    (agent-shell-queue--format-age (time-since dispatched)))
                                   (t "")))
                         (first-line (car (split-string
                                           (agent-shell-queue-item-args item) "\n")))
                         (buf-cell (funcall cell
                                            (if (equal (car pair) agent-shell-queue--unassigned-key)
                                                "(unassigned)" (car pair))))
                         (kind-str (agent-shell-queue--item-kind-string item))
                         (row (let (cols)
                                (push (funcall cell status-str) cols)
                                (when agent-shell-queue-show-kind-column
                                  (push (funcall cell kind-str) cols))
                                (when show-buffer-p (push buf-cell cols))
                                (when agent-shell-queue-show-ordinal-column
                                  (push (funcall cell (if (> ordinal 0)
                                                          (number-to-string ordinal) ""))
                                        cols))
                                (when agent-shell-queue-show-age-column
                                  (push (funcall cell age-str) cols))
                                (push (funcall cell (truncate-string-to-width
                                                     first-line pw nil nil "…"))
                                      cols)
                                (apply #'vector (nreverse cols)))))
                    (list id row)))
                (cdr pair))))))
    (tabulated-list-print t)
    (when agent-shell-queue-multiline-format
      (agent-shell-queue--expand-multiline))))

(defun agent-shell-queue--expand-multiline ()
  "Expand each tabulated entry with a second prompt line and a separator.
Must be called immediately after `tabulated-list-print'."
  (let* ((inhibit-read-only t)
         (sep-face 'shadow)
         (sep-char ?─)
         ;; Collect (id . line-start-pos) in reverse buffer order so that
         ;; inserting extra lines below each entry does not shift earlier positions.
         (positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id (tabulated-list-get-id)))
          (push (cons id (line-beginning-position)) positions))
        (forward-line 1)))
    ;; positions is already in reverse order due to push; process top-to-bottom
    ;; would corrupt offsets, so keep reverse (last entry first).
    (seq-do (lambda (it)
              (when-let* ((id (car it))
                          (line-start (cdr it))
                          (item (cdr (agent-shell-queue--item-by-id id)))
                          (prompt (agent-shell-queue-item-args item)))
                (let ((face (cdr (agent-shell-queue--item-display item nil nil))))
                  (save-excursion
                    (goto-char line-start)
                    (end-of-line)
                    (let ((insert-start (point))
                          (sep (propertize
                                (make-string (max 4 (1- (window-width))) sep-char)
                                'face sep-face)))
                      (insert "\n")
                      (insert (propertize (concat "  " prompt) 'face face))
                      (put-text-property insert-start (point) 'tabulated-list-id id)
                      (insert "\n" sep)
                      (put-text-property (1- (point)) (point)
                                         'agent-shell-queue-separator t))))))
            positions)))

(defun agent-shell-queue-next-item ()
  "Move point to the first line of the next queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line 1)
    (let ((current-id (tabulated-list-get-id)))
      (forward-line 1)
      (while (and (not (eobp))
                  (equal (tabulated-list-get-id) current-id))
        (forward-line 1)))))

(defun agent-shell-queue-prev-item ()
  "Move point to the first line of the previous queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line -1)
    (let ((current-id (tabulated-list-get-id))
          (target-id nil))
      ;; Step backward until a different id appears
      (forward-line -1)
      (while (and (not (bobp))
                  (or (null (tabulated-list-get-id))
                      (equal (tabulated-list-get-id) current-id)))
        (forward-line -1))
      (setq target-id (tabulated-list-get-id))
      ;; Now find the first (topmost) line of that item
      (when target-id
        (while (and (not (bobp))
                    (equal (get-text-property (line-beginning-position 0)
                                              'tabulated-list-id)
                           target-id))
          (forward-line -1))
        ;; If we overshot past the item, go forward one line
        (unless (equal (tabulated-list-get-id) target-id)
          (forward-line 1))))))

(defun agent-shell-queue-buffer-jump-to-next ()
  "Move point to the next item that will be dispatched."
  (interactive)
  (let ((next-ids (thread-last
                    (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-map (lambda (pair)
                               (agent-shell-queue--next-dispatchable-item (cdr pair))))
                    (seq-filter #'identity)
                    (seq-map #'agent-shell-queue-item-id))))
    (if (null next-ids)
        (message "No pending items in queue")
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (not (eobp)))
          (if (member (tabulated-list-get-id) next-ids)
              (setq found t)
            (forward-line 1)))
        (unless found
          (message "No pending items visible in current scope"))))))

(defun agent-shell-queue-buffer-pause ()
  "Pause the item at point — suspend it from auto-dispatch without removing it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'blocked.skip)
    (agent-shell-queue--save)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-schedule ()
  "Schedule the paused item at point — resume it for auto-dispatch.
For blocked.task items, cascades active to subsequent blocked.dep items.
Draft items are promoted directly to active without cascade."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (status (agent-shell-queue-item-status item)))
    (cond
     ((agent-shell-queue--blocked-status-p status)
      (agent-shell-queue-unblock id))
     ((eq status 'draft)
      (setf (agent-shell-queue-item-status item) 'active)
      (agent-shell-queue--save))
     (t (user-error "Item %s cannot be scheduled from status %s" id status)))
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-unblock ()
  "Unblock the item at point (blocked.task → active with cascade)."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-unblock id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-unblock ()
  "Unblock the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-unblock id)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-buffer-remove ()
  "Remove the item at point from the queue, with confirmation."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (agent-shell-queue--confirm-remove item)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)))

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

(defun agent-shell-queue-buffer-send ()
  "Send the item at point to its target buffer now.
When another task is already running for the same session, behavior adapts
based on the item's current status — see `agent-shell-queue--send-now'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue--send-now id)
    (agent-shell-queue-buffer-refresh)))

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

(defun agent-shell-queue-buffer-untrack-running ()
  "Remove the running item at point from queue tracking without aborting it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-untrack-running id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-untrack-running ()
  "Remove the displayed running item from queue tracking without aborting it."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (quit-window)
    (agent-shell-queue-untrack-running id)
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

(defun agent-shell-queue-buffer-enqueue-running-copy ()
  "Append a copy of the running item at point to the end of its queue."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-enqueue-running-copy id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-enqueue-running-copy ()
  "Append a copy of the displayed running item to the end of its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-enqueue-running-copy id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

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

(defun agent-shell-queue-buffer-reenqueue ()
  "Re-enqueue the done or aborted item at point as a new active item."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue-buffer-refresh)))

;;;###autoload
(defun agent-shell-queue-insert-pause (&optional buf position duration)
  "Insert a pause item into BUF's queue, optionally at 1-based POSITION.
If DURATION is specified (number of seconds), the pause auto-resumes after DURATION seconds.
When called interactively, prompts for target buffer (and duration with prefix arg)."
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
otherwise reads a string parseable by `date-to-time' (e.g. \"2026-05-16 14:30\").
When dispatched the item blocks the queue until the target time is reached,
then marks itself done and advances to the next item automatically."
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

(defun agent-shell-queue-buffer-mark-done ()
  "Mark the item at point as done."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-mark-done id)))

(defun agent-shell-queue-item-view-mark-done ()
  "Mark the displayed item as done."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-mark-done id)
    (agent-shell-queue-item-view-refresh)))

;;; Item view

;;; Item-view action table — single source of truth for keys, transient, and ACR menu

(defconst agent-shell-queue--item-view-action-table
  (list
   ;; Plist fields: :key :label :cmd :group :annotation :if
   ;; :group nil  — keymap only, not shown in transient or ACR
   ;; :if nil     — always shown when :group is non-nil
   (list :key "s"
         :label "Dispatch now"
         :cmd 'agent-shell-queue-item-view-send
         :group "Manage Task"
         :annotation "Send item to target shell immediately"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted draft)))))
   (list :key "X"
         :label "Abort (interrupt)"
         :cmd 'agent-shell-queue-item-view-abort
         :group "Manage Task"
         :annotation "Interrupt running item, mark as aborted"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "E"
         :label "Enqueue copy (repeat after current run)"
         :cmd 'agent-shell-queue-item-view-enqueue-running-copy
         :group "Manage Task"
         :annotation "Append an active copy to the queue without interrupting the current run"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "U"
         :label "Untrack (remove without aborting)"
         :cmd 'agent-shell-queue-item-view-untrack-running
         :group "Manage Task"
         :annotation "Drop queue tracking for this item; the shell process continues"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "R"
         :label "Re-enqueue"
         :cmd 'agent-shell-queue-item-view-reenqueue
         :group "Manage Task"
         :annotation "Create a new active copy of this completed item"
         :if (lambda () (memq (agent-shell-queue--iv-status) '(done aborted))))
   (list :key "z"
         :label "Mark done"
         :cmd 'agent-shell-queue-item-view-mark-done
         :group "Manage Task"
         :annotation "Manually mark item done without dispatching"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "e"
         :label "Edit"
         :cmd 'agent-shell-queue-item-view-edit
         :group "Manage Task"
         :annotation "Open item in edit buffer"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "d"
         :label "Pause (suspend dispatch)"
         :cmd 'agent-shell-queue-item-view-pause
         :group "Manage Task"
         :annotation "Suspend item from being dispatched"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'active)))
   (list :key "u"
         :label "Schedule (resume dispatch)"
         :cmd 'agent-shell-queue-item-view-schedule
         :group "Manage Task"
         :annotation "Return item to active dispatch queue"
         :if (lambda () (memq (agent-shell-queue--iv-status) '(draft))))
   (list :key "f"
         :label "Unblock"
         :cmd 'agent-shell-queue-item-view-unblock
         :group "Manage Task"
         :annotation "Unblock item and cascade to dependent items"
         :if (lambda () (agent-shell-queue--blocked-status-p (agent-shell-queue--iv-status))))
   (list :key "b"
         :label "Enable background task"
         :cmd 'agent-shell-queue-item-view-enable-background-task
         :group "Manage Task"
         :annotation "Prefix prompt with /background on dispatch"
         :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                             (not (agent-shell-queue--iv-bg-p)))))
   (list :key "B"
         :label "Disable background task"
         :cmd 'agent-shell-queue-item-view-disable-background-task
         :group "Manage Task"
         :annotation "Remove background task flag"
         :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                             (agent-shell-queue--iv-bg-p))))
   (list :key "o"
         :label "Open shell buffer"
         :cmd 'agent-shell-queue-item-view-open-shell
         :group "Manage Task"
         :annotation "Switch to this item's target shell buffer"
         :if nil)
   (list :key "i"
         :label "Inspect raw"
         :cmd 'agent-shell-queue-item-view-inspect
         :group "Manage Task"
         :annotation "View raw serialization of this item"
         :if nil)
   (list :key "C-d"
         :label "Destructive…"
         :cmd 'agent-shell-queue-item-destructive-menu
         :group "Manage Task"
         :annotation "Archive, remove, or other destructive operations"
         :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
   ;; Move / Assign group
   (list :key "M-<up>"
         :label "Move up"
         :cmd 'agent-shell-queue-item-view-move-up
         :group "Move / Assign"
         :annotation "Move item earlier in its bucket queue"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "M-<down>"
         :label "Move down"
         :cmd 'agent-shell-queue-item-view-move-down
         :group "Move / Assign"
         :annotation "Move item later in its bucket queue"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "t"
         :label "Assign to shell…"
         :cmd 'agent-shell-queue-item-view-assign
         :group "Move / Assign"
         :annotation "Move item to a different agent-shell buffer"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   ;; Detached reassignment — only visible when target buffer is dead
   (list :key "T"
         :label "Reassign (this item)"
         :cmd 'agent-shell-queue-item-view-reassign-detached
         :group "Move / Assign"
         :annotation "Assign this detached item to an active or new shell"
         :if #'agent-shell-queue--iv-detached-p)
   (list :key "C-t"
         :label "Reassign (all in same bucket)"
         :cmd 'agent-shell-queue-item-view-reassign-bucket-detached
         :group "Move / Assign"
         :annotation "Assign all items from the same dead shell to a shell"
         :if #'agent-shell-queue--iv-detached-p)
   (list :key "C-T"
         :label "Reassign (all detached)"
         :cmd 'agent-shell-queue-item-view-reassign-all-detached
         :group "Move / Assign"
         :annotation "Assign every detached item across all buckets to a shell"
         :if #'agent-shell-queue--iv-detached-p)
   ;; Keymap-only entries (no transient/ACR group)
   (list :key "C-K"     :label "Remove"   :cmd 'agent-shell-queue-item-view-remove  :group nil :annotation nil :if nil)
   (list :key "C-<DEL>" :label "Remove"   :cmd 'agent-shell-queue-item-view-remove  :group nil :annotation nil :if nil)
   (list :key "C-A"     :label "Archive"  :cmd 'agent-shell-queue-item-view-archive :group nil :annotation nil :if nil)
   (list :key "g"       :label "Refresh"  :cmd 'agent-shell-queue-item-view-refresh :group nil :annotation nil :if nil)
   (list :key "m"       :label "Menu"     :cmd 'agent-shell-queue-item-menu         :group nil :annotation nil :if nil)
   (list :key "a"       :label "Actions"  :cmd 'agent-shell-queue-item-view-actions :group nil :annotation nil :if nil)
   (list :key "q"       :label "Close"    :cmd 'quit-window                          :group nil :annotation nil :if nil))
  "Action table for `agent-shell-queue-item-view-mode'.
Each entry is a plist with keys:
  :key        — key-binding string for `kbd'
  :label      — human-readable label
  :cmd        — command symbol
  :group      — transient group name (nil = keymap only)
  :annotation — short annotation for ACR menu (nil = not in ACR)
  :if         — predicate function or nil (nil = always visible)")

(defconst agent-shell-queue--item-view-action-groups
  '("Manage Task" "Move / Assign")
  "Ordered group names for the item-view transient menu.")

(defun agent-shell-queue--item-view-build-map ()
  "Build `agent-shell-queue-item-view-mode-map' from the action table."
  (let ((m (make-sparse-keymap)))
    (seq-do (lambda (entry)
              (define-key m (kbd (plist-get entry :key)) (plist-get entry :cmd)))
            agent-shell-queue--item-view-action-table)
    m))

(defvar agent-shell-queue-item-view-mode-map
  (agent-shell-queue--item-view-build-map)
  "Keymap for `agent-shell-queue-item-view-mode'.")

(define-derived-mode agent-shell-queue-item-view-mode markdown-mode "Queue-Item"
  "Read-only view of a single agent-shell queue item."
  (setq buffer-read-only t)
  (font-lock-mode -1))

(defvar-local agent-shell-queue--item-view-id nil
  "ID of the queue item displayed in this item-view buffer.")

(defvar-local agent-shell-queue--item-view-queue-buf nil
  "The queue buffer that spawned this item view.")

(defun agent-shell-queue--render-item-view (id item target)
  "Render ITEM with ID and TARGET into the current buffer."
  (setq-local fill-column 80)
  (let* ((created (agent-shell-queue-item-created item))
         (dispatched (agent-shell-queue-item-dispatched item))
         (completed (agent-shell-queue-item-completed item))
         (bg (agent-shell-queue-item-background item))
         (kind (agent-shell-queue-item-kind item))
         (next-p (when-let* ((first (agent-shell-queue--next-dispatchable-item
                                    (cdr (assoc target (agent-shell-queue-store-items agent-shell-queue--store))))))
                   (equal (agent-shell-queue-item-id first) id)))
         (field (lambda (label value)
                  (insert (propertize (format "%-12s" label) 'face 'bold))
                  (insert (format " %s\n" value)))))
    (funcall field "ID:" id)
    (funcall field "Target:"
             (if (equal target agent-shell-queue--unassigned-key)
                 "(unassigned)" target))
    (when-let* ((dir (agent-shell-queue-item-directory item)))
      (funcall field "Directory:" dir))
    (funcall field "Status:" (agent-shell-queue--status-string item target next-p))
    (when-let* ((outcome (agent-shell-queue-item-outcome item)))
      (funcall field "Outcome:" (symbol-name outcome)))
    (when-let* ((from-id (agent-shell-queue-item-reenqueued-from item)))
      (funcall field "Re-enq from:" from-id))
    (when-let* ((as-ids (agent-shell-queue-item-reenqueued-as item)))
      (funcall field "Re-enq as:" (string-join as-ids ", ")))
    (funcall field "Kind:" (symbol-name (or kind 'prompt)))
    (funcall field "Background:" (if bg "yes" "no"))
    (insert "\n")
    (funcall field "Created:"
             (format "%s (%s ago)"
                     (format-time-string "%F %T" created)
                     (agent-shell-queue--format-age (time-since created))))
    (when dispatched
      (funcall field "Dispatched:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" dispatched)
                       (agent-shell-queue--format-age (time-since dispatched)))))
    (when completed
      (funcall field "Completed:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" completed)
                       (agent-shell-queue--format-age (time-since completed)))))
    (when (and dispatched completed)
      (funcall field "Latency:"
               (agent-shell-queue--format-age (time-subtract completed dispatched))))
    (insert "\n")
    (insert (propertize "Prompt:\n" 'face 'bold))
    (insert (agent-shell-queue-item-args item) "\n")
    (when-let* ((response (agent-shell-queue-item-response item)))
      (insert "\n")
      (insert (propertize "Response:\n" 'face 'bold))
      (insert response "\n"))
    (when-let* ((ipromt (agent-shell-queue-item-interjection-prompt item)))
      (insert "\n")
      (insert (propertize "Interjection prompt:\n" 'face 'bold))
      (insert (string-replace "\n" "\n  " (concat "  " ipromt)) "\n")
      (when-let* ((iresult (agent-shell-queue-item-interjection-result item)))
        (insert "\n")
        (insert (propertize "Interjection response:\n" 'face 'bold))
        (insert (string-replace "\n" "\n  " (concat "  " iresult)) "\n")))
    (insert "\n")
    (insert (propertize "[m] menu  [a] actions  [q] close" 'face 'shadow))))

(defun agent-shell-queue-buffer-view-item ()
  "Open an item-view window below showing the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair))
              (queue-buf (current-buffer))
              (view-name (format "*agent-shell-queue-item: %s*" id))
              (view-buf (get-buffer-create view-name)))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-item-view-mode)
        (setq agent-shell-queue--item-view-id id
              agent-shell-queue--item-view-queue-buf queue-buf)
        (agent-shell-queue--render-item-view id item target)))
    (display-buffer view-buf '(display-buffer-below-selected
                               (window-height . 0.35)))))

(defun agent-shell-queue-find-item-command ()
  "Interactively pick any queue item and display it in the item-view buffer."
  (interactive)
  (when-let* ((pair (agent-shell-queue-find-item "Jump to item: "))
              (item (cdr pair))
              (id (agent-shell-queue-item-id item))
              (target (car pair))
              (view-name (format "*agent-shell-queue-item: %s*" id))
              (view-buf (get-buffer-create view-name)))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-item-view-mode)
        (setq agent-shell-queue--item-view-id id)
        (agent-shell-queue--render-item-view id item target)))
    (display-buffer view-buf '(display-buffer-below-selected
                               (window-height . 0.35)))))

(defun agent-shell-queue-item-view-refresh ()
  "Refresh the content of the current item-view buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair))
              (inhibit-read-only t))
    (erase-buffer)
    (agent-shell-queue--render-item-view id item target)))

(defun agent-shell-queue-item-view-send ()
  "Send the displayed item to its target buffer now.
When another task is already running for the same session, behavior adapts
based on the item's current status — see `agent-shell-queue--send-now'."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair)))
    (when (not (agent-shell-queue--has-running-item-p buf-name))
      (quit-window))
    (agent-shell-queue--send-now id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-remove ()
  "Remove the displayed item from the queue, with confirmation."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (or (agent-shell-queue--assert-not-running item) t)))
    (when (agent-shell-queue--confirm-remove item)
      (quit-window)
      (agent-shell-queue-remove id)
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue-item-view-pause ()
  "Pause the displayed item — suspend it from auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'blocked.skip)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-schedule ()
  "Schedule the displayed item — resume it for auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'draft)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'active)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reenqueue ()
  "Re-enqueue the displayed done or aborted item as a new active item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
    (quit-window)
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-archive ()
  "Archive the displayed item and close the view.
Archiving must be enabled via `agent-shell-queue-archive-enabled'."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--write-archive (car pair) item)
    (quit-window)
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %s" id)))

;;;###autoload
(defun agent-shell-queue-archive-done-n (n)
  "Archive the N oldest done items across all queues.
Errors if archiving is not enabled."
  (interactive (list (let ((n (read-number "Archive how many done items? " 10)))
                       (if (> n 0)
                           n
                         (user-error "N must be a positive number")))))
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (let* ((all-pairs
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-mapcat (lambda (bucket)
                                     (seq-map (lambda (item) (cons (car bucket) item))
                                              (cdr bucket))))
                       (seq-filter (lambda (pair)
                                     (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
                       (seq-sort (lambda (a b)
                                   (< (agent-shell-queue-item-created (cdr a))
                                      (agent-shell-queue-item-created (cdr b)))))))
         (to-archive (seq-take all-pairs n))
         (count (length to-archive)))
    (when (= count 0)
      (user-error "No done items to archive"))
    (seq-do (lambda (pair)
              (agent-shell-queue--write-archive (car pair) (cdr pair))
              (agent-shell-queue-remove (agent-shell-queue-item-id (cdr pair))))
            to-archive)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %d done item(s)" count)))

;;;###autoload
(defun agent-shell-queue-archive-done-all ()
  "Archive all done items across all queues.
Errors if archiving is not enabled or no done items exist."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (let* ((all-pairs
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-mapcat (lambda (bucket)
                                     (seq-map (lambda (item) (cons (car bucket) item))
                                              (cdr bucket))))
                       (seq-filter (lambda (pair)
                                     (eq (agent-shell-queue-item-status (cdr pair)) 'done)))
                       (seq-sort (lambda (a b)
                                   (< (agent-shell-queue-item-created (cdr a))
                                      (agent-shell-queue-item-created (cdr b)))))))
         (count (length all-pairs)))
    (when (= count 0)
      (user-error "No done items to archive"))
    (seq-do (lambda (pair)
              (agent-shell-queue--write-archive (car pair) (cdr pair))
              (agent-shell-queue-remove (agent-shell-queue-item-id (cdr pair))))
            all-pairs)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %d done item(s)" count)))

;;;###autoload
(defun agent-shell-queue-toggle-archive ()
  "Toggle `agent-shell-queue-archive-enabled' and report the new state."
  (interactive)
  (setq agent-shell-queue-archive-enabled (not agent-shell-queue-archive-enabled))
  (message "agent-shell-queue: archiving %s"
           (if agent-shell-queue-archive-enabled "enabled" "disabled")))

;;;###autoload
(defun agent-shell-queue-load-archive (&optional file)
  "Import items from the JSONL archive file into the queue as active items.
FILE defaults to the path returned by `agent-shell-queue-archive-file-function';
when called interactively with a prefix argument, prompts for a file path."
  (interactive
   (list (if current-prefix-arg
             (read-file-name "Archive file: " nil (agent-shell-queue--archive-file) t)
           (or (agent-shell-queue--archive-file)
               (user-error "Set `agent-shell-queue-archive-enabled' or pass a file")))))
  (unless (file-exists-p file)
    (user-error "Archive file not found: %s" file))
  (let ((count 0)
        (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (seq-do (lambda (line)
              (unless (string-blank-p line)
                (condition-case err
                    (let* ((obj (json-parse-string line :object-type 'plist))
                           (item (agent-shell-queue-item--make
                                  :id (agent-shell-queue--gen-id)
                                  :args (or (plist-get obj :args) (plist-get obj :prompt))
                                  :status 'active
                                  :kind (intern (or (plist-get obj :kind) "prompt"))
                                  :background (eq t (plist-get obj :background))
                                  :created (float-time)))
                           (raw-target (plist-get obj :target))
                           (target (if (and raw-target (get-buffer raw-target))
                                       raw-target
                                     agent-shell-queue--unassigned-key)))
                      (agent-shell-queue--add-item-to-bucket target item)
                      (setq count (1+ count)))
                  (error (message "agent-shell-queue: skipping malformed archive line: %s" err)))))
            (split-string content "\n"))
    (when (= count 0)
      (user-error "No items found in archive"))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: imported %d item(s) from archive" count)))

(defun agent-shell-queue-item-view-enable-background-task ()
  "Flag the displayed item for background sub-agent execution."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-disable-background-task ()
  "Clear the background sub-agent flag from the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-up ()
  "Move the displayed item one position earlier in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-down ()
  "Move the displayed item one position later in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-assign ()
  "Assign the displayed item to a different agent-shell buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (bufs (or (agent-shell-buffers)
                        (user-error "No live agent-shell buffers")))
              (_ (or (agent-shell-queue--assert-not-running (cdr pair)) t)))
    (let* ((current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (seq-map
                   (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (cons (buffer-name buf)
                             (if (and current-dir (equal dir current-dir))
                                 (concat "(same dir) " (abbreviate-file-name dir))
                               (abbreviate-file-name (or dir ""))))))
                   bufs)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)
        (agent-shell-queue--refresh-buffer)
        (agent-shell-queue-item-view-refresh)))))

;;; Detached item reassignment

(defun agent-shell-queue--pick-shell-for-reassign (prompt)
  "Prompt for a live agent-shell buffer or offer to create a new one.
Returns a buffer name string, or nil if cancelled."
  (let* ((bufs (agent-shell-buffers))
         (live-entries (seq-map (lambda (b)
                                  (cons (buffer-name b)
                                        (abbreviate-file-name
                                         (or (buffer-local-value 'default-directory b) ""))))
                                bufs))
         (choices (cons (cons "(create new shell)" "Open a new agent-shell in a chosen directory")
                        live-entries))
         (choice (annotated-completing-read choices
                                            :prompt prompt
                                            :require-match t)))
    (if (equal choice "(create new shell)")
        (let* ((dir (read-directory-name "Shell directory: "))
               (before (agent-shell-buffers))
               (_ (let ((default-directory dir))
                    (call-interactively #'agent-shell-new-shell)))
               (_ (sit-for 0.1))
               (new-buf (seq-find (lambda (b) (not (memq b before)))
                                  (agent-shell-buffers))))
          (when new-buf (buffer-name new-buf)))
      choice)))

(defun agent-shell-queue-item-view-reassign-detached ()
  "Assign this detached item to an active or newly created shell."
  (interactive)
  (unless (agent-shell-queue--iv-detached-p)
    (user-error "This item is not detached"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (new-name (agent-shell-queue--pick-shell-for-reassign "reassign to: ")))
    (agent-shell-queue--assign-item id new-name)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reassign-bucket-detached ()
  "Assign all items in the same dead bucket to an active or new shell."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (target (agent-shell-queue--iv-target))
              (_ (or (agent-shell-queue--item-detached-p target)
                     (user-error "This item is not detached")))
              (cell (assoc target (agent-shell-queue-store-items agent-shell-queue--store)))
              (ids (seq-map #'agent-shell-queue-item-id (cdr cell)))
              (new-name (agent-shell-queue--pick-shell-for-reassign
                         (format "reassign %d item(s) from dead shell to: " (length ids)))))
    (seq-do (lambda (item-id)
              (when (agent-shell-queue--item-by-id item-id)
                (agent-shell-queue--assign-item item-id new-name)))
            ids)
    (agent-shell-queue--refresh-buffer)
    (when (derived-mode-p 'agent-shell-queue-item-view-mode)
      (agent-shell-queue-item-view-refresh))))

(defun agent-shell-queue-item-view-reassign-all-detached ()
  "Assign all detached items across all buckets to an active or new shell."
  (interactive)
  (let* ((all-ids (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-filter (lambda (p) (agent-shell-queue--item-detached-p (car p))))
                    (seq-mapcat (lambda (p) (seq-map #'agent-shell-queue-item-id (cdr p)))))))
    (when (null all-ids)
      (user-error "No detached items in the queue"))
    (when-let* ((new-name (agent-shell-queue--pick-shell-for-reassign
                           (format "reassign %d detached item(s) to: " (length all-ids)))))
      (seq-do (lambda (item-id)
                (when (agent-shell-queue--item-by-id item-id)
                  (agent-shell-queue--assign-item item-id new-name)))
              all-ids)
      (agent-shell-queue--refresh-buffer)
      (when (derived-mode-p 'agent-shell-queue-item-view-mode)
        (agent-shell-queue-item-view-refresh)))))

(defun agent-shell-queue-item-view-edit ()
  "Open the edit buffer for the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id)))
              (qbuf agent-shell-queue--item-view-queue-buf)
              ((buffer-live-p qbuf)))
    (agent-shell-queue--assert-not-running item)
    (quit-window)
    (with-current-buffer qbuf
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) id))
                  (not (eobp)))
        (forward-line 1))
      (agent-shell-queue--open-edit-for-id id))))

;;; Item raw inspect

(defvar-local agent-shell-queue--inspect-id nil
  "ID of the queue item shown in this inspect buffer.")

(defvar-local agent-shell-queue--inspect-format nil
  "Serialization format currently used in this inspect buffer (plist, json, or yaml).")

(defvar agent-shell-queue-inspect-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") #'agent-shell-queue-inspect-as-plist)
    (define-key m (kbd "j") #'agent-shell-queue-inspect-as-json)
    (define-key m (kbd "y") #'agent-shell-queue-inspect-as-yaml)
    (define-key m (kbd "g") #'agent-shell-queue-inspect-refresh)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `agent-shell-queue-inspect-mode'.")

(define-minor-mode agent-shell-queue-inspect-mode
  "Minor mode active in queue item inspect buffers.
Binds p/j/y to switch formats, g to refresh, q to quit."
  :lighter nil
  :keymap agent-shell-queue-inspect-mode-map)

(defun agent-shell-queue--inspect-buffer-name (id format)
  "Buffer name for the raw inspect view of item ID in FORMAT."
  (format "*agent-shell-queue-inspect: %s [%s]*" id format))

(defun agent-shell-queue--serialize-single-item (item target format)
  "Serialize ITEM from TARGET bucket to a string in FORMAT."
  (pcase format
    ('plist
     (with-temp-buffer
       (pp (list :buffer target :item (agent-shell-queue-item-to-plist item))
           (current-buffer))
       (buffer-string)))
    ('json
     (unless (fboundp 'json-serialize)
       (user-error "json-serialize not available (requires Emacs 27+)"))
     (with-temp-buffer
       (insert (json-serialize (list :buffer target
                                     :item (agent-shell-queue--item-to-json item))))
       (when (fboundp 'json-pretty-print-buffer)
         (json-pretty-print-buffer))
       (buffer-string)))
    ('yaml
     (unless (fboundp 'yaml-encode)
       (user-error "yaml-encode not available; install the `yaml' package"))
     (yaml-encode
      (map-into (list (cons "buffer" target)
                      (cons "item" (agent-shell-queue--item-to-yaml item)))
                '(hash-table :test equal))))
    (_ (user-error "Unknown inspect format: %S" format))))

(defun agent-shell-queue-inspect-refresh ()
  "Refresh the current inspect buffer from live queue state."
  (interactive)
  (let ((id agent-shell-queue--inspect-id)
        (fmt agent-shell-queue--inspect-format))
    (unless (and id fmt)
      (user-error "Not in an agent-shell queue inspect buffer"))
    (when-let* ((pair (agent-shell-queue--item-by-id id))
                (inhibit-read-only t))
      (erase-buffer)
      (insert (agent-shell-queue--serialize-single-item (cdr pair) (car pair) fmt))
      (goto-char (point-min)))))

(defun agent-shell-queue--apply-inspect-format (format)
  "Switch the current inspect buffer to FORMAT and re-render."
  (unless agent-shell-queue--inspect-id
    (user-error "Not in an agent-shell queue inspect buffer"))
  (let ((saved-id agent-shell-queue--inspect-id)
        (inhibit-read-only t))
    (erase-buffer)
    (when-let* ((pair (agent-shell-queue--item-by-id saved-id)))
      (insert (agent-shell-queue--serialize-single-item
               (cdr pair) (car pair) format)))
    (rename-buffer (agent-shell-queue--inspect-buffer-name saved-id format) t)
    (pcase format
      ('plist (emacs-lisp-mode))
      ('json  (if (fboundp 'json-mode) (json-mode) (js-mode)))
      ('yaml  (if (fboundp 'yaml-mode) (yaml-mode) (fundamental-mode))))
    (setq-local agent-shell-queue--inspect-id saved-id)
    (setq-local agent-shell-queue--inspect-format format)
    (agent-shell-queue-inspect-mode 1)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun agent-shell-queue-inspect-as-plist ()
  "Show the current inspect item as a plist."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'plist))

(defun agent-shell-queue-inspect-as-json ()
  "Show the current inspect item as JSON."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'json))

(defun agent-shell-queue-inspect-as-yaml ()
  "Show the current inspect item as YAML."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'yaml))

(defun agent-shell-queue--inspect-format-display ()
  "Return an alist of (LABEL . ANNOTATION) for format completion.
Labels are format symbol names; the on-disk format is annotated with [on-disk]."
  (let ((on-disk (agent-shell-queue-store-format agent-shell-queue--store)))
    (seq-filter
     #'identity
     (list (cons "plist" (if (eq on-disk 'plist) "[on-disk]" ""))
           (cons "json"  (if (eq on-disk 'json)  "[on-disk]" ""))
           (when (fboundp 'yaml-encode)
             (cons "yaml" (if (eq on-disk 'yaml) "[on-disk]" "")))))))

(defun agent-shell-queue--inspect-prompt-format ()
  "Prompt for a serialization format; return the format symbol."
  (intern (annotated-completing-read
           (agent-shell-queue--inspect-format-display)
           :prompt "inspect format => "
           :category 'agent-shell-queue-inspect-format
           :require-match t
           :history 'agent-shell-queue-inspect-format)))

(defun agent-shell-queue--inspect-open (id format)
  "Open or refresh the inspect buffer for item ID in FORMAT."
  (let* ((pair (or (agent-shell-queue--item-by-id id)
                   (user-error "Item %s not found in queue" id)))
         (buf (get-buffer-create (agent-shell-queue--inspect-buffer-name id format))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (agent-shell-queue--serialize-single-item (cdr pair) (car pair) format)))
      (pcase format
        ('plist (emacs-lisp-mode))
        ('json  (if (fboundp 'json-mode) (json-mode) (js-mode)))
        ('yaml  (if (fboundp 'yaml-mode) (yaml-mode) (fundamental-mode))))
      (setq-local agent-shell-queue--inspect-id id)
      (setq-local agent-shell-queue--inspect-format format)
      (agent-shell-queue-inspect-mode 1)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun agent-shell-queue-buffer-inspect-item ()
  "Open a read-only raw-serialization view of the queue item at point.
Prompts for the serialization format (p=plist j=json y=yaml in the buffer)."
  (interactive)
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Not in an agent-shell queue buffer"))
  (agent-shell-queue--inspect-open
   (or (tabulated-list-get-id) (user-error "No item at point"))
   (agent-shell-queue--inspect-prompt-format)))

;;;###autoload
(defun agent-shell-queue-item-view-inspect ()
  "Open a raw-serialization view of the item shown in this buffer.
Prompts for the serialization format (p=plist j=json y=yaml in the buffer)."
  (interactive)
  (agent-shell-queue--inspect-open
   (or agent-shell-queue--item-view-id
       (user-error "Not in a queue item view buffer"))
   (agent-shell-queue--inspect-prompt-format)))

;;; Running guard

(defun agent-shell-queue--assert-not-running (item)
  "Signal `user-error' if ITEM status is `running'.
Running items may only be interrupted via the abort command."
  (when (eq (agent-shell-queue-item-status item) 'running)
    (user-error "Cannot modify a running item; abort it first")))

(defun agent-shell-queue-buffer-abort ()
  "Interrupt the running item at point and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))
    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (setf (agent-shell-queue-item-outcome item) 'canceled)
      (agent-shell-queue--insert-resume-task buf-name item)
      (agent-shell-queue--pause-and-save buf-name))))

(defun agent-shell-queue-item-view-abort ()
  "Interrupt the running displayed item and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))

    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (setf (agent-shell-queue-item-outcome item) 'canceled)
      (agent-shell-queue--insert-resume-task buf-name item)
      (agent-shell-queue--pause-and-save buf-name)
      (agent-shell-queue-item-view-refresh))))

;;; Transient predicates

(defun agent-shell-queue--iv-item ()
  "Return the item being viewed in the current item-view buffer, or nil."
  (when (and (boundp 'agent-shell-queue--item-view-id) agent-shell-queue--item-view-id)
    (cdr (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--item-detached-p (buf-name)
  "Return non-nil when BUF-NAME names a bucket whose buffer is not live.
Does not match the unassigned bucket — only truly detached (dead) targets."
  (and buf-name
       (not (equal buf-name agent-shell-queue--unassigned-key))
       (not (buffer-live-p (get-buffer buf-name)))))

(defun agent-shell-queue--iv-target ()
  "Return the bucket key for the item currently shown in this item-view buffer."
  (when (and (boundp 'agent-shell-queue--item-view-id)
             agent-shell-queue--item-view-id)
    (car (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--iv-detached-p ()
  "Return non-nil if the viewed item's target shell is no longer live."
  (agent-shell-queue--item-detached-p (agent-shell-queue--iv-target)))

(defun agent-shell-queue--iv-status ()
  "Return the status of the item being viewed, or nil."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--iv-bg-p ()
  "Return non-nil if the viewed item has background mode enabled."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-item ()
  "Return the queue item at point in the queue buffer, or nil."
  (when-let* ((id (and (derived-mode-p 'agent-shell-queue-mode)
                      (tabulated-list-get-id))))
    (cdr (agent-shell-queue--item-by-id id))))

(defun agent-shell-queue--point-status ()
  "Return the status of the queue item at point, or nil."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--point-bg-p ()
  "Return non-nil if the item at point has background mode enabled."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-running-p ()
  "Return non-nil when the item at point is running."
  (eq (agent-shell-queue--point-status) 'running))

(defun agent-shell-queue--point-not-running-p ()
  "Return non-nil when the item at point is not running."
  (not (agent-shell-queue--point-running-p)))

(defun agent-shell-queue--point-active-p ()
  "Return non-nil when the item at point is active."
  (eq (agent-shell-queue--point-status) 'active))

(defun agent-shell-queue--point-deferred-p ()
  "Return non-nil when the item at point is in any blocked state or draft."
  (let ((status (agent-shell-queue--point-status)))
    (or (eq status 'draft)
        (agent-shell-queue--blocked-status-p status))))

(defun agent-shell-queue--point-blocked-p ()
  "Return non-nil when the item at point has any blocked.* status."
  (agent-shell-queue--blocked-status-p (agent-shell-queue--point-status)))

(defun agent-shell-queue--point-done-p ()
  "Return non-nil when the item at point is done or aborted."
  (memq (agent-shell-queue--point-status) '(done aborted)))

(defun agent-shell-queue--point-dispatchable-p ()
  "Return non-nil when the item at point can be dispatched."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil draft))))

(defun agent-shell-queue--point-not-done-p ()
  "Return non-nil when the item at point is in a not-done, non-running state."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil))))

(defun agent-shell-queue--point-editable-p ()
  "Return non-nil when the item at point can be edited or moved.
Items in aborted state remain editable; only running, done, or absent items are excluded."
  (not (memq (agent-shell-queue--point-status) '(done running nil))))

(transient-define-prefix agent-shell-queue-item-destructive-menu ()
  "Destructive actions for the item shown in the current item-view buffer."
  [["Destructive"
    ("A" "Archive" agent-shell-queue-item-view-archive
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
    ("k" "Remove" agent-shell-queue-item-view-remove
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
    ("x" "Disable archiving" agent-shell-queue-toggle-archive
     :if (lambda () agent-shell-queue-archive-enabled))
    ("x" "Enable archiving" agent-shell-queue-toggle-archive
     :if (lambda () (not agent-shell-queue-archive-enabled)))]])

(defun agent-shell-queue-item-view-actions ()
  "Show available item-view actions via annotated-completing-read."
  (interactive)
  (let* ((visible (thread-last agent-shell-queue--item-view-action-table
                    (seq-filter (lambda (a)
                                  (let ((if-fn (plist-get a :if)))
                                    (and (plist-get a :group)
                                         (plist-get a :annotation)
                                         (or (null if-fn) (funcall if-fn))))))))
         (table (seq-map (lambda (a)
                           (cons (plist-get a :label) (plist-get a :annotation)))
                         visible)))
    (when-let* ((label (annotated-completing-read table
                                                  :prompt "item action: "
                                                  :category 'agent-shell-queue-item-action
                                                  :require-match t))
                (entry (seq-find (lambda (a) (equal (plist-get a :label) label))
                                 visible))
                (cmd (plist-get entry :cmd)))
      (call-interactively cmd))))

(defun agent-shell-queue--build-item-menu ()
  "Regenerate `agent-shell-queue-item-menu' from `agent-shell-queue--item-view-action-table'."
  (let* ((action-entries (seq-filter (lambda (a) (plist-get a :group))
                                     agent-shell-queue--item-view-action-table))
         (group-forms
          (seq-map
           (lambda (gname)
             (apply #'vector
                    gname
                    (seq-map
                     (lambda (a)
                       (let ((key    (plist-get a :key))
                             (label  (plist-get a :label))
                             (cmd    (plist-get a :cmd))
                             (if-fn  (plist-get a :if)))
                         (if if-fn
                             (list key label cmd :if if-fn)
                           (list key label cmd))))
                     (seq-filter (lambda (a) (equal (plist-get a :group) gname))
                                 action-entries))))
           agent-shell-queue--item-view-action-groups)))
    (eval
     `(transient-define-prefix agent-shell-queue-item-menu ()
        "Actions for the item shown in the current item-view buffer."
        ,@group-forms)
     t)))

(agent-shell-queue--build-item-menu)

(defun agent-shell-queue-buffer-move-up ()
  "Move the item at point one position earlier."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-move-down ()
  "Move the item at point one position later."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-enable-background-task ()
  "Flag the item at point for background sub-agent execution."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-disable-background-task ()
  "Clear the background sub-agent flag from the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-assign ()
  "Assign the item at point to a compatible buffer or unassigned.
Candidate buffers are filtered by the item's kind via the type registry.
Offers nil/unassigned as an option for deferred assignment."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id)))
    (agent-shell-queue--assert-not-running (cdr pair))
    (let* ((item (cdr pair))
           (kind (agent-shell-queue-item-kind item))
           (type (agent-shell-queue--type-for-kind kind))
           (pred (when type (agent-shell-queue-item-type-buffer-pred type)))
           (bufs (if pred
                     (seq-filter (lambda (b) (and (buffer-live-p b) (funcall pred b)))
                                 (buffer-list))
                   (agent-shell-buffers)))
           (current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (rows (seq-map (lambda (it)
                            (let* ((name (buffer-name it))
                                   (dir (buffer-local-value 'default-directory it))
                                   (dir-str (if (and current-dir (equal dir current-dir))
                                                (concat "(same dir) " (abbreviate-file-name dir))
                                              (abbreviate-file-name (or dir "")))))
                              (cons name
                                    (agent-shell-queue--annotation
                                     (format "%s  %s" dir-str
                                             (agent-shell-queue--buffer-state-label name))
                                     60))))
                          bufs))
           (table (cons (cons agent-shell-queue--unassigned-key "defer — unassigned bucket")
                        rows)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)))))

(defun agent-shell-queue-buffer-context-menu ()
  "Offer context-sensitive actions for the item at point via completing-read."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (let* ((status (agent-shell-queue-item-status item))
           (done (eq status 'done))
           (bg (agent-shell-queue-item-background item))
           (running (eq status 'running))
           (cmds (append
                  (unless done
                    (seq-remove #'null
                                (list
                       (cons "send now" #'agent-shell-queue-buffer-send)
                       (when (eq status 'active)
                         (cons "pause (suspend from dispatch)" #'agent-shell-queue-buffer-pause))
                       (when (agent-shell-queue--blocked-status-p status)
                         (cons "unblock" #'agent-shell-queue-buffer-unblock))
                       (when (agent-shell-queue--blocked-status-p status)
                         (cons "schedule (resume dispatch)" #'agent-shell-queue-buffer-schedule))
                       (when running
                         (cons "enqueue copy (repeat after current run)" #'agent-shell-queue-buffer-enqueue-running-copy))
                       (when running
                         (cons "untrack (remove from queue without aborting)" #'agent-shell-queue-buffer-untrack-running))
                       (unless running
                         (if bg
                             (cons "disable background sub-agent" #'agent-shell-queue-buffer-disable-background-task)
                           (cons "enable background sub-agent" #'agent-shell-queue-buffer-enable-background-task)))
                       (unless running (cons "assign to shell" #'agent-shell-queue-buffer-assign))
                       (unless running (cons "move up" #'agent-shell-queue-buffer-move-up))
                       (unless running (cons "move down" #'agent-shell-queue-buffer-move-down))
                       (cons "insert pause checkpoint" #'agent-shell-queue-insert-pause)
                       (cons "insert context drop" #'agent-shell-queue-insert-clear-context))))
                  (when done
                    (list (cons "re-enqueue (new active copy)" #'agent-shell-queue-buffer-reenqueue)))
                  (unless running
                    (list (cons "remove" #'agent-shell-queue-buffer-remove)))))
           (table (seq-map (lambda (it)
                             (cons (car it)
                                   (or (car (split-string (or (documentation (cdr it)) "") "\n")) "")))
                           cmds)))
      (when-let* ((choice (annotated-completing-read table
                                                     :prompt "action => "
                                                     :category 'agent-shell-queue-action
                                                     :require-match t
                                                     :history 'agent-shell-queue-buffer-context-menu))
                  (cmd (cdr (assoc choice cmds))))
        (call-interactively cmd)))))

(defun agent-shell-queue--get-or-create-buffer ()
  "Return the *agent-shell-queue* buffer, initializing it if needed."
  (let ((buf (get-buffer-create "*agent-shell-queue*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-queue-mode)
        (agent-shell-queue-mode))
      (agent-shell-queue-buffer-refresh))
    buf))

;;;###autoload
(defun agent-shell-queue-buffer-open ()
  "Open (or refresh) the *agent-shell-queue* buffer."
  (interactive)
  (pop-to-buffer (agent-shell-queue--get-or-create-buffer)))

;;;###autoload
(defun agent-shell-queue-buffer-switch ()
  "Switch to the *agent-shell-queue* buffer in the current window."
  (interactive)
  (switch-to-buffer (agent-shell-queue--get-or-create-buffer)))

(defun agent-shell-queue-buffer-toggle-only-mode ()
  "Toggle queue-only mode in the shell buffer for the item at point.
Done and aborted items cannot be attached to a shell — re-enqueue them first.
When the shell buffer is dead, picks a live replacement via completing-read."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair))
              (item (cdr pair)))
    (when (memq (agent-shell-queue-item-status item) '(done aborted))
      (user-error "Cannot toggle queue-only mode for a %s item; re-enqueue it first"
                  (agent-shell-queue-item-status item)))
    (let ((buf (or (get-buffer buf-name)
                   (agent-shell-queue--pick-buffer
                    (format "Buffer '%s' is gone. Enable queue-only mode in: " buf-name))
                   (user-error "No live agent-shell buffers available"))))
      (with-current-buffer buf
        (agent-shell-queue-set-input-mode
         (if (eq agent-shell-queue-input-mode 'queue-only) 'default 'queue-only))))))

(defun agent-shell-queue-intercept-p ()
  "Return non-nil when queue-intercept mode is active in the current shell buffer."
  (and (featurep 'agent-shell-queue)
       (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
         (eq (buffer-local-value 'agent-shell-queue-input-mode shell) 'queue-intercept))))

(defun agent-shell-queue-input-mode-value ()
  "Return `agent-shell-queue-input-mode' for the current shell buffer, or `default'."
  (or (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
        (buffer-local-value 'agent-shell-queue-input-mode shell))
      'default))

(defun agent-shell-queue-only-enable ()
  (interactive)
  (agent-shell-queue-set-input-mode 'queue-only))

(defun agent-shell-queue-only-p ()
  "Return non-nil when queue-only mode is active in the current shell buffer."
  (and (featurep 'agent-shell-queue)
       (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
         (eq (buffer-local-value 'agent-shell-queue-input-mode shell) 'queue-only))))

(defun agent-shell-queue-only-disable-in-buffer (&optional buf)
  (interactive)
  (with-current-buffer (or (when (bufferp buf) buf)
                           (when (get-buffer buf) buf)
                           (current-buffer))
    (when (eq agent-shell-queue-input-mode 'queue-only)
      (agent-shell-queue-set-input-mode 'default))))

(defun agent-shell-queue-only-disable ()
  (interactive)
  (agent-shell-queue-only-disable-in-buffer))

(defun agent-shell-queue-only-disable-all ()
  (interactive)
  (let ((cleared (seq-filter (lambda (buf)
                               (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                   'queue-only))
                             (agent-shell-buffers))))
    (seq-do #'agent-shell-queue-only-disable-in-buffer cleared)
    (when cleared (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: queue-only disabled in %d buffer(s)" (length cleared))))

(defun agent-shell-queue-buffer-open-shell ()
  "Switch to the shell buffer for the item at point.
If the buffer is not live and the item's executor provides a create
function, offer to create a new buffer of the same type."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair)))
    (if-let* ((buf (get-buffer buf-name)))
        (pop-to-buffer buf)
      (if-let* ((item (cdr pair))
                (executor-fn (agent-shell-queue-item-executor item))
                (executor-name (agent-shell-queue--executor-name executor-fn))
                (entry (agent-shell-queue--find-executor executor-name))
                (create-fn (agent-shell-queue-executor-create entry)))
          (when (y-or-n-p (format "Buffer %s is not live. Create a new one? " buf-name))
            (when-let* ((new-buf (funcall create-fn)))
              (pop-to-buffer new-buf)))
        (user-error "Shell buffer %s is not live" buf-name)))))

(defun agent-shell-queue-item-view-open-shell ()
  "Switch to the shell buffer for the item shown in this view.
If the buffer is not live and the item's executor provides a create
function, offer to create a new buffer of the same type."
  (interactive)
  (when-let* ((buf-name (agent-shell-queue--iv-target)))
    (if-let* ((buf (get-buffer buf-name)))
        (pop-to-buffer buf)
      (if-let* ((item (agent-shell-queue--iv-item))
                (executor-fn (agent-shell-queue-item-executor item))
                (executor-name (agent-shell-queue--executor-name executor-fn))
                (entry (agent-shell-queue--find-executor executor-name))
                (create-fn (agent-shell-queue-executor-create entry)))
          (when (y-or-n-p (format "Buffer %s is not live. Create a new one? " buf-name))
            (when-let* ((new-buf (funcall create-fn)))
              (pop-to-buffer new-buf)))
        (user-error "Shell buffer %s is not live" buf-name)))))

;;;###autoload
(defun agent-shell-queue-capture (&optional buf)
  "Open a capture buffer targeting BUF (nil adds to the unassigned queue).
When called interactively from an agent-shell buffer, targets that buffer.
With a prefix argument, opens an unassigned capture instead."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture buf (current-buffer)))

;;; Display toggle commands

(defun agent-shell-queue-toggle-buffer-column ()
  "Toggle visibility of the Buffer column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-buffer-column (not agent-shell-queue-show-buffer-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue buffer column: %s"
	   (if agent-shell-queue-show-buffer-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-ordinal-column ()
  "Toggle visibility of the ordinal (#) column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-ordinal-column (not agent-shell-queue-show-ordinal-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue ordinal column: %s"
	   (if agent-shell-queue-show-ordinal-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-age-column ()
  "Toggle visibility of the Age column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-age-column (not agent-shell-queue-show-age-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue age column: %s"
           (if agent-shell-queue-show-age-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-kind-column ()
  "Toggle visibility of the Kind column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-kind-column (not agent-shell-queue-show-kind-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue kind column: %s"
           (if agent-shell-queue-show-kind-column "on" "off")))

(defun agent-shell-queue-toggle-multiline-format ()
  "Toggle multi-line display format for the queue buffer."
  (interactive)
  (setq agent-shell-queue-multiline-format
        (not agent-shell-queue-multiline-format))
  (agent-shell-queue-buffer-refresh)
  (message "Queue multi-line format: %s"
           (if agent-shell-queue-multiline-format "on" "off")))

;;;###autoload
(defun agent-shell-queue-select-columns ()
  "Pick column display options via `annotated-completing-read'.
Offers bulk presets, per-column visibility toggles, and the multi-line
format switch.  Changes take effect immediately via `agent-shell-queue-buffer-refresh'."
  (interactive)
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Not in an agent-shell queue buffer"))
  (let* ((columns `(("Buffer column" . agent-shell-queue-show-buffer-column)
                    ("Ordinal # column" . agent-shell-queue-show-ordinal-column)
                    ("Age column" . agent-shell-queue-show-age-column)
                    ("Kind column" . agent-shell-queue-show-kind-column)))
         (table (map-into
                 (append
                  (list (cons "+ show all columns"
                              (if (and agent-shell-queue-show-buffer-column
                                       agent-shell-queue-show-ordinal-column
                                       agent-shell-queue-show-age-column
                                       agent-shell-queue-show-kind-column)
                                  "already showing all columns"
                                "enable Buffer, Ordinal, Age, and Kind columns"))
                        (cons "+ minimal: status and prompt only"
                              (if (not (or agent-shell-queue-show-buffer-column
                                           agent-shell-queue-show-ordinal-column
                                           agent-shell-queue-show-age-column
                                           agent-shell-queue-show-kind-column))
                                  "already minimal"
                                "hide Buffer, Ordinal, Age, and Kind columns")))
                  (seq-map (lambda (it)
                             (cons (car it)
                                   (if (symbol-value (cdr it))
                                       "visible · click to hide"
                                     "hidden · click to show")))
                           columns)
                  (list (cons "Multi-line format"
                              (if agent-shell-queue-multiline-format
                                  "on · prompt on second line · click to disable"
                                "off · single-line · click to enable"))))
                 '(hash-table :test equal))))
    (when-let* ((choice (annotated-completing-read
                        table
                        :prompt "queue columns: "
                        :category 'agent-shell-queue-column
                        :require-match t
                        :history 'agent-shell-queue-select-columns)))
      (cond
       ((equal choice "+ show all columns")
        (setq agent-shell-queue-show-buffer-column t
              agent-shell-queue-show-ordinal-column t
              agent-shell-queue-show-age-column t
              agent-shell-queue-show-kind-column t))
       ((equal choice "+ minimal: status and prompt only")
        (setq agent-shell-queue-show-buffer-column nil
              agent-shell-queue-show-ordinal-column nil
              agent-shell-queue-show-age-column nil
              agent-shell-queue-show-kind-column nil))
       ((equal choice "Multi-line format")
        (setq agent-shell-queue-multiline-format (not agent-shell-queue-multiline-format)))
       (t
        (when-let* ((var (cdr (assoc choice columns))))
          (set var (not (symbol-value var))))))
      (agent-shell-queue-buffer-refresh))))

;;; Transient menu

(transient-define-prefix agent-shell-queue-destructive-menu ()
  "Destructive actions for the item at point in the queue buffer."
  [["Destructive"
    ("A" "Archive" agent-shell-queue-buffer-archive
     :if agent-shell-queue--point-not-running-p)
    ("k" "Remove" agent-shell-queue-buffer-remove
     :if agent-shell-queue--point-not-running-p)
    ("x" "Disable archiving" agent-shell-queue-toggle-archive
     :if-non-nil agent-shell-queue-archive-enabled)
    ("x" "Enable archiving" agent-shell-queue-toggle-archive
     :if-nil agent-shell-queue-archive-enabled)]])

(define-advice agent-shell-queue-destructive-menu (:before () guard-queue-buffer)
  "Signal an error when not invoked from an agent-shell queue overview buffer."
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Queue menu is only available from the agent-shell queue buffer")))

(transient-define-prefix agent-shell-queue-dispatch ()
  "Actions for the item at point in the queue buffer."
  [["Session"
    (".p" "Pause" agent-shell-queue-session-pause
     :inapt-if agent-shell-queue-session-paused-p)
    (".r" "Resume" agent-shell-queue-session-resume
     :inapt-if-not agent-shell-queue-session-paused-p)
    (".k" "Recover stuck" agent-shell-queue-recover-stuck-shell)
    (".m" agent-shell-queue-toggle-input-mode
     :description (lambda ()
                    (format "Input mode: [%s]" agent-shell-queue-input-mode)))]
   ["Fork"
    :if agent-shell-queue--point-item
    ("ff" "Fork queue" agent-shell-queue-buffer-fork)
    ("fb" "Insert before" agent-shell-queue-buffer-insert-fork-before)
    ("fa" "Insert after" agent-shell-queue-buffer-insert-fork-after)
    ("fr" "Release pending" agent-shell-queue-release-pending-fork)]
   ["All"
    ("gp" "Pause all" agent-shell-queue-pause)
    ("gr" "Resume all" agent-shell-queue-resume)
    ("ga" "Resume all sessions" agent-shell-queue-unpause-all-sessions)
    ("gi" "Reset all to default" agent-shell-queue-reset-all-input-modes)
    ("gm" agent-shell-queue-set-input-mode-default
     :description (lambda ()
                    (format "[%s] Input mode default" agent-shell-queue-input-mode-default)))]
   ["Task"
    :if agent-shell-queue--point-item
    ("!" "Dispatch now" agent-shell-queue-buffer-send
     :if agent-shell-queue--point-dispatchable-p)
    ("a" "Abort" agent-shell-queue-buffer-abort
     :if agent-shell-queue--point-running-p)
    ("R" "Re-enqueue" agent-shell-queue-buffer-reenqueue
     :if agent-shell-queue--point-done-p)
    ("z" "Mark done" agent-shell-queue-buffer-mark-done
     :if agent-shell-queue--point-not-done-p)
    ("e" "Enqueue" agent-shell-queue-enqueue-dispatch)
    ("tp" "Pause item" agent-shell-queue-buffer-pause
     :if agent-shell-queue--point-active-p)
    ("tr" "Schedule" agent-shell-queue-buffer-schedule
     :if agent-shell-queue--point-deferred-p)
    ("tu" "Unblock" agent-shell-queue-buffer-unblock
     :if agent-shell-queue--point-blocked-p)
    ("tc" "Enqueue copy" agent-shell-queue-buffer-enqueue-running-copy
     :if agent-shell-queue--point-running-p)
    ("tk" "Untrack running" agent-shell-queue-buffer-untrack-running
     :if agent-shell-queue--point-running-p)
    ("te" "Edit" agent-shell-queue-edit-task)
    ("tbe" "Background on" agent-shell-queue-buffer-enable-background-task
     :if agent-shell-queue--point-editable-p
     :inapt-if agent-shell-queue--point-bg-p)
    ("tbd" "Background off" agent-shell-queue-buffer-disable-background-task
     :if agent-shell-queue--point-editable-p
     :inapt-if-not agent-shell-queue--point-bg-p)
    ("td" "Destructive…" agent-shell-queue-destructive-menu
     :if agent-shell-queue--point-not-running-p)
    ("jo" "Open shell" agent-shell-queue-buffer-open-shell)
    ("lu" "Move up" agent-shell-queue-buffer-move-up
     :if agent-shell-queue--point-editable-p)
    ("ld" "Move down" agent-shell-queue-buffer-move-down
     :if agent-shell-queue--point-editable-p)
    ("ta" "Assign to shell…" agent-shell-queue-buffer-assign
     :if agent-shell-queue--point-editable-p)]]
  [["Capture"
    ("cw" "Compose" agent-shell-queue-capture)
    ("ca" "After point" agent-shell-queue-buffer-capture-after)
    ("cu" "Unassigned" agent-shell-queue-capture-unassigned)
    ("cr" "From region" agent-shell-queue-capture-from-region)
    ("cy" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("cc" "From context" agent-shell-queue-capture-from-context)
    ("ce" "Enqueue prompt" agent-shell-queue-enqueue)
    ("cx" "Enqueue clear" agent-shell-queue-enqueue-clear)]
   ["Insert"
    ("ip" "Pause checkpoint" agent-shell-queue-insert-pause)
    ("id" "Context drop" agent-shell-queue-insert-clear-context)
    ("ic" "Compact (manual)" agent-shell-queue-insert-compact)
    ("ie" "Emacs call" agent-shell-queue-enqueue-emacs)
    ("iw" "Wait-until (timer)" agent-shell-queue-insert-wait)]
   ["Scope / Export"
    ("sn" "Set scope" agent-shell-queue-set-scope)
    ("sw" "Global scope" agent-shell-queue-scope-global)
    ("v" "Export to YAML" agent-shell-queue-export)
    ("sf" "Flush to disk" agent-shell-queue-flush)
    ("sd" "Show disk state" agent-shell-queue-show-disk-state)
    ("=" "Inspect item…" agent-shell-queue-buffer-inspect-item
     :if agent-shell-queue--point-item)]
   ["Display"
    ("dv" "Column options" agent-shell-queue-select-columns)
    ("db" agent-shell-queue-toggle-buffer-column
     :description (lambda ()
                    (if agent-shell-queue-show-buffer-column
                        "[x] Buffer column"
                      "[ ] Buffer column")))
    ("dn" agent-shell-queue-toggle-ordinal-column
     :description (lambda ()
                    (if agent-shell-queue-show-ordinal-column
                        "[x] Ordinal column"
                      "[ ] Ordinal column")))
    ("da" agent-shell-queue-toggle-age-column
     :description (lambda ()
                    (if agent-shell-queue-show-age-column
                        "[x] Age column"
                      "[ ] Age column")))
    ("dk" agent-shell-queue-toggle-kind-column
     :description (lambda ()
                    (if agent-shell-queue-show-kind-column
                        "[x] Kind column"
                      "[ ] Kind column")))
    ("dm" agent-shell-queue-toggle-multiline-format
     :description (lambda ()
                    (if agent-shell-queue-multiline-format
                        "[x] Multi-line"
                      "[ ] Multi-line")))]
   ["Edit / Import"
    ("ly" "Raw edit (YAML)" agent-shell-queue-raw-edit)
    ("li" "Import (YAML)" agent-shell-queue-import)
    ("lr" "Reload from disk" agent-shell-queue-reload)]])

(define-advice agent-shell-queue-dispatch (:before () guard-queue-buffer)
  "Signal an error when not invoked from an agent-shell queue overview buffer."
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Queue menu is only available from the agent-shell queue buffer")))

;;; Enqueue dispatch

(defun agent-shell-queue-enqueue-dispatch ()
  "Choose an item kind and target buffer via `annotated-completing-read', then collect input.
Choices are built from the item-type registry.  nil/unassigned is always
offered as a target so items can be deferred for later assignment."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((choices (seq-map
                   (lambda (type)
                     (cons (agent-shell-queue-item-type-label type)
                           (let ((pred (agent-shell-queue-item-type-buffer-pred type)))
                             (cond
                              ((null pred)                                           "any buffer or unassigned")
                              ((eq pred #'agent-shell-queue--agent-shell-buffer-p)  "agent-shell session")
                              ((eq pred #'agent-shell-queue--eshell-buffer-p)       "eshell buffer")
                              ((eq pred #'agent-shell-queue--eat-buffer-p)          "eat buffer")
                              (t                                                     "compatible buffer")))))
                   agent-shell-queue--item-types))
         (choice (annotated-completing-read choices :prompt "enqueue: " :require-match t))
         (type (seq-find (lambda (e)
                           (equal (agent-shell-queue-item-type-label e) choice))
                         agent-shell-queue--item-types)))
    (when type
      (let ((buf (agent-shell-queue--pick-buffer-for-kind
                  (agent-shell-queue-item-type-kind type)
                  "Target (or unassigned): ")))
        (agent-shell-queue--invoke-input-for-type type buf)))))

;;; Edit popup

(defvar agent-shell-queue-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-edit-cancel)
    (define-key m (kbd "C-x C-s") #'agent-shell-queue-edit-save-and-flush)
    (define-key m (kbd "C-c C-f") #'agent-shell-queue-insert-file)
    (define-key m (kbd "C-c M-f") #'agent-shell-queue-insert-buffer)
    m)
  "Keymap for `agent-shell-queue-edit-mode'.")

(define-derived-mode agent-shell-queue-edit-mode markdown-mode "Queue-Edit"
  "Mode for editing a queued prompt in a popup buffer.")

(defvar-local agent-shell-queue--editing-id nil
  "Item ID being edited in this `agent-shell-queue-edit-mode' buffer.")

(defun agent-shell-queue--open-edit-for-id (id)
  "Open an edit popup for the item with ID.
Enforces the one-edit-at-a-time constraint: if a different item is already
being edited, switches to that buffer and signals an error."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (if-let* ((existing (get-buffer "*agent-shell-queue-edit*"))
              (_ (buffer-live-p existing))
              (_ (not (equal (buffer-local-value 'agent-shell-queue--editing-id existing) id))))
        (progn
          (pop-to-buffer existing '(display-buffer-below-selected))
          (user-error "agent-shell-queue: already editing item %s — save or cancel first"
                      (buffer-local-value 'agent-shell-queue--editing-id existing)))
      (let ((edit-buf (get-buffer-create "*agent-shell-queue-edit*")))
          (with-current-buffer edit-buf
            (when-let* ((prev-id agent-shell-queue--editing-id))
              (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
                    (delete prev-id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
            (erase-buffer)
            (insert (agent-shell-queue-item-args item))
            (agent-shell-queue-edit-mode)
            (setq-local agent-shell-queue--editing-id id)
            (let* ((bucket-name (car pair))
                   (bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
                   (depth (agent-shell-queue--active-item-count bucket-items))
                   (state (agent-shell-queue--activity-state)))
              (setq-local header-line-format
                          (concat
                           (propertize (format " %s  |  " bucket-name) 'face 'shadow)
                           state
                           (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
          (cl-pushnew id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) :test #'equal)
          (agent-shell-queue--refresh-buffer)
          (pop-to-buffer edit-buf '(display-buffer-below-selected))))))

;;;###autoload
(defun agent-shell-queue-edit-task (&optional select)
  "Edit a queued item's prompt.
In `agent-shell-queue-mode' without SELECT (prefix argument): edit the item at
point immediately.  With SELECT, or when point carries no item, or when called
from outside `agent-shell-queue-mode': select via annotated-completing-read.
Candidates include all non-done, non-running items across all buffers."
  (interactive "P")
  (agent-shell-queue--ensure-loaded)
  (if-let* ((_ (not select))
            (_ (derived-mode-p 'agent-shell-queue-mode))
            (id (tabulated-list-get-id)))
      (agent-shell-queue--open-edit-for-id id)
    (let ((table (make-hash-table :test #'equal))
          (id-by-key (make-hash-table :test #'equal)))
      (seq-do (lambda (pair)
                (let* ((buf-name (car pair))
                       (buf (get-buffer buf-name))
                       (buf-state (cond
                                   ((member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)) "paused")
                                   ((and buf (with-current-buffer buf (shell-maker-busy))) "busy")
                                   (t "idle")))
                       (it-index 0))
                  (seq-do
                   (lambda (it)
                     (unless (memq (agent-shell-queue-item-status it) '(done running))
                       (let* ((id (agent-shell-queue-item-id it))
                              (prompt (agent-shell-queue-item-args it))
                              (status (agent-shell-queue--status-string it))
                              (age (agent-shell-queue--format-age
                                    (time-since (agent-shell-queue-item-created it))))
                              (pos (1+ it-index))
                              (key (format "%s: %s" id
                                           (agent-shell-queue--annotation prompt 60)))
                              (ann (format "#%d · %s [%s] · %s · %s"
                                           pos buf-name buf-state status age)))
                         (setf (map-elt table key) ann)
                         (setf (map-elt id-by-key key) id)))
                     (cl-incf it-index))
                   (cdr pair))))
              (agent-shell-queue-store-items agent-shell-queue--store))
      (when (zerop (hash-table-count table))
        (user-error "No editable queued items"))
      (when-let* ((choice (annotated-completing-read table
                                                     :prompt "edit task: "
                                                     :category 'agent-shell-queue-item
                                                     :require-match t
                                                     :history 'agent-shell-queue-edit-task))
                  (id (map-elt id-by-key choice))) ;; map-elt might be able to handle the nill choice output and so this could all be inlined 
        (agent-shell-queue--open-edit-for-id id)))))

(defun agent-shell-queue-edit-save-and-flush ()
  "Save the edited prompt, close the popup, and flush the queue to disk."
  (interactive)
  (agent-shell-queue-edit-confirm)
  (agent-shell-queue--save)
  (message "agent-shell-queue: edit saved and flushed to disk"))

(defun agent-shell-queue-edit-confirm ()
  "Save the edited prompt and close the popup."
  (interactive)
  (let ((new-prompt (string-trim (buffer-string)))
        (id agent-shell-queue--editing-id))
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))
    (quit-window t)
    (unless (string-empty-p new-prompt)
      (agent-shell-queue-edit id new-prompt))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-edit-cancel ()
  "Discard edits and close the popup."
  (interactive)
  (let ((id agent-shell-queue--editing-id))
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
  (quit-window t)
  (agent-shell-queue--refresh-buffer))

;;; Capture buffer

(defvar agent-shell-queue-capture-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-capture-cancel)
    (define-key m (kbd "C-c C-s") #'agent-shell-queue-capture-save-draft)
    (define-key m (kbd "C-c C-b") #'agent-shell-queue-capture-enable-background-task)
    (define-key m (kbd "C-c M-b") #'agent-shell-queue-capture-disable-background-task)
    (define-key m (kbd "C-c C-y") #'agent-shell-queue-capture-yank-kill)
    (define-key m (kbd "C-c M-w") #'agent-shell-queue-capture-yank-clipboard)
    (define-key m (kbd "C-c C-p") #'agent-shell-queue-capture-insert-thing-at-point)
    (define-key m (kbd "C-c C-x") #'agent-shell-queue-capture-select-context)
    (define-key m (kbd "C-c C-f") #'agent-shell-queue-insert-file)
    (define-key m (kbd "C-c M-f") #'agent-shell-queue-insert-buffer)
    m)
  "Keymap for `agent-shell-queue-capture-mode'.")

(define-derived-mode agent-shell-queue-capture-mode markdown-mode "Queue-Capture"
  "Mode for composing a queued agent-shell prompt.
\\{agent-shell-queue-capture-mode-map}"
  (setq-local electric-indent-inhibit t))

(defvar-local agent-shell-queue--capture-target nil
  "Target agent-shell buffer for this capture session.")

(defvar-local agent-shell-queue--capture-origin nil
  "Buffer from which capture was launched, used for context insertion.")

(defvar-local agent-shell-queue--capture-background-task nil
  "When non-nil, the captured prompt will be flagged for background execution.")

(defvar-local agent-shell-queue--capture-after-id nil
  "When non-nil, the confirmed item will be inserted after this item ID.")

(defvar-local agent-shell-queue--capture-draft-id nil
  "ID of the draft item saved from this capture buffer, or nil.
Set by `agent-shell-queue-capture-save-draft' and used to update rather
than duplicate the draft on subsequent saves.")

(defvar-local agent-shell-queue--capture-kind 'prompt
  "Kind of queue item to create when this capture buffer is confirmed.
Defaults to `prompt'; set to `emacs-lisp' for Emacs Lisp capture buffers.")

(defun agent-shell-queue--open-capture (target-buf &optional origin-buf initial-content kind mode)
  "Open a capture buffer targeting TARGET-BUF (nil for unassigned queue).
Multiple capture buffers can be open simultaneously; each is named after its target.
ORIGIN-BUF is used for context-insertion commands; defaults to current buffer.
INITIAL-CONTENT, when non-nil, is inserted into the buffer before display.
KIND sets `agent-shell-queue--capture-kind' (defaults to `prompt').
MODE, when non-nil, is a major-mode function used instead of `agent-shell-queue-capture-mode';
C-c C-c and C-c C-k are bound in the new mode's local map."
  (let* ((bucket-name (if target-buf
                          (buffer-name target-buf)
                        agent-shell-queue--unassigned-key))
         (kind-label (when (and kind (not (eq kind 'prompt)))
                       (concat "  |  " (symbol-name kind))))
         (capture-buf (get-buffer-create
                       (if target-buf
                           (format "*agent-shell-queue-capture: %s*" (buffer-name target-buf))
                         "*agent-shell-queue-capture: unassigned*"))))
    (with-current-buffer capture-buf
      (erase-buffer)
      (if mode
          (progn
            (funcall mode)
            (use-local-map (copy-keymap (current-local-map)))
            (local-set-key (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
            (local-set-key (kbd "C-c C-k") #'agent-shell-queue-capture-cancel))
        (agent-shell-queue-capture-mode))
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (or origin-buf (current-buffer))
            agent-shell-queue--capture-background-task nil
            agent-shell-queue--capture-after-id nil
            agent-shell-queue--capture-kind (or kind 'prompt))
      (when (and initial-content (not (string-empty-p initial-content)))
        (insert initial-content))
      (let* ((bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
             (depth (agent-shell-queue--active-item-count bucket-items))
             (state (agent-shell-queue--activity-state)))
        (setq-local header-line-format
                    (concat
                     (propertize (format " %s%s  |  " bucket-name (or kind-label "")) 'face 'shadow)
                     state
                     (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

;;; Registry dispatch functions

(defun agent-shell-queue--dispatch-to-session (item buf-name)
  "Dispatch ITEM to its agent-shell session via the default executor."
  (agent-shell-queue--default-executor item (agent-shell-queue-item-args item)))

(defun agent-shell-queue--dispatch-emacs-lisp (item buf-name)
  "Dispatch an emacs-lisp ITEM by evaluating its args as a Lisp form."
  (condition-case err
      (eval (read (agent-shell-queue-item-args item)) t)
    (error (message "agent-shell-queue: emacs-lisp %s error: %s"
                    (agent-shell-queue-item-id item) err)))
  (agent-shell-queue--complete-item item buf-name))

(defun agent-shell-queue--dispatch-emacs-command (item buf-name)
  "Dispatch an emacs-command ITEM by invoking it interactively."
  (condition-case err
      (call-interactively (intern (agent-shell-queue-item-args item)))
    (error (message "agent-shell-queue: emacs-command %s error: %s"
                    (agent-shell-queue-item-id item) err)))
  (agent-shell-queue--complete-item item buf-name))

(defun agent-shell-queue--dispatch-pause-compact (item buf-name)
  "Dispatch a pause or compact ITEM: pause the queue or delay and alert."
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
  "Dispatch a shell-eshell ITEM by inserting and sending in the eshell buffer."
  (if-let* ((buf (get-buffer buf-name)))
      (progn
        (with-current-buffer buf
          (eshell-insert-and-send (agent-shell-queue-item-args item)))
        (agent-shell-queue--complete-item item buf-name))
    (message "agent-shell-queue: eshell buffer %s gone for item %s"
             buf-name (agent-shell-queue-item-id item))))

(declare-function eat-term-send-string "eat")

(defun agent-shell-queue--dispatch-shell-eat (item buf-name)
  "Dispatch a shell-eat ITEM via eat-term-send-string.
See ~/garen/plans/emacs/shell-eat-enhancement.org for improvement notes."
  (if-let* ((buf (get-buffer buf-name)))
      (progn
        (with-current-buffer buf
          (when (bound-and-true-p eat-terminal)
            (eat-term-send-string eat-terminal
                                  (concat (agent-shell-queue-item-args item) "\n"))))
        (agent-shell-queue--complete-item item buf-name))
    (message "agent-shell-queue: eat buffer %s gone for item %s"
             buf-name (agent-shell-queue-item-id item))))

;;; Registry input helpers

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

;;; Capture buffers

(defun agent-shell-queue--open-elisp-capture (target-buf)
  "Open an Emacs Lisp capture buffer targeting TARGET-BUF's queue.
The buffer is in `emacs-lisp-mode' with C-c C-c / C-c C-k bindings.
Items created from this buffer have kind `emacs-lisp'."
  (let* ((capture-buf (get-buffer-create
                       (format "*agent-shell-queue-elisp: %s*"
                               (buffer-name target-buf))))
         (bucket-name (buffer-name target-buf)))
    (with-current-buffer capture-buf
      (erase-buffer)
      (emacs-lisp-mode)
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
      (local-set-key (kbd "C-c C-k") #'agent-shell-queue-capture-cancel)
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (current-buffer)
            agent-shell-queue--capture-background-task nil
            agent-shell-queue--capture-after-id nil
            agent-shell-queue--capture-kind 'emacs-lisp)
      (let* ((bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
             (depth (agent-shell-queue--active-item-count bucket-items))
             (state (agent-shell-queue--activity-state)))
        (setq-local header-line-format
                    (concat
                     (propertize (format " %s  |  emacs-lisp  |  " bucket-name) 'face 'shadow)
                     state
                     (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

(defun agent-shell-queue--close-capture-window ()
  "Delete the current capture window and kill its buffer.
Explicitly deletes the window first so the split disappears regardless of
how the window was opened (i.e. independent of `quit-restore' state)."
  (let ((win (selected-window))
        (buf (current-buffer)))
    (if (and (not (one-window-p)) (window-deletable-p win))
        (progn (delete-window win) (kill-buffer buf))
      (kill-buffer buf))))

(defun agent-shell-queue--capture-plan-mode-choice (buf)
  "When BUF's session is in a blocking mode, prompt for what to do.
Returns non-nil if the item should be queued as `blocked.skip'.
If the user chooses \"switch mode\", calls `agent-shell-set-session-mode'
interactively on BUF and returns nil so the item is queued normally.
Cancelling (C-g) defaults to \"queue as blocked\"."
  (when (and buf (agent-shell-queue--session-mode-blocked-p buf))
    (let* ((mode-id (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                    '(:session :mode-id)))
           (choice (condition-case nil
                       (completing-read
                        (format "Session in %s mode: " mode-id)
                        '("queue as blocked" "switch mode")
                        nil t nil nil "queue as blocked")
                     (quit "queue as blocked"))))
      (if (equal choice "switch mode")
          (progn
            (with-current-buffer buf
              (call-interactively #'agent-shell-set-session-mode))
            nil)
        t))))

(defun agent-shell-queue-capture-confirm ()
  "Confirm capture: queue the buffer contents and close."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task)
        (after-id agent-shell-queue--capture-after-id)
        (kind agent-shell-queue--capture-kind))
    (let ((use-blocked (agent-shell-queue--capture-plan-mode-choice buf)))
      (agent-shell-queue--close-capture-window)
      (unless (string-empty-p prompt)
        (message "agent-shell: %s" (truncate-string-to-width prompt 200 nil nil "…"))
        (cond
         (after-id
          (when-let* ((pair (agent-shell-queue--item-by-id after-id))
                      (bucket-name (car pair))
                      (item (agent-shell-queue--make-item prompt bg kind)))
            (when use-blocked
              (setf (agent-shell-queue-item-status item) 'blocked.skip))
            (let ((items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store)))))
              (if-let* ((idx (cl-position after-id items
                                          :key #'agent-shell-queue-item-id :test #'equal))
                        (cell (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
                  (setcdr cell (append (cl-subseq items 0 (1+ idx))
                                       (list item)
                                       (cl-subseq items (1+ idx))))
                (agent-shell-queue--add-item-to-bucket bucket-name item)))
            (when buf (agent-shell-queue--ensure-subscription buf))
            (agent-shell-queue--save)
            (agent-shell-queue--refresh-buffer)))
         (buf
          (with-agent-shell-queue
            (let ((item (agent-shell-queue--make-item prompt bg kind)))
              (when use-blocked
                (setf (agent-shell-queue-item-status item) 'blocked.skip))
              (setf (agent-shell-queue-item-directory item)
                    (buffer-local-value 'default-directory buf))
              (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
              (agent-shell-queue--ensure-subscription buf)))
          (unless use-blocked
            (agent-shell-queue--send-next-for-buffer buf)))
         (t
          (with-agent-shell-queue
            (let ((item (agent-shell-queue--make-item prompt bg kind)))
              (when use-blocked
                (setf (agent-shell-queue-item-status item) 'blocked.skip))
              (agent-shell-queue--add-item-to-bucket agent-shell-queue--unassigned-key item)))))))))

(defun agent-shell-queue-capture-cancel ()
  "Discard the capture buffer without queuing."
  (interactive)
  (agent-shell-queue--close-capture-window))

(defun agent-shell-queue-capture-save-draft ()
  "Save capture buffer contents as a draft queue item without closing.
The item is stored with `draft' status and skipped by dispatch.
If a draft was previously saved from this buffer it is updated in place."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task))
    (when (string-empty-p prompt)
      (user-error "Buffer is empty — nothing to save as draft"))
    (if-let* ((draft-id agent-shell-queue--capture-draft-id)
              (_ (agent-shell-queue--item-by-id draft-id)))
        (progn
          (agent-shell-queue-edit draft-id prompt)
          (message "agent-shell-queue: draft updated (%s)" draft-id))
      (let* ((item (agent-shell-queue--make-item prompt bg))
             (bucket-name (if buf (buffer-name buf) agent-shell-queue--unassigned-key)))
        (setf (agent-shell-queue-item-status item) 'draft)
        (agent-shell-queue--ensure-loaded)
        (agent-shell-queue--add-item-to-bucket bucket-name item)
        (when buf (agent-shell-queue--ensure-subscription buf))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (setq agent-shell-queue--capture-draft-id (agent-shell-queue-item-id item))
        (message "agent-shell-queue: draft saved (%s)" agent-shell-queue--capture-draft-id)))))

(defun agent-shell-queue-capture-enable-background-task ()
  "Flag this capture for background sub-agent execution."
  (interactive)
  (setq agent-shell-queue--capture-background-task t)
  (message "Background: on"))

(defun agent-shell-queue-capture-disable-background-task ()
  "Clear the background sub-agent flag from this capture."
  (interactive)
  (setq agent-shell-queue--capture-background-task nil)
  (message "Background: off"))

(defun agent-shell-queue-capture-yank-kill ()
  "Insert the most recent kill-ring entry at point."
  (interactive)
  (when kill-ring (insert (car kill-ring))))

(defun agent-shell-queue-capture-yank-clipboard ()
  "Insert the current clipboard contents at point."
  (interactive)
  (when-let* ((sel (ignore-errors (gui-get-selection 'CLIPBOARD))))
    (insert sel)))

(defun agent-shell-queue-capture-insert-thing-at-point ()
  "Insert the thing at point from the buffer that opened this capture."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (thing (with-current-buffer origin
                       (or (thing-at-point 'url t)
                           (thing-at-point 'filename t)
                           (thing-at-point 'symbol t)
                           (thing-at-point 'word t)))))
    (insert thing)))

(defun agent-shell-queue-capture-select-context ()
  "Select a string from the origin buffer's context via completing-read and insert it."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (text (with-current-buffer origin
                      (annotated-completing-read-context-from-point
                       :prompt "insert context: "
                       :history 'agent-shell-queue-capture-select-context)))
              (_ (not (string-empty-p text))))
    (insert text)))

(defun agent-shell-queue-insert-file ()
  "Prompt for a file and insert its contents at point.
Works in both capture and edit buffers."
  (interactive)
  (let ((file (read-file-name "Insert file: ")))
    (when (file-readable-p file)
      (insert-file-contents file))))

(defun agent-shell-queue-insert-buffer ()
  "Pick a buffer and insert its entire contents at point.
Works in both capture and edit buffers."
  (interactive)
  (when-let* ((name (annotated-completing-read
		     (map-into
		      (seq-map (lambda (buf)
				 (cons (buffer-name buf)
				       (with-current-buffer buf
					 (format "%-20s %s"
						 (symbol-name major-mode)
						 (or (buffer-file-name) "")))))
			       (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b)))
					   (buffer-list)))
		      '(hash-table :test equal))
                     :prompt "Insert buffer: "
                     :require-match t))
              (buf (get-buffer name)))
    (insert (with-current-buffer buf (buffer-string)))))

;;;###autoload
(defun agent-shell-queue-enqueue (prompt &optional buf background delay-before delay-after)
  "Queue PROMPT for BUF, optionally flagged for BACKGROUND sub-agent execution.
Send immediately if BUF is idle, otherwise store in the queue.
When called interactively, opens a capture buffer for composing the prompt.
Optional DELAY-BEFORE and DELAY-AFTER specify per-task pre-dispatch and post-completion delays."
  (interactive
   (let ((target (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                     (agent-shell-queue--pick-buffer "Enqueue to: "))))
     (agent-shell-queue--open-capture target (current-buffer))
     (list nil nil nil)))
  (when-let* ((buf (and prompt
                        (or buf
                            (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                            (agent-shell-queue--pick-buffer "Enqueue to: ")))))
    (let* ((buf-name (buffer-name buf))
           (item (agent-shell-queue--make-item prompt background 'prompt delay-before delay-after)))
      (with-current-buffer buf
        (if (shell-maker-busy)
            (agent-shell-queue-add prompt buf background)
          (agent-shell-insert
           :text (if background
                     (concat (agent-shell-queue--get-background-prefix buf) prompt)
                   prompt)
           :submit t
           :no-focus t))))))
;;;###autoload
(defun agent-shell-queue-enqueue-clear (&optional buf)
  "Enqueue a clear command for BUF.
Uses the resolved clear command for BUF as the prompt."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Clear queue for: "))))
  (agent-shell-queue-enqueue (agent-shell-queue--get-clear-command buf) buf))
;;;###autoload
(defun agent-shell-queue-capture-unassigned ()
  "Open a capture buffer to compose a prompt for the unassigned queue.
Unassigned items display in blue and can later be assigned to a shell via `t'."
  (interactive)
  (agent-shell-queue--open-capture nil (current-buffer)))

(defun agent-shell-queue-buffer-capture-after ()
  "Open a capture buffer for an item to be inserted after the item at point.
The confirmed item is spliced into the queue immediately after the current row,
rather than appended to the end."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id)))
    (let ((bucket-name (car pair)))
      (with-current-buffer
          (agent-shell-queue--open-capture
           (unless (equal bucket-name agent-shell-queue--unassigned-key)
             (get-buffer bucket-name))
           (current-buffer))
        (setq agent-shell-queue--capture-after-id id)))))


;;;###autoload
(defun agent-shell-queue-capture-from-region (&optional buf)
  "Open a capture buffer pre-seeded with the active region text.
When no region is active, opens an empty capture.  BUF is the target
agent-shell buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture buf (current-buffer)
                                   (when (use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end)))))

;;;###autoload
(defun agent-shell-queue-capture-from-context (&optional buf)
  "Open a capture buffer pre-seeded with a string selected from context.
Candidates include thing-at-point, the active region, the current line, and
the kill ring.  BUF is the target agent-shell buffer; nil for the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))

  (let ((text (annotated-completing-read-context-from-point
               :prompt "seed capture: "
               :history 'agent-shell-queue-capture-from-context)))
    (agent-shell-queue--open-capture
     buf (current-buffer)
     (unless (string-empty-p text)
       text))))

;;;###autoload
(defun agent-shell-queue-capture-from-clipboard (&optional buf)
  "Open a capture buffer pre-seeded with the current clipboard contents.
BUF is the target agent-shell buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture
   buf (current-buffer)
   (ignore-errors
     (gui-get-selection 'CLIPBOARD))))

;;; Raw edit mode

(defvar agent-shell-queue-raw-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-raw-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-raw-edit-cancel)
    m)
  "Keymap for `agent-shell-queue-raw-edit-mode'.")

(define-derived-mode agent-shell-queue-raw-edit-mode text-mode "Queue-RawEdit"
  "Mode for directly editing the queue in YAML format.
Every session not already paused is paused while this buffer is live.
Confirm with \\[agent-shell-queue-raw-edit-confirm], cancel with \\[agent-shell-queue-raw-edit-cancel].
\\{agent-shell-queue-raw-edit-mode-map}")

(defvar-local agent-shell-queue--raw-edit-snapshot nil
  "Hash-table of id→item for the queue state when raw edit was started.")

(defvar-local agent-shell-queue--raw-edit-newly-paused nil
  "Buffer names added to the session-paused list when raw edit was started.
Only these are removed again on confirm/cancel, so a buffer that was already
individually paused before raw edit began stays paused afterward.")

(defun agent-shell-queue--item-to-yaml-edit (item)
  "Convert ITEM to a hash-table for raw editing; omits nil timestamp fields."
  (map-into
   (append
    (list (cons "id" (agent-shell-queue-item-id item))
          (cons "prompt" (agent-shell-queue-item-args item))
          (cons "status" (symbol-name (agent-shell-queue-item-status item)))
          (cons "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
          (cons "background" (if (agent-shell-queue-item-background item) t nil))
          (cons "created" (agent-shell-queue-item-created item)))
    (when (agent-shell-queue-item-dispatched item)
      (list (cons "dispatched" (agent-shell-queue-item-dispatched item))))
    (when (agent-shell-queue-item-completed item)
      (list (cons "completed" (agent-shell-queue-item-completed item))))
    (when (agent-shell-queue-item-directory item)
      (list (cons "directory" (agent-shell-queue-item-directory item)))))
   '(hash-table :test equal)))

(defun agent-shell-queue--render-to-yaml ()
  "Render active/deferred queue items to a YAML string for raw editing."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  ;; if-let*
  (let ((buckets (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                              (seq-map (lambda (pair)
                                         (when-let* ((items (seq-remove
                                                             (lambda (item)
                                                               (memq (agent-shell-queue-item-status item)
                                                                     '(done running)))
                                                             (cdr pair))))
                                           (map-into (list (cons "buffer" (car pair))
                                                           (cons "items"
                                                                 (vconcat (seq-map #'agent-shell-queue--item-to-yaml-edit items))))
                                                     '(hash-table :test equal)))))
                              (seq-remove #'null))))
    (if buckets
	(yaml-encode (vconcat buckets))
      "")))

(defun agent-shell-queue--make-edit-snapshot ()
  "Return a hash-table mapping item ID to item struct for all current items."
  (map-into
   (seq-map (lambda (it) (cons (agent-shell-queue-item-id it) it))
            (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store)))
   '(hash-table :test equal)))

;;;###autoload
(defun agent-shell-queue-raw-edit ()
  "Open the queue for direct YAML editing.
Every session not already paused is paused while the edit buffer is live.
Confirm changes with \\[agent-shell-queue-raw-edit-confirm];
cancel with \\[agent-shell-queue-raw-edit-cancel]."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (unless (fboundp 'yaml-encode)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((buf (get-buffer-create "*agent-shell-queue-raw-edit*"))
         (newly-paused
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-map #'car)
                       (seq-remove (lambda (name)
                                     (member name (agent-shell-queue-queue-session-paused
                                                   agent-shell-queue--queue)))))))
    (seq-do #'agent-shell-queue--session-pause-name newly-paused)
    (agent-shell-queue--refresh-buffer)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-raw-edit-mode)
        (setq agent-shell-queue--raw-edit-snapshot (agent-shell-queue--make-edit-snapshot)
              agent-shell-queue--raw-edit-newly-paused newly-paused)
        (insert (agent-shell-queue--render-to-yaml))))
    (pop-to-buffer buf '(display-buffer-below-selected (window-height . 0.5)))))

(defun agent-shell-queue--raw-edit-fail (text errors)
  "Save TEXT to a timestamped fail file, report ERRORS, leave queue paused."
  (let ((file (expand-file-name
               (format "agent-shell-queue-edit-failed-%s.yaml" (format-time-string "%Y%m%dT%H%M%S"))
               (file-name-directory (agent-shell-queue--state-file)))))
    (with-temp-file file
      (insert text))
    (seq-do (lambda (it) (message "agent-shell-queue raw edit: %s" it))
            (nreverse errors))
    (message "agent-shell-queue: %d error(s) — buffer saved to %s (queue remains paused)"
             (length errors) file)))

(defun agent-shell-queue--parse-yaml-item (item-h snapshot)
  "Validate hash-table ITEM-H against SNAPSHOT; return (item . errors) or (nil . errors)."
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

(cl-defun agent-shell-queue-raw-edit-confirm ()
  "Validate and apply the YAML in this raw-edit buffer."
  (interactive)
  (unless (fboundp 'yaml-parse-string)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((text (buffer-string))
         (snapshot agent-shell-queue--raw-edit-snapshot)
         (newly-paused agent-shell-queue--raw-edit-newly-paused)
         errors
         parsed)
    (condition-case err
        (setq parsed (yaml-parse-string text
                                        :object-type 'hash-table
                                        :sequence-type 'list
                                        :null-object nil
                                        :false-object nil))
      (error (push (format "YAML parse error: %s" (cadr err)) errors)
             (cl-return-from agent-shell-queue-raw-edit-confirm
               (agent-shell-queue--raw-edit-fail text errors))))
    (let ((all-ids nil)
          (new-buckets nil))
      (thread-last (agent-shell-queue--yaml-buckets parsed)
        (seq-filter #'hash-table-p)
        (seq-do (lambda (bucket)
                  (let* ((buf-name (map-elt bucket "buffer"))
                         (items-raw (map-elt bucket "items"))
                         (items-list (if (vectorp items-raw)
                                         (append items-raw nil)
                                       items-raw))
                         (bucket-items))
                    (unless buf-name
                      (push "a bucket is missing the 'buffer' field" errors))
                    (seq-do (lambda (item-h)
                              (let ((id (map-elt item-h "id")))
                                (when (and id (member id all-ids))
                                  (push (format "duplicate ID '%s'" id) errors))
                                (when id (push id all-ids))
                                (let ((result (agent-shell-queue--parse-yaml-item item-h snapshot)))
                                  (if (cdr result)
                                      (setq errors (append errors (cdr result)))
                                    (push (car result) bucket-items)))))
                            (seq-filter #'hash-table-p items-list))
                    (when (and buf-name bucket-items)
                      (push (cons buf-name (nreverse bucket-items)) new-buckets))))))
      (when errors
        (cl-return-from agent-shell-queue-raw-edit-confirm
          (agent-shell-queue--raw-edit-fail text errors)))
      ;; Preserve running/done items from current queue
      (let ((preserved (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                                    (seq-map (lambda (it)
                                               (let ((kept (seq-filter
                                                            (lambda (item)
                                                              (memq (agent-shell-queue-item-status item)
                                                                    '(running done)))
                                                            (cdr it))))
                                                 (when kept (cons (car it) kept)))))
                                    (seq-remove #'null))))
        (let ((result (nreverse new-buckets)))
          (seq-do (lambda (it)
                    (if-let* ((cell (assoc (car it) result)))
                        (setcdr cell (append (cdr cell) (cdr it)))
                      (push it result)))
                  preserved)
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (seq-remove #'agent-shell-queue--bucket-empty-p result))))
      (seq-do #'agent-shell-queue--session-unpause-name newly-paused)
      (agent-shell-queue--save)
      (quit-window t)
      (agent-shell-queue--refresh-buffer)
      (message "agent-shell-queue: raw edit applied%s"
               (if newly-paused
                   (format " (%d session(s) resumed)" (length newly-paused))
                 "")))))

(defun agent-shell-queue-raw-edit-cancel ()
  "Cancel raw edit; resume exactly the sessions this raw edit newly paused."
  (interactive)
  (let ((newly-paused agent-shell-queue--raw-edit-newly-paused))
    (quit-window t)
    (seq-do #'agent-shell-queue--session-unpause-name newly-paused)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: raw edit cancelled%s"
             (if newly-paused
                 (format " (%d session(s) resumed)" (length newly-paused))
               ""))))

;;; Import

;;;###autoload
(defun agent-shell-queue-import (&optional source)
  "Import queue items from YAML.
With no prefix arg reads from clipboard; with prefix arg prompts for a file.
For each item whose ID already exists, prompts to keep, replace, or assign new ID."
  (interactive (list (if current-prefix-arg 'file 'clipboard)))
  (unless (fboundp 'yaml-parse-string)
    (user-error "Import requires the `yaml' package"))
  (agent-shell-queue--ensure-loaded)
  (let* ((text
          (if (eq source 'file)
              (let ((f (read-file-name "Import YAML from file: ")))
                (with-temp-buffer (insert-file-contents f) (buffer-string)))
            (or (ignore-errors (gui-get-selection 'CLIPBOARD))
                (user-error "Clipboard is empty"))))
         (parsed
          (condition-case err
              (yaml-parse-string text
                                 :object-type 'hash-table
                                 :sequence-type 'list
                                 :null-object nil
                                 :false-object nil)
            (error (user-error "YAML parse error: %s" (cadr err)))))
         (added 0)
         (skipped 0))
    (thread-last (agent-shell-queue--yaml-buckets parsed)
      (seq-filter #'hash-table-p)
      (seq-do (lambda (bucket)
                (let* ((buf-name (map-elt bucket "buffer"))
                       (items-raw (map-elt bucket "items"))
                       (items-list (cond
                                    ((vectorp items-raw) (append items-raw nil))
                                    ((listp items-raw) items-raw)
                                    (t nil))))
                  (seq-do (lambda (it)
                            (let* ((raw-id (map-elt it "id"))
                                   (existing (and raw-id (agent-shell-queue--item-by-id raw-id)))
                                   (final-id
                                    (cond
                                     ((null raw-id) (agent-shell-queue--gen-id))
                                     ((null existing) raw-id)
                                     (t (let ((choice (completing-read
                                                       (format "ID '%s' exists — " raw-id)
                                                       '("keep existing (skip)"
                                                         "replace existing"
                                                         "assign new ID")
                                                       nil t)))
                                          (cond
                                           ((string-prefix-p "keep" choice) 'skip)
                                           ((string-prefix-p "replace" choice) raw-id)
                                           (t (agent-shell-queue--gen-id))))))))
                              (cond
                               ((eq final-id 'skip) (cl-incf skipped))
                               (t
                                (when (and (stringp final-id) (equal final-id raw-id) existing)
                                  (agent-shell-queue-remove raw-id))
                                (let* ((prompt (or (map-elt it "args") (map-elt it "prompt") ""))
                                       (status-str (map-elt it "status" "active"))
                                       (status (condition-case nil (intern status-str) (error 'active)))
                                       (kind-str (map-elt it "kind" "prompt"))
                                       (kind (condition-case nil (intern kind-str) (error 'prompt)))
                                       (bg (eq t (map-elt it "background")))
                                       (target-buf (and buf-name
                                                        (not (equal buf-name agent-shell-queue--unassigned-key))
                                                        (get-buffer buf-name)))
                                       (item (agent-shell-queue-item--make
                                              :id final-id
                                              :args (if (string-empty-p (string-trim (or prompt "")))
                                                        "(imported)" (string-trim prompt))
                                              :status (if (or (memq status '(active invalid))
                                                              (agent-shell-queue--blocked-status-p status))
                                                          status 'active)
                                              :kind (if (memq kind '(prompt pause context emacs wait compact)) kind 'prompt)
                                              :background bg
                                              :created (or (map-elt it "created") (float-time)))))
                                  (when target-buf
                                    (agent-shell-queue--ensure-subscription target-buf))
                                  (agent-shell-queue--add-item-to-bucket
                                   (if (and buf-name (not (string-empty-p buf-name)))
                                       buf-name
                                     agent-shell-queue--unassigned-key)
                                   item)
                                  (cl-incf added))))))
                          (seq-filter #'hash-table-p items-list))))))
    (when (> added 0)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer))
    (let ((skip-note (if (> skipped 0) (format " (%d skipped)" skipped) "")))
      (message "agent-shell-queue: imported %d item(s)%s" added skip-note))))


;;; Fork Queue Operations

(defface agent-shell-queue-pending-fork-face
  '((t :foreground "mediumpurple3" :slant italic))
  "Face for queue items held pending a fork operation.")

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
  "Create a new agent-shell session and return the new buffer.
SOURCE-BUF is the session being forked (used for directory and fork mode).
FORK-MODE is `new' (create via `agent-shell-new-shell') or `fork' (via `agent-shell-fork').
TARGET-DIR, when non-nil, overrides the working directory for the new session.
Returns the newly created buffer on success, nil if none could be detected."
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
  "Return an Emacs Lisp form string for a fork-queue emacs item.
BUF-NAME is the source session; OPTS is the fork options plist.
The form calls `agent-shell-queue--fork-session-from-running-emacs' at
dispatch time to dynamically determine which items to fork."
  (format "(agent-shell-queue--fork-session-from-running-emacs %S %S)"
          buf-name opts))

(defun agent-shell-queue--fork-session-from-running-emacs (buf-name opts)
  "Fork items after the currently-running emacs item in BUF-NAME's queue.
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
  "Fork the queue for SOURCE-BUF starting at FROM-ID into a new agent-shell session.

Items at or after FROM-ID (by queue position among active/deferred/draft items)
are moved to the new session.  When FROM-ID is nil, all eligible items are
moved.  The original session is paused during session creation.

OPTS is a plist with these keys:
  :fork-mode       Symbol `new' (default) or `fork' — how to create the new session.
                   `new' calls `agent-shell-new-shell'; `fork' calls `agent-shell-fork'.
  :use-worktree    Non-nil — create a git worktree for the new session.
  :worktree-path   String — explicit worktree path (auto-generated when nil).
  :worktree-branch String — new branch name for the worktree.
  :capture-pending Non-nil — mark items at/after FROM-ID as `pending-fork' in the
                   original session instead of moving them, then leave the session
                   paused so new items can be inserted before the frozen ones."
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
      (user-error "agent-shell-queue: no eligible items to fork in %s" source-name))
    ;; Pause source session while we create the new one.
    (cl-pushnew source-name
                (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                :test #'equal)
    (unwind-protect
        (let* ((target-dir (when use-worktree
                             (agent-shell-queue--fork-create-worktree
                              source-buf worktree-branch worktree-path)))
               (_ (when (and use-worktree (null target-dir))
                    (user-error "agent-shell-queue: worktree creation failed")))
               (new-buf (agent-shell-queue--fork-create-session
                         source-buf fork-mode target-dir)))
          (unless new-buf
            (user-error "agent-shell-queue: could not detect new session after creation"))
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
BUF defaults to the current agent-shell session when called from one."
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
When the fork task is dispatched (as an emacs item), it forks the queue
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

(defun agent-shell-queue--fork-build-opts ()
  "Build fork options plist interactively using annotated-completing-read.
Prompts for fork mode, worktree settings, and capture-pending flag.
Returns a plist suitable for `agent-shell-queue-fork-session' or nil to abort."
  (let* ((mode-choice (annotated-completing-read
                       '(("new session" . "Create a clean new session via agent-shell-new-shell")
                         ("fork session (ACP)" . "Fork via agent-shell-fork (preserves context)"))
                       :prompt "fork mode: "
                       :category 'agent-shell-fork-mode
                       :require-match t
                       :history 'agent-shell-queue-fork-mode))
         (fork-mode (if (equal mode-choice "fork session (ACP)") 'fork 'new))
         (wt-choice (annotated-completing-read
                     '(("no worktree" . "New session opens in the same working directory")
                       ("create worktree" . "Run git worktree add and open the session in the new tree"))
                     :prompt "worktree: "
                     :category 'agent-shell-fork-worktree
                     :require-match t
                     :history 'agent-shell-queue-fork-worktree))
         (use-worktree (equal wt-choice "create worktree"))
         (worktree-branch (when use-worktree
                            (let ((b (read-string "Branch name (empty = auto): ")))
                              (unless (string-empty-p b) b))))
         (worktree-path (when use-worktree
                          (let ((p (read-string "Worktree path (empty = auto): ")))
                            (unless (string-empty-p p) p))))
         (cp-choice (annotated-completing-read
                     '(("move items to new session" . "Items are moved; original session resumes automatically")
                       ("capture pending (freeze & pause)" . "Items stay in original session as pending-fork; session stays paused"))
                     :prompt "after fork: "
                     :category 'agent-shell-fork-capture
                     :require-match t
                     :history 'agent-shell-queue-fork-capture))
         (capture-pending (equal cp-choice "capture pending (freeze & pause)")))
    (list :fork-mode fork-mode
          :use-worktree use-worktree
          :worktree-branch worktree-branch
          :worktree-path worktree-path
          :capture-pending capture-pending)))

;;;###autoload
(defun agent-shell-queue-buffer-fork ()
  "Fork the queue starting at the item at point into a new session.
Prompts interactively for fork options; uses annotated-completing-read when
called outside the queue buffer to build options without task-at-point context."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Fork session: ")))
         (from-id (when pair id))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-fork-session buf from-id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-before ()
  "Insert a fork queue item before the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-before in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-before buf id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-after ()
  "Insert a fork queue item after the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-after in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-after buf id opts)))

;;; File sending to capture buffers

(defun agent-shell-queue--live-capture-buffers ()
  "Return all live buffers in `agent-shell-queue-capture-mode'."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'agent-shell-queue-capture-mode)))
              (buffer-list)))

(defun agent-shell-queue--ad:agent-shell-send-file-to (orig-fn &optional prompt-for-file)
  "Around advice: include open queue capture buffers in the target picker.
When a capture buffer is chosen, the file context is inserted at point-max
of that buffer instead of being sent to an agent-shell via `agent-shell-insert'."
  (let* ((capture-bufs (agent-shell-queue--live-capture-buffers))
         (shell-names (seq-map #'buffer-name (agent-shell-buffers)))
         (capture-names (seq-map #'buffer-name capture-bufs))
         (all-names (append shell-names capture-names)))
    (cond
     ((null capture-bufs)
      ;; No open capture buffers — delegate unchanged.
      (funcall orig-fn prompt-for-file))
     ((null all-names)
      (user-error "No shells or capture buffers available"))
     (t
      (let* ((chosen-name (completing-read "Send file to: " all-names nil t))
             (chosen-buf (get-buffer chosen-name)))
        (if (and chosen-buf
                 (with-current-buffer chosen-buf
                   (derived-mode-p 'agent-shell-queue-capture-mode)))
            ;; Capture buffer: intercept agent-shell-insert and insert there.
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (when-let* ((text (plist-get args :text)))
                           (with-current-buffer chosen-buf
                             (goto-char (point-max))
                             (unless (bolp) (insert "\n"))
                             (insert text))))))
              (agent-shell-send-file prompt-for-file nil))
          ;; Regular shell: pre-select via completing-read intercept.
          (cl-letf* ((real-cr (symbol-function 'completing-read))
                     ((symbol-function 'completing-read)
                      (lambda (prompt collection &rest args)
                        (if (string-match-p "[Ss]hell" prompt)
                            chosen-name
                          (apply real-cr prompt collection args)))))
            (funcall orig-fn prompt-for-file))))))))

(advice-add 'agent-shell-send-file-to :around
            #'agent-shell-queue--ad:agent-shell-send-file-to)

;;; Interjection

(defvar agent-shell-queue-interjection-continuation-suffix
  "\n\nAfter addressing the above, please resume your previous task where you left off."
  "Text appended to the interjection prompt before sending.
Set to nil to send the user's text verbatim without a continuation instruction.")

(defvar-local agent-shell-queue-interjection--item nil
  "The queue item being interject-edited in this capture buffer.")

(defvar-local agent-shell-queue-interjection--shell nil
  "The agent-shell buffer associated with this interjection capture buffer.")

(defvar agent-shell-queue-interjection-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-interjection-send)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-interjection-close)
    (define-key m (kbd "q")       #'agent-shell-queue-interjection-close)
    m)
  "Keymap for `agent-shell-queue-interjection-mode'.")

(define-derived-mode agent-shell-queue-interjection-mode text-mode "ASQ-Interject"
  "Capture mode for interjection messages.
C-c C-c to send, C-c C-k / q to close or abort.")

(defun agent-shell-queue--open-interjection-buffer (item buf-name)
  "Create and display an interjection capture buffer for ITEM in BUF-NAME shell."
  (let* ((shell-buf (get-buffer buf-name))
         (capture-buf (generate-new-buffer "*asq-interjection*")))
    (with-current-buffer capture-buf
      (setq-local agent-shell-queue-interjection--item item)
      (setq-local agent-shell-queue-interjection--shell shell-buf)
      (let ((inhibit-read-only t))
        (insert (format "Interjecting: %s — %s\n"
                        (agent-shell-queue-item-id item)
                        (car (split-string (agent-shell-queue-item-args item) "\n"))))
        (insert "Original prompt (truncated):\n")
        (seq-do (lambda (line) (insert "  " line "\n"))
                (seq-take (split-string (agent-shell-queue-item-args item) "\n") 3))
        (insert (make-string 60 ?─) "\n\n")
        (add-text-properties (point-min) (point) '(read-only t front-sticky (read-only))))
      (goto-char (point-max))
      (agent-shell-queue-interjection-mode))
    (pop-to-buffer capture-buf)))

(defun agent-shell-queue--interjection-readable-prompt (raw-text)
  "Return the editable user portion of RAW-TEXT in the interjection buffer.
Strips the read-only header by looking for the separator line."
  (let ((sep (make-string 60 ?─)))
    (if (string-match (concat (regexp-quote sep) "\n\n?") raw-text)
        (substring raw-text (match-end 0))
      raw-text)))

(defun agent-shell-queue-interjection-send ()
  "Send the interjection message to the agent shell and close this buffer."
  (interactive)
  (let* ((item agent-shell-queue-interjection--item)
         (shell-buf agent-shell-queue-interjection--shell)
         (buf-name (buffer-name shell-buf))
         (raw (buffer-substring-no-properties (point-min) (point-max)))
         (user-text (string-trim (agent-shell-queue--interjection-readable-prompt raw)))
         (full-text (if (and agent-shell-queue-interjection-continuation-suffix
                             (not (string-empty-p user-text)))
                        (concat user-text agent-shell-queue-interjection-continuation-suffix)
                      user-text)))
    (when (string-empty-p user-text)
      (user-error "Interjection prompt is empty — type a message or use C-c C-k to close"))
    (setf (agent-shell-queue-item-interjection-prompt item) user-text)
    ;; Remove any stale response-start entry from the original dispatch, then
    ;; track the new start position so --capture-response finds the right text.
    (setq agent-shell-queue--response-start-positions
          (seq-remove (lambda (it) (equal (car it) (agent-shell-queue-item-id item)))
                      agent-shell-queue--response-start-positions))
    (agent-shell-insert :text full-text :submit t :no-focus t :shell-buffer shell-buf)
    (push (cons (agent-shell-queue-item-id item)
                (with-current-buffer shell-buf (point-max)))
          agent-shell-queue--response-start-positions)
    (kill-buffer (current-buffer))
    (message "agent-shell-queue: interjection sent to %s — waiting for response…" buf-name)))

(defun agent-shell-queue--interjection-mark-aborted (item buf-name)
  "Mark ITEM as aborted due to interjection abort, clear interjection-pending."
  (setf (agent-shell-queue-item-status item) 'aborted)
  (setf (agent-shell-queue-item-completed item) (float-time))
  (setf (agent-shell-queue-item-outcome item) 'interrupted)
  (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) nil)
  ;; Remove any stale response-start position.
  (setq agent-shell-queue--response-start-positions
        (seq-remove (lambda (it) (equal (car it) (agent-shell-queue-item-id item)))
                    agent-shell-queue--response-start-positions))
  (agent-shell-queue--append-done-log buf-name item)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-interjection-close ()
  "Close the interjection buffer with a choice of how to handle the interrupted task."
  (interactive)
  (let* ((item agent-shell-queue-interjection--item)
         (shell-buf agent-shell-queue-interjection--shell)
         (buf-name (buffer-name shell-buf))
         (choice
          (annotated-completing-read
           '(("resume previous work"
              . "Send 'please continue your previous task' to the agent and close")
             ("mark done and continue"
              . "Mark item done without result; clear pause; dispatch next item")
             ("mark done and pause"
              . "Mark item done without result; leave queue paused")
             ("clear context and continue"
              . "Mark item aborted; clear session pause; let next item start fresh")
             ("insert resume task"
              . "Mark item aborted; queue a blocked resume task; stay paused"))
           :prompt "Interjection abort: "
           :require-match t)))
    (pcase choice
      ("resume previous work"
       (setf (agent-shell-queue-item-interjection-prompt item) "")
       (agent-shell-insert
        :text "Please resume your previous task where you left off."
        :submit t :no-focus t :shell-buffer shell-buf)
       (push (cons (agent-shell-queue-item-id item)
                   (with-current-buffer shell-buf (point-max)))
             agent-shell-queue--response-start-positions)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: asking agent to resume previous work in %s…" buf-name))
      ("mark done and continue"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--session-unpause-name buf-name)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (when-let* ((buf (get-buffer buf-name)))
         (agent-shell-queue--send-next-for-buffer buf))
       (message "agent-shell-queue: item aborted, queue continuing in %s" buf-name))
      ("mark done and pause"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: item aborted, queue paused in %s" buf-name))
      ("clear context and continue"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--session-unpause-name buf-name)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (when-let* ((buf (get-buffer buf-name)))
         (agent-shell-queue--send-next-for-buffer buf))
       (message "agent-shell-queue: item aborted (clear context), queue continuing in %s" buf-name))
      ("insert resume task"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--insert-resume-task buf-name item)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: resume task inserted in %s (queue remains paused)" buf-name)))))

(defun agent-shell-queue-interject-available-p ()
  "Return non-nil when `agent-shell-queue-interject' can be called.
True when queue data is loaded, a task is running or interjecting, and no
interjection buffer is already pending."
  (condition-case nil
    (and agent-shell-queue--queue
         (not (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue))
         (seq-find (lambda (item)
                     (memq (agent-shell-queue-item-status item) '(running interjecting)))
                   (seq-mapcat #'cdr
                     (agent-shell-queue-store-items agent-shell-queue--store))))
    (args-out-of-range nil)))

(defun agent-shell-queue-interject ()
  "Interrupt the currently running queue task and open an interjection buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((running-item
          (seq-find (lambda (item)
                      (memq (agent-shell-queue-item-status item) '(running interjecting)))
                    (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store))))
         (buf-name (when running-item
                     (car (agent-shell-queue--item-by-id
                           (agent-shell-queue-item-id running-item))))))
    (unless running-item
      (user-error "No running item to interject"))
    (when (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue)
      (user-error "An interjection is already in progress"))
    (setf (agent-shell-queue-item-status running-item) 'interjecting)
    (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) t)
    (when-let* ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (agent-shell-interrupt)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue--open-interjection-buffer running-item buf-name)))

;;; Queue-only mode

(defcustom agent-shell-queue-input-mode-default 'default
  "Default input routing mode for new agent-shell sessions.
One of `default' (normal shell input), `queue-intercept' (capture user
input as queue items while still submitting), or `queue-only' (no prompt;
all input routed through the queue).
Use `agent-shell-queue-set-input-mode-default' to change this and sync
all existing sessions simultaneously."
  :type '(choice (const :tag "Normal (direct input)" default)
                 (const :tag "Queue intercept (capture to queue)" queue-intercept)
                 (const :tag "Queue only (all input via queue)" queue-only))
  :group 'agent-shell-queue)

(defun agent-shell-queue--apply-input-mode-default ()
  "Apply `agent-shell-queue-input-mode-default' to the current buffer.
Installed on `agent-shell-mode-hook'."
  (unless (eq agent-shell-queue-input-mode-default 'default)
    (agent-shell-queue-set-input-mode agent-shell-queue-input-mode-default)))

(add-hook 'agent-shell-mode-hook #'agent-shell-queue--apply-input-mode-default)

;;;###autoload
(defun agent-shell-queue-set-input-mode-default (mode)
  "Set `agent-shell-queue-input-mode-default' to MODE and sync all existing sessions.
MODE is prompted interactively from the three valid options.
All live agent-shell buffers are immediately updated to the new default."
  (interactive
   (list (intern (completing-read "Input mode default: "
                                  '("default" "queue-intercept" "queue-only")
                                  nil t nil nil "default"))))
  (unless (memq mode '(default queue-intercept queue-only))
    (user-error "Invalid mode %s: expected default, queue-intercept, or queue-only" mode))
  (setq agent-shell-queue-input-mode-default mode)
  (let ((bufs (agent-shell-buffers)))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode mode)))
            bufs)
    (when bufs (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: input mode default → %s (%d session(s) updated)"
             mode (length bufs))))

;;;###autoload
(defun agent-shell-queue-reset-all-input-modes ()
  "Reset all live agent-shell buffers to default input mode."
  (interactive)
  (let ((changed (seq-filter (lambda (buf)
                               (not (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                        'default)))
                             (agent-shell-buffers))))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode 'default)))
            changed)
    (when changed (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: reset %d session(s) to default" (length changed))))

;;;###autoload
(defun agent-shell-queue-toggle-intercept-default ()
  "Toggle queue-intercept as the default input mode and sync all sessions."
  (interactive)
  (agent-shell-queue-set-input-mode-default
   (if (eq agent-shell-queue-input-mode-default 'queue-intercept) 'default 'queue-intercept)))

;;;###autoload
(defun agent-shell-queue-toggle-only-default ()
  "Toggle queue-only as the default input mode and sync all sessions."
  (interactive)
  (agent-shell-queue-set-input-mode-default
   (if (eq agent-shell-queue-input-mode-default 'queue-only) 'default 'queue-only)))

(defface agent-shell-queue-ready-face
  '((t :foreground "red" :weight bold))
  "Face for the <ready> indicator shown in `agent-shell-queue-only-mode'.")

(defvar-local agent-shell-queue--ready-overlay nil)
(defvar-local agent-shell-queue--ready-sub-prompt nil)
(defvar-local agent-shell-queue--ready-sub-submit nil)

(defun agent-shell-queue--ready-clear ()
  (when (overlayp agent-shell-queue--ready-overlay)
    (delete-overlay agent-shell-queue--ready-overlay)
    (setq agent-shell-queue--ready-overlay nil)))

(defun agent-shell-queue--ready-show (_event)
  (when (and agent-shell-queue-only-mode
             (derived-mode-p 'agent-shell-mode))
    (agent-shell-queue--ready-clear)
    (when-let* ((proc (get-buffer-process (current-buffer)))
                (pmark (process-mark proc)))
      (let ((inhibit-read-only t))
        (delete-region (marker-position pmark) (point-max))))
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'after-string
                   (propertize "<ready>" 'face 'agent-shell-queue-ready-face))
      (overlay-put ov 'agent-shell-queue-ready t)
      (setq agent-shell-queue--ready-overlay ov))))

(defun agent-shell-queue--ready-hide (_event)
  (agent-shell-queue--ready-clear))

;;;###autoload
(defun agent-shell-queue-ready-capture ()
  "Clear the ready overlay and open the queue enqueue dispatch menu.
Any keypress in queue-only mode at the idle prompt routes here, giving
access to all registered item kinds rather than prompt-only capture."
  (interactive)
  (agent-shell-queue--ready-clear)
  (call-interactively #'agent-shell-queue-enqueue-dispatch))

(defvar-keymap agent-shell-queue-only-mode-map
  "SPC" #'agent-shell-queue-ready-capture
  "RET" #'agent-shell-queue-ready-capture
  "<remap> <self-insert-command>" #'agent-shell-queue-ready-capture)

;;;###autoload
(define-minor-mode agent-shell-queue-only-mode
  "Route all agent-shell input through the queue; show <ready> when idle."
  :lighter " Q⌛"
  :keymap agent-shell-queue-only-mode-map
  (if agent-shell-queue-only-mode
      (progn
        (setq-local buffer-read-only t)
        (setq agent-shell-queue--ready-sub-prompt
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'prompt-ready
               :on-event #'agent-shell-queue--ready-show))
        (setq agent-shell-queue--ready-sub-submit
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'input-submitted
               :on-event #'agent-shell-queue--ready-hide))
        (unless (shell-maker-busy)
          (agent-shell-queue--ready-show nil)))
    (setq-local buffer-read-only nil)
    (agent-shell-queue--ready-clear)
    (when agent-shell-queue--ready-sub-prompt
      (agent-shell-unsubscribe
       :subscription agent-shell-queue--ready-sub-prompt)
      (setq agent-shell-queue--ready-sub-prompt nil))
    (when agent-shell-queue--ready-sub-submit
      (agent-shell-unsubscribe
       :subscription agent-shell-queue--ready-sub-submit)
      (setq agent-shell-queue--ready-sub-submit nil))))

;;; Initialize on load

(agent-shell-queue--setup-hooks)

;;; Built-in item type registrations

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

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
