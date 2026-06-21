;;; agent-shell-queue-persistence.el --- File-based persistence for agent-shell-queue -*- lexical-binding: t; -*-

;;; Commentary:
;; Serialization (plist/JSON/YAML), save/load, archive, done-log, and write
;; logging for agent-shell-queue.  Loaded by agent-shell-queue.el at the end
;; of its load sequence; do not require this file independently.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)

;;; Forward declarations (functions/vars defined in agent-shell-queue.el)

(declare-function agent-shell-queue--current-store "agent-shell-queue")
(declare-function agent-shell-queue--ensure-loaded "agent-shell-queue")
(declare-function agent-shell-queue--make-store "agent-shell-queue")
(declare-function agent-shell-queue-store-items "agent-shell-queue")
(declare-function agent-shell-queue-store-format "agent-shell-queue")
(declare-function agent-shell-queue-store-file "agent-shell-queue")
(declare-function agent-shell-queue-item-to-plist "agent-shell-queue")
(declare-function agent-shell-queue-item-from-plist "agent-shell-queue")
(declare-function agent-shell-queue-item--make "agent-shell-queue")
(declare-function agent-shell-queue--executor-name "agent-shell-queue")
(declare-function agent-shell-queue--executor-from-plist "agent-shell-queue")
(declare-function agent-shell-queue--migrate-deferred-statuses "agent-shell-queue")
(declare-function agent-shell-queue-item-id "agent-shell-queue")
(declare-function agent-shell-queue-item-args "agent-shell-queue")
(declare-function agent-shell-queue-item-status "agent-shell-queue")
(declare-function agent-shell-queue-item-kind "agent-shell-queue")
(declare-function agent-shell-queue-item-background "agent-shell-queue")
(declare-function agent-shell-queue-item-created "agent-shell-queue")
(declare-function agent-shell-queue-item-dispatched "agent-shell-queue")
(declare-function agent-shell-queue-item-completed "agent-shell-queue")
(declare-function agent-shell-queue-item-response "agent-shell-queue")
(declare-function agent-shell-queue-item-outcome "agent-shell-queue")
(declare-function agent-shell-queue-item-directory "agent-shell-queue")
(declare-function agent-shell-queue-item-executor "agent-shell-queue")
(declare-function agent-shell-queue-queue-p "agent-shell-queue")
(declare-function agent-shell-queue-queue--make "agent-shell-queue")
(declare-function agent-shell-queue-queue-paused "agent-shell-queue")
(declare-function agent-shell-queue-queue-session-paused "agent-shell-queue")
(declare-function (setf agent-shell-queue-item-status) "agent-shell-queue")
(declare-function (setf agent-shell-queue-item-dispatched) "agent-shell-queue")
(declare-function (setf agent-shell-queue-store-items) "agent-shell-queue")
(declare-function yaml-encode "yaml")
(declare-function yaml-parse-string "yaml")

(defvar agent-shell-queue--store)
(defvar agent-shell-queue--queue)
(defvar agent-shell-queue--loaded)
(defvar agent-shell-queue-serialization-format)
(defvar agent-shell-queue-save-function)
(defvar agent-shell-queue-load-function)
(defvar agent-shell-queue-safe-save)
(defvar agent-shell-queue-safe-save-directory)
(defvar agent-shell-queue-safe-save-format)
(defvar agent-shell-queue-instance-name)
(defvar agent-shell-queue-done-log-file)
(defvar agent-shell-queue-archive-enabled)
(defvar agent-shell-queue-archive-file-function)
(defvar agent-shell-queue--last-flush-time)
(defvar agent-shell-queue--next-flush-time)
(defvar agent-shell-queue-auto-flush-interval)

;;; Write log

(defvar agent-shell-queue--write-log nil
  "Ring buffer of recent persistence events, newest first.
Each entry is a plist: :time :trigger :file :items :buckets :format :error.")

(defcustom agent-shell-queue-write-log-max-entries 200
  "Maximum entries retained in `agent-shell-queue--write-log'."
  :type 'integer
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-write-log-enabled nil
  "When non-nil, append each persistence event to *agent-shell-queue-log* and emit a message."
  :type 'boolean
  :group 'agent-shell-queue)

(defun agent-shell-queue--log-write (trigger &optional error-info)
  "Record a persistence event with TRIGGER symbol and optional ERROR-INFO string.
Always appends to `agent-shell-queue--write-log' (in-memory ring buffer).
When `agent-shell-queue-write-log-enabled' is non-nil, also appends to
*agent-shell-queue-log* buffer and calls `message'."
  (let* ((store (and (boundp 'agent-shell-queue--store) agent-shell-queue--store))
         (items (when store (ignore-errors (agent-shell-queue-store-items store))))
         (item-count (length (seq-mapcat #'cdr items)))
         (bucket-count (length items))
         (file (when store (ignore-errors (agent-shell-queue-store-file store))))
         (fmt (when store (ignore-errors (agent-shell-queue-store-format store))))
         (entry (list :time (float-time)
                      :trigger trigger
                      :file file
                      :items item-count
                      :buckets bucket-count
                      :format fmt
                      :error error-info)))
    (push entry agent-shell-queue--write-log)
    (when (> (length agent-shell-queue--write-log) agent-shell-queue-write-log-max-entries)
      (setcdr (nthcdr (1- agent-shell-queue-write-log-max-entries)
                      agent-shell-queue--write-log)
              nil))
    (when agent-shell-queue-write-log-enabled
      (let ((line (format "[%s] %s — %d item(s) in %d bucket(s), fmt=%s, file=%s%s"
                          (format-time-string "%H:%M:%S")
                          trigger item-count bucket-count
                          (or fmt "?")
                          (or file "backend")
                          (if error-info (format " ERROR: %s" error-info) ""))))
        (with-current-buffer (get-buffer-create "*agent-shell-queue-log*")
          (goto-char (point-max))
          (insert line "\n"))
        (message "agent-shell-queue: %s" line)))))

;;;###autoload
(defun agent-shell-queue-show-write-log ()
  "Display the in-memory write log in a readable buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*agent-shell-queue-write-log*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "agent-shell-queue write log\n" 'face 'bold))
      (insert (make-string 40 ?─) "\n\n")
      (if (null agent-shell-queue--write-log)
          (insert "(no entries yet)\n")
        (seq-do (lambda (entry)
                  (insert (format "%s  %-20s  items=%-4d  buckets=%-2d  fmt=%-6s  %s%s\n"
                                  (format-time-string "%H:%M:%S" (plist-get entry :time))
                                  (plist-get entry :trigger)
                                  (or (plist-get entry :items) 0)
                                  (or (plist-get entry :buckets) 0)
                                  (or (plist-get entry :format) "?")
                                  (or (plist-get entry :file) "backend")
                                  (if-let* ((e (plist-get entry :error)))
                                      (format "  ERROR: %s" e)
                                    ""))))
                agent-shell-queue--write-log)))
    (read-only-mode 1)
    (local-set-key (kbd "q") #'quit-window)
    (goto-char (point-min)))
  (pop-to-buffer "*agent-shell-queue-write-log*"))

;;; State file path helpers

(defun agent-shell-queue--default-state-file ()
  "Default queue state file path under `user-emacs-directory'."
  (expand-file-name (concat "agent-shell-queue."
                            (substring (agent-shell-queue-format-file-extension
                                        agent-shell-queue-serialization-format)
                                       1))
                    user-emacs-directory))

(defun agent-shell-queue--default-archive-file ()
  "Default JSONL archive file path under `user-emacs-directory'."
  (expand-file-name "agent-shell-queue-archive.jsonl" user-emacs-directory))

(defun agent-shell-queue--archive-file ()
  "Return the resolved archive file path, or nil if archiving is disabled."
  (when agent-shell-queue-archive-enabled
    (funcall agent-shell-queue-archive-file-function)))

;;; Persistence — plist format

(defun agent-shell-queue--serialize-plist (items)
  "Serialize ITEMS to an s-expression string (plist item format)."
  (with-temp-buffer
    (prin1 (seq-map (lambda (it) (cons (car it) (seq-map #'agent-shell-queue-item-to-plist (cdr it)))) items)
           (current-buffer))
    (buffer-string)))

(defun agent-shell-queue--deserialize-plist (str)
  "Deserialize STR (plist format) into an items alist."
  (let ((data (read str)))
    (unless (listp data)
      (error "Expected list, got %S" data))
    (seq-map (lambda (it) (cons (car it) (seq-map #'agent-shell-queue-item-from-plist (cdr it)))) data)))

;;; Persistence — JSON format

(defun agent-shell-queue--item-to-json (item)
  "Convert ITEM to a JSON-serializable plist.
Status is stored as a string; background as a JSON boolean;
executor as its registry name string or JSON null."
  (list
   :id (agent-shell-queue-item-id item)
   :args (agent-shell-queue-item-args item)
   :status (symbol-name (agent-shell-queue-item-status item))
   :kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
   :background (if (agent-shell-queue-item-background item) t :false)
   :created (agent-shell-queue-item-created item)
   :dispatched (or (agent-shell-queue-item-dispatched item) :null)
   :completed (or (agent-shell-queue-item-completed item) :null)
   :response (or (agent-shell-queue-item-response item) :null)
   :outcome (if-let* ((o (agent-shell-queue-item-outcome item))) (symbol-name o) :null)
   :directory (or (agent-shell-queue-item-directory item) :null)
   :executor (or (agent-shell-queue--executor-name (agent-shell-queue-item-executor item)) :null)))

(defun agent-shell-queue--item-from-json (obj)
  "Reconstruct a queue item from JSON-parsed plist OBJ.
Status is interned; background truthy only when exactly `t';
executor resolved from the registry (nil when absent or unknown)."
  (agent-shell-queue-item--make
   :id (plist-get obj :id)
   :args (or (plist-get obj :args) (plist-get obj :prompt))
   :status (intern (plist-get obj :status))
   :kind (intern (or (plist-get obj :kind) "prompt"))
   :background (eq t (plist-get obj :background))
   :created (plist-get obj :created)
   :dispatched (plist-get obj :dispatched)
   :completed (plist-get obj :completed)
   :response (let ((r (plist-get obj :response))) (unless (eq r :null) r))
   :outcome (when-let* ((o (plist-get obj :outcome))) (unless (eq o :null) (intern o)))
   :directory (let ((d (plist-get obj :directory))) (unless (eq d :null) d))
   :executor (let ((e (plist-get obj :executor)))
               (unless (or (null e) (eq e :null))
                 (agent-shell-queue--executor-from-plist e)))))

(defun agent-shell-queue--serialize-json (items)
  "Serialize ITEMS to a JSON string."
  (unless (fboundp 'json-serialize)
    (error "json-serialize not available (requires Emacs 27+)"))
  (json-serialize
   (vconcat
    (seq-map (lambda (it)
               (list :buffer (car it)
                     :items (vconcat (seq-map #'agent-shell-queue--item-to-json (cdr it)))))
             items))))

(defun agent-shell-queue--deserialize-json (str)
  "Deserialize STR (JSON format) into an items alist."
  (unless (fboundp 'json-parse-string)
    (error "json-parse-string not available (requires Emacs 27+)"))
  (thread-last (json-parse-string str
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)
               (seq-map (lambda (bucket)
                          (cons (plist-get bucket :buffer)
                                (seq-map #'agent-shell-queue--item-from-json
                                         (plist-get bucket :items)))))))

;;; Persistence — YAML format

(defun agent-shell-queue--item-to-yaml (item)
  "Convert ITEM to a hash-table suitable for `yaml-encode'.
Status is stored as a string; background as t or nil."
  (let ((h (make-hash-table :test 'equal)))
    (map-put! h "id" (agent-shell-queue-item-id item))
    (map-put! h "args" (agent-shell-queue-item-args item))
    (map-put! h "status" (symbol-name (agent-shell-queue-item-status item)))
    (map-put! h "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
    (map-put! h "background" (if (agent-shell-queue-item-background item) t nil))
    (map-put! h "created" (agent-shell-queue-item-created item))
    (map-put! h "dispatched" (agent-shell-queue-item-dispatched item))
    (map-put! h "completed" (agent-shell-queue-item-completed item))
    (map-put! h "response" (or (agent-shell-queue-item-response item) :null))
    (map-put! h "outcome" (if-let* ((o (agent-shell-queue-item-outcome item))) (symbol-name o) :null))
    (map-put! h "directory" (or (agent-shell-queue-item-directory item) :null))
    (map-put! h "executor" (or (agent-shell-queue--executor-name (agent-shell-queue-item-executor item)) :null))
    h))

(defun agent-shell-queue--item-from-yaml (obj)
  "Reconstruct a queue item from a hash-table OBJ produced by `yaml-parse-string'."
  (agent-shell-queue-item--make
   :id (map-elt obj "id")
   :args (or (map-elt obj "args") (map-elt obj "prompt"))
   :status (intern (map-elt obj "status"))
   :kind (intern (or (map-elt obj "kind") "prompt"))
   :background (eq t (map-elt obj "background"))
   :created (map-elt obj "created")
   :dispatched (map-elt obj "dispatched")
   :completed (map-elt obj "completed")
   :response (let ((r (map-elt obj "response"))) (unless (eq r :null) r))
   :outcome (when-let* ((o (map-elt obj "outcome"))) (unless (eq o :null) (intern o)))
   :directory (let ((d (map-elt obj "directory"))) (unless (eq d :null) d))
   :executor (let ((e (map-elt obj "executor")))
               (unless (or (null e) (eq e :null))
                 (agent-shell-queue--executor-from-plist e)))))

(defun agent-shell-queue--serialize-yaml (items)
  "Serialize ITEMS to a YAML string via `yaml-encode'."
  (unless (fboundp 'yaml-encode)
    (error "yaml-encode not available; install the `yaml' package"))
  (yaml-encode
   (vconcat
    (seq-map (lambda (pair)
               (let ((h (make-hash-table :test 'equal)))
                 (map-put! h "buffer" (car pair))
                 (map-put! h "items" (vconcat (seq-map #'agent-shell-queue--item-to-yaml (cdr pair))))
                 h))
             items))))

(defun agent-shell-queue--deserialize-yaml (str)
  "Deserialize STR (YAML format) into an items alist via `yaml-parse-string'."
  (unless (fboundp 'yaml-parse-string)
    (error "yaml-parse-string not available; install the `yaml' package"))
  (thread-last (yaml-parse-string str
                                  :object-type 'hash-table
                                  :sequence-type 'list
                                  :null-object nil
                                  :false-object nil)
               (seq-map (lambda (bucket)
                          (cons (map-elt bucket "buffer")
                                (seq-map #'agent-shell-queue--item-from-yaml
                                         (map-elt bucket "items")))))))

;;; Serialization generics

(cl-defgeneric agent-shell-queue--serialize-items (format items)
  "Serialize ITEMS for FORMAT, returning a string.
Signals an error for unknown formats.")

(cl-defgeneric agent-shell-queue--deserialize-items (format string)
  "Deserialize STRING for FORMAT, returning an items alist.
Signals an error for unknown formats.")

(cl-defgeneric agent-shell-queue-format-file-extension (_format)
  "Return the file extension string (with leading dot) for FORMAT."
  ".el")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql plist)) items)
  (agent-shell-queue--serialize-plist items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql plist)) string)
  (agent-shell-queue--deserialize-plist string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql plist)))
  ".el")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql json)) items)
  (agent-shell-queue--serialize-json items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql json)) string)
  (agent-shell-queue--deserialize-json string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql json)))
  ".json")

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql yaml)) items)
  (agent-shell-queue--serialize-yaml items))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql yaml)) string)
  (agent-shell-queue--deserialize-yaml string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql yaml)))
  ".yaml")

(defun agent-shell-queue-register-format (fmt serialize-fn deserialize-fn)
  "Register serialization FORMAT with SERIALIZE-FN and DESERIALIZE-FN.
FMT is a symbol; SERIALIZE-FN takes one argument (an items alist) and returns
a string; DESERIALIZE-FN takes a string and returns an items alist.
Installs cl-generic methods for `agent-shell-queue--serialize-items' and
`agent-shell-queue--deserialize-items' specialised on (eql FMT)."
  (eval `(cl-defmethod agent-shell-queue--serialize-items ((_format (eql ,fmt)) items)
           (funcall ,serialize-fn items))
        t)
  (eval `(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql ,fmt)) string)
           (funcall ,deserialize-fn string))
        t))

(defun agent-shell-queue-serialize (store)
  "Serialize STORE to a string using the format recorded in STORE."
  (agent-shell-queue--serialize-items
   (agent-shell-queue-store-format store)
   (agent-shell-queue-store-items store)))

(defun agent-shell-queue-deserialize (store string)
  "Parse STRING using the format in STORE, returning an items alist."
  (agent-shell-queue--deserialize-items
   (agent-shell-queue-store-format store)
   string))

(defun agent-shell-queue--safe-save-directory ()
  "Return the directory for safe-save backups.
Uses `agent-shell-queue-safe-save-directory' when set, otherwise
a subdirectory of `temporary-file-directory' named emacs-<instance>."
  (or agent-shell-queue-safe-save-directory
      (expand-file-name
       (format "emacs-%s"
               (if (functionp agent-shell-queue-instance-name)
                   (funcall agent-shell-queue-instance-name)
                 agent-shell-queue-instance-name))
       (temporary-file-directory))))

;;; Save

(defun agent-shell-queue--save ()
  "Persist all queue items; all items are persisted regardless of status.
Delegates to `agent-shell-queue-save-function' when set; otherwise writes
to the file in the current store.
When `agent-shell-queue-safe-save' is non-nil and no custom save function
is set, writes a versioned backup before overwriting the state file.

CRITICAL: Ensures the queue is loaded from disk before saving to prevent
overwriting existing state with an empty queue."
  ;; ALWAYS ensure loaded before saving to prevent data loss
  (agent-shell-queue--ensure-loaded)
  (if agent-shell-queue-save-function
      (progn
        (agent-shell-queue--log-write 'save-backend)
        (funcall agent-shell-queue-save-function))
    (let* ((base-store (agent-shell-queue--current-store))
           (store (agent-shell-queue--make-store
                   :items (agent-shell-queue-store-items base-store)
                   :format (agent-shell-queue-store-format base-store)
                   :file (agent-shell-queue-store-file base-store)))
           (serialized (agent-shell-queue-serialize store))
           (file (agent-shell-queue-store-file store)))
      (when-let* ((_ agent-shell-queue-safe-save)
                  (_ (file-exists-p file))
                  (fmt (or agent-shell-queue-safe-save-format
                           (agent-shell-queue-store-format store)))
                  (ext (agent-shell-queue-format-file-extension fmt))
                  (dir (agent-shell-queue--safe-save-directory))
                  (backup (expand-file-name
                           (format "agent-shell-queue-archive-%d%s"
                                   (truncate (float-time)) ext)
                           dir)))
        (make-directory dir t)
        (with-temp-file backup
          (insert (agent-shell-queue-serialize
                   (agent-shell-queue--make-store
                    :items (agent-shell-queue-store-items base-store)
                    :format fmt
                    :file nil)))))
      (condition-case err
          (progn
            (make-directory (file-name-directory file) t)
            (with-temp-file file
              (insert serialized))
            (agent-shell-queue--log-write 'save))
        (error
         (agent-shell-queue--log-write 'save (error-message-string err))
         (signal (car err) (cdr err))))))
  (setq agent-shell-queue--last-flush-time (float-time))
  (when agent-shell-queue-auto-flush-interval
    (setq agent-shell-queue--next-flush-time
          (time-add (current-time)
                    (seconds-to-time agent-shell-queue-auto-flush-interval)))))

;;; Done log and archive

(defun agent-shell-queue--append-done-log (buf-name item)
  "Append a JSON line for the completed ITEM in BUF-NAME to the done log.
No-op when `agent-shell-queue-done-log-file' is nil."
  (when agent-shell-queue-done-log-file
    (condition-case err
        (let ((entry (json-serialize
                      (list :id (agent-shell-queue-item-id item)
                            :target buf-name
                            :args (agent-shell-queue-item-args item)
                            :background (if (agent-shell-queue-item-background item) t :false)
                            :created (agent-shell-queue-item-created item)
                            :completed (float-time)))))
          (make-directory (file-name-directory agent-shell-queue-done-log-file) t)
          (write-region (concat entry "\n") nil agent-shell-queue-done-log-file t 'silent))
      (error (message "agent-shell-queue: done-log write failed: %s" err)))))

(defun agent-shell-queue--write-archive (buf-name item)
  "Append a JSONL record for ITEM in BUF-NAME to `agent-shell-queue-archive-file'.
The record includes the target buffer's directory, instance name, whether the
item was dispatched, ISO-8601 archive timestamp, and runtime (dispatched→completed)."
  (when-let* ((file (agent-shell-queue--archive-file)))
    (condition-case err
        (let* ((path (when-let* ((buf (get-buffer buf-name))
                                 ((buffer-live-p buf)))
                       (buffer-local-value 'default-directory buf)))
               (dispatched (agent-shell-queue-item-dispatched item))
               (completed (agent-shell-queue-item-completed item))
               (runtime (when (and dispatched completed) (- completed dispatched)))
               (instance (if (functionp agent-shell-queue-instance-name)
                            (funcall agent-shell-queue-instance-name)
                          agent-shell-queue-instance-name))
               (entry (json-serialize
                       (list :id (agent-shell-queue-item-id item)
                             :args (agent-shell-queue-item-args item)
                             :status (symbol-name (agent-shell-queue-item-status item))
                             :background (if (agent-shell-queue-item-background item) t :false)
                             :target buf-name
                             :path (or path :null)
                             :instance (or instance :null)
                             :ran (if dispatched t :false)
                             :archived (format-time-string "%Y-%m-%dT%H:%M:%S")
                             :created (or (agent-shell-queue-item-created item) :null)
                             :dispatched (or dispatched :null)
                             :completed (or completed :null)
                             :runtime (or runtime :null)
                             :outcome (if-let* ((o (agent-shell-queue-item-outcome item))) (symbol-name o) :null)))))
          (make-directory (file-name-directory file) t)
          ;; Advisory lock via Emacs's own .#file mechanism — no subprocess,
          ;; no persistent fd.  write-region with append opens, writes, closes.
          (unwind-protect
              (progn
                (lock-file file)
                (write-region (concat entry "\n") nil file t 'silent))
            (unlock-file file)))
      (error (message "agent-shell-queue: archive write failed: %s" err)))))

;;; Load

(defun agent-shell-queue--load ()
  "Populate the live store items from the durable store.
Delegates to `agent-shell-queue-load-function' when set; otherwise reads
from the file in the current store.  After loading, items with status
`running' are normalized to `active' since a running item from a previous
session cannot be resumed and must be re-dispatched."
  (if agent-shell-queue-load-function
      (condition-case err
          (progn
            (funcall agent-shell-queue-load-function)
            (agent-shell-queue--log-write 'load-backend))
        (error
         (agent-shell-queue--log-write 'load-backend (error-message-string err))
         (message "agent-shell-queue: load failed: %s" err)))
    (let* ((store (agent-shell-queue--current-store))
           (file (agent-shell-queue-store-file store)))
      (when (file-exists-p file)
        (condition-case err
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (agent-shell-queue-deserialize
                   store
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
          (error
           (agent-shell-queue--log-write 'load (error-message-string err))
           (message "agent-shell-queue: ignoring unreadable state: %s" err)))))
    ;; Items that were running when the session ended cannot be resumed.
    ;; Normalize them to active so they will be re-dispatched.
    (thread-last
      (agent-shell-queue-store-items agent-shell-queue--store)
      (seq-mapcat #'cdr)
      (seq-do (lambda (item)
                (when (eq (agent-shell-queue-item-status item) 'running)
                  (setf (agent-shell-queue-item-status item) 'active)
                  (setf (agent-shell-queue-item-dispatched item) nil)))))
    (agent-shell-queue--migrate-deferred-statuses)
    (agent-shell-queue--log-write 'load)))

(defun agent-shell-queue--migrate-queue-struct ()
  "Replace `agent-shell-queue--queue' if its struct layout is stale.
Detects any mismatch between the persisted vector length and the current struct
definition by comparing against a freshly-constructed instance.  This handles
any past or future field addition without per-field migration logic.
Preserves `paused' and `session-paused' from the old value when readable."
  (when (and (agent-shell-queue-queue-p agent-shell-queue--queue)
             (/= (length agent-shell-queue--queue)
                 (length (agent-shell-queue-queue--make))))
    (setq agent-shell-queue--queue
          (agent-shell-queue-queue--make
           :paused (condition-case nil
                     (agent-shell-queue-queue-paused agent-shell-queue--queue)
                     (error nil))
           :session-paused (condition-case nil
                             (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                             (error nil))))))

(provide 'agent-shell-queue-persistence)
;;; agent-shell-queue-persistence.el ends here
