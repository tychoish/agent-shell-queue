;;; test-agent-shell-queue-persistence.el --- Round-trip persistence tests -*- lexical-binding: t -*-

;; Tests that queue state survives save/load cycles as it would between
;; Emacs sessions.  Items are never auto-dispatched here — tests call
;; `agent-shell-queue-send-item' explicitly, bypassing the auto-scan/turn-complete
;; dispatch paths entirely, so no pause state is needed to keep them inert.
;;
;; Run filtered:
;;   (ert "^agent-shell-queue/persist")

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defmacro agent-shell-queue-test/with-persist-file (&rest body)
  "Run BODY with an isolated queue backed by a real temp plist state file.
Unlike `agent-shell-queue-test/isolate', `agent-shell-queue--save' and
`agent-shell-queue--load' execute for real against the temp file.
Nothing auto-dispatches here since no idle timer or turn-complete
subscription runs; tests call `agent-shell-queue-send-item' explicitly."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "asq-persist-" nil ".el"))
          (agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file tmp))
          (agent-shell-queue-state-file-function (lambda () tmp))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make))
          (agent-shell-queue--loaded t)
          (agent-shell-queue--subscriptions nil)
          (agent-shell-queue--stale-item-ids nil)
          (agent-shell-queue--next-flush-time nil)
          (agent-shell-queue--wait-timers nil)
          (agent-shell-queue--compact-running nil)
          (agent-shell-queue--response-start-positions nil)
          (agent-shell-queue--last-flush-time nil))
     (cl-letf (((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
               ((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
               ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore)
               ((symbol-function 'alert) #'ignore))
       (unwind-protect
           (progn ,@body)
         (ignore-errors (delete-file tmp))))))

(defun agent-shell-queue-test/persist-item (id prompt &optional kind status)
  "Build a minimal item for persistence tests."
  (agent-shell-queue-item--make
   :id id
   :args prompt
   :status (or status 'active)
   :kind (or kind 'emacs)
   :background nil
   :created 1000.0))

(defun agent-shell-queue-test/persist-save-and-reload ()
  "Save the live store to disk then reload it, simulating a session restart.
Clears in-memory items first so the load result is unambiguous."
  (agent-shell-queue--save)
  (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
  (agent-shell-queue--load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic round-trip

(ert-deftest agent-shell-queue/persist-active-items-survive-round-trip ()
  "Active items written to disk are present after a load."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "test prompt")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*test-session*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*test-session*"
                                  (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 1 (length restored)))
        (should (equal "q01" (agent-shell-queue-item-id (car restored))))))))

(ert-deftest agent-shell-queue/persist-deferred-items-survive ()
  "Deferred items persist across save/load but are migrated to blocked.skip on load."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "held" nil 'deferred)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should restored)
        (should (eq 'blocked.skip (agent-shell-queue-item-status restored)))))))

(ert-deftest agent-shell-queue/persist-empty-queue-round-trips ()
  "An empty queue produces no items after save/load."
  (agent-shell-queue-test/with-persist-file
    (agent-shell-queue-test/persist-save-and-reload)
    (should (null (agent-shell-queue-store-items agent-shell-queue--store)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering: only running items are not persisted; done items are kept

(ert-deftest agent-shell-queue/persist-done-items-are-saved ()
  "Done items are now persisted so their responses survive session reloads."
  (agent-shell-queue-test/with-persist-file
    (let ((done (agent-shell-queue-test/persist-item "q01" "finished" nil 'done))
          (active (agent-shell-queue-test/persist-item "q02" "pending")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" done active)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length items)))
        (let ((restored-done (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
              (restored-active (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items)))
          (should restored-done)
          (should restored-active)
          (should (eq 'done (agent-shell-queue-item-status restored-done))))))))

(ert-deftest agent-shell-queue/persist-running-items-reload-as-active ()
  "Running items are saved and reload as active with dispatched cleared."
  (agent-shell-queue-test/with-persist-file
    (let ((running (agent-shell-queue-test/persist-item "q01" "in-flight" nil 'running))
          (active (agent-shell-queue-test/persist-item "q02" "queued")))
      (setf (agent-shell-queue-item-dispatched running) 999.0)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" running active)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let* ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))
             (restored-running (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
             (restored-active (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items)))
        (should (= 2 (length items)))
        (should restored-running)
        (should (eq 'active (agent-shell-queue-item-status restored-running)))
        (should (null (agent-shell-queue-item-dispatched restored-running)))
        (should restored-active)
        (should (eq 'active (agent-shell-queue-item-status restored-active)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field fidelity

(ert-deftest agent-shell-queue/persist-item-fields-preserved ()
  "All serializable item fields survive a save/load round-trip."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-item--make
                 :id "q42"
                 :args "(message \"hello\")"
                 :status 'active
                 :kind 'emacs
                 :background t
                 :created 12345.0
                 :dispatched nil
                 :completed nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((r (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should (equal "q42"               (agent-shell-queue-item-id r)))
        (should (equal "(message \"hello\")" (agent-shell-queue-item-args r)))
        (should (eq 'active                (agent-shell-queue-item-status r)))
        (should (eq 'emacs                 (agent-shell-queue-item-kind r)))
        (should (eq t                      (agent-shell-queue-item-background r)))
        (should (= 12345.0                 (agent-shell-queue-item-created r)))))))

(ert-deftest agent-shell-queue/persist-emacs-kind-item-survives ()
  "An emacs-kind item round-trips with kind and prompt intact."
  (agent-shell-queue-test/with-persist-file
    (let ((item (agent-shell-queue-test/persist-item "q01" "(+ 1 2)" 'emacs)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*s*" item)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((r (car (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store))))))
        (should (eq 'emacs    (agent-shell-queue-item-kind r)))
        (should (equal "(+ 1 2)" (agent-shell-queue-item-args r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple buckets

(ert-deftest agent-shell-queue/persist-multiple-buckets-survive ()
  "Items across multiple buckets all persist independently."
  (agent-shell-queue-test/with-persist-file
    (let ((a (agent-shell-queue-test/persist-item "qa" "prompt-a"))
          (b (agent-shell-queue-test/persist-item "qb" "prompt-b")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "*session-a*" a)
                  (list "*session-b*" b)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((items-a (cdr (assoc "*session-a*" (agent-shell-queue-store-items agent-shell-queue--store))))
            (items-b (cdr (assoc "*session-b*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 1 (length items-a)))
        (should (= 1 (length items-b)))
        (should (equal "qa" (agent-shell-queue-item-id (car items-a))))
        (should (equal "qb" (agent-shell-queue-item-id (car items-b))))))))

(ert-deftest agent-shell-queue/persist-ordering-preserved ()
  "Items in a bucket preserve their insertion order across save/load."
  (agent-shell-queue-test/with-persist-file
    (let ((items (list (agent-shell-queue-test/persist-item "q01" "first")
                       (agent-shell-queue-test/persist-item "q02" "second")
                       (agent-shell-queue-test/persist-item "q03" "third"))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (cons "*s*" items)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (equal '("q01" "q02" "q03")
                       (seq-map #'agent-shell-queue-item-id restored)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paused queue persistence

(ert-deftest agent-shell-queue/persist-paused-queue-items-stay-active ()
  "Items are never auto-dispatched in this test fixture and remain active,
ensuring they survive the save/load cycle."
  (agent-shell-queue-test/with-persist-file
    (let ((items (list (agent-shell-queue-test/persist-item "q01" "a")
                       (agent-shell-queue-test/persist-item "q02" "b")
                       (agent-shell-queue-test/persist-item "q03" "c"))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (cons "*s*" items)))
      (agent-shell-queue-test/persist-save-and-reload)
      (let ((restored (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length restored)))
        (should (seq-every-p (lambda (it) (eq 'active (agent-shell-queue-item-status it)))
                             restored))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs kind: dispatch without LLM

(defvar agent-shell-queue-persist-test--side-effect nil
  "Scratch variable written by emacs-kind test items.")

(ert-deftest agent-shell-queue/persist-emacs-kind-dispatches-without-llm ()
  "An emacs-lisp-kind item executes Lisp synchronously with no LLM interaction.
The item completes and the side effect is visible."
  (agent-shell-queue-test/with-persist-file
    (let* ((target (generate-new-buffer "*asq-persist-emacs-target*"))
           (buf-name (buffer-name target))
           (item (agent-shell-queue-item--make
                  :id "q01"
                  :args (format "(setq agent-shell-queue-persist-test--side-effect %S)" 42)
                  :status 'active
                  :kind 'emacs-lisp
                  :background nil
                  :created 1000.0)))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list buf-name item)))
            (setq agent-shell-queue-persist-test--side-effect nil)
            (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                      ((symbol-function 'agent-shell-queue--append-done-log) #'ignore))
              (agent-shell-queue-send-item "q01"))
            ;; Side effect ran.
            (should (= 42 agent-shell-queue-persist-test--side-effect))
            ;; Item is done.
            (should (eq 'done (agent-shell-queue-item-status item))))
        (kill-buffer target)))))

(ert-deftest agent-shell-queue/persist-emacs-kind-survives-then-dispatches ()
  "An emacs-lisp-kind item survives a save/load cycle, then dispatches without LLM."
  (agent-shell-queue-test/with-persist-file
    (let* ((target (generate-new-buffer "*asq-persist-emacs-target2*"))
           (buf-name (buffer-name target))
           (item (agent-shell-queue-item--make
                  :id "q01"
                  :args (format "(setq agent-shell-queue-persist-test--side-effect %S)" 99)
                  :status 'active
                  :kind 'emacs-lisp
                  :background nil
                  :created 1000.0)))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list buf-name item)))
            (agent-shell-queue-test/persist-save-and-reload)
            (let ((restored (car (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))
              (should restored)
              (should (eq 'emacs-lisp (agent-shell-queue-item-kind restored)))
              ;; Dispatch the restored item.
              (setq agent-shell-queue-persist-test--side-effect nil)
              (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                        ((symbol-function 'agent-shell-queue--append-done-log) #'ignore))
                (agent-shell-queue-send-item (agent-shell-queue-item-id restored)))
              (should (= 99 agent-shell-queue-persist-test--side-effect))
              (should (eq 'done (agent-shell-queue-item-status restored)))))
        (kill-buffer target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bulk archive

(ert-deftest agent-shell-queue/persist-archive-done-n-removes-oldest ()
  "archive-done-n removes the N oldest done items and leaves the rest."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((old1 (agent-shell-queue-item--make
                     :id "q01" :args "oldest" :status 'done
                     :kind 'emacs :background nil :created 100.0))
              (old2 (agent-shell-queue-item--make
                     :id "q02" :args "middle" :status 'done
                     :kind 'emacs :background nil :created 200.0))
              (new1 (agent-shell-queue-item--make
                     :id "q03" :args "newest" :status 'done
                     :kind 'emacs :background nil :created 300.0))
              (active (agent-shell-queue-test/persist-item "q04" "active")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*s*" old1 old2 new1 active)))
          (agent-shell-queue-archive-done-n 2)
          (let ((items (cdr (assoc "*s*" (agent-shell-queue-store-items agent-shell-queue--store)))))
            (should (= 2 (length items)))
            (should (seq-find (lambda (it) (equal "q03" (agent-shell-queue-item-id it))) items))
            (should (seq-find (lambda (it) (equal "q04" (agent-shell-queue-item-id it))) items))
            (should-not (seq-find (lambda (it) (equal "q01" (agent-shell-queue-item-id it))) items))
            (should-not (seq-find (lambda (it) (equal "q02" (agent-shell-queue-item-id it))) items))))))))

(ert-deftest agent-shell-queue/persist-archive-done-all-removes-all-done ()
  "archive-done-all removes every done item across all buckets."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((done-a (agent-shell-queue-item--make
                       :id "qa1" :args "done-a" :status 'done
                       :kind 'emacs :background nil :created 100.0))
              (done-b (agent-shell-queue-item--make
                       :id "qb1" :args "done-b" :status 'done
                       :kind 'emacs :background nil :created 200.0))
              (active-a (agent-shell-queue-test/persist-item "qa2" "active-a"))
              (active-b (agent-shell-queue-test/persist-item "qb2" "active-b")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*session-a*" done-a active-a)
                      (list "*session-b*" done-b active-b)))
          (agent-shell-queue-archive-done-all)
          (let ((items-a (cdr (assoc "*session-a*" (agent-shell-queue-store-items agent-shell-queue--store))))
                (items-b (cdr (assoc "*session-b*" (agent-shell-queue-store-items agent-shell-queue--store)))))
            (should (= 1 (length items-a)))
            (should (= 1 (length items-b)))
            (should (equal "qa2" (agent-shell-queue-item-id (car items-a))))
            (should (equal "qb2" (agent-shell-queue-item-id (car items-b))))))))))

(ert-deftest agent-shell-queue/persist-archive-done-n-errors-when-none ()
  "archive-done-n signals user-error when no done items exist."
  (agent-shell-queue-test/with-persist-file
    (let ((agent-shell-queue-archive-enabled t))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (let ((active (agent-shell-queue-test/persist-item "q01" "active")))
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (list (list "*s*" active)))
          (should-error (agent-shell-queue-archive-done-n 5) :type 'user-error))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load archive

(ert-deftest agent-shell-queue/persist-load-archive-imports-as-active ()
  "load-archive reads a JSONL file and imports each line as an active item."
  (agent-shell-queue-test/with-persist-file
    (let* ((archive-file (make-temp-file "asq-archive-" nil ".jsonl"))
           (agent-shell-queue-archive-enabled t)
           (agent-shell-queue-archive-file-function (lambda () archive-file)))
      (unwind-protect
          (progn
            (with-temp-file archive-file
              (insert "{\"prompt\":\"hello world\",\"kind\":\"prompt\",\"background\":false,\"target\":\"*nonexistent*\"}\n")
              (insert "{\"prompt\":\"(+ 1 2)\",\"kind\":\"emacs\",\"background\":true,\"target\":\"*also-gone*\"}\n"))
            (agent-shell-queue-load-archive archive-file)
            (let* ((all-items
                    (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                                 (seq-mapcat #'cdr))))
              (should (= 2 (length all-items)))
              (should (seq-every-p (lambda (it) (eq 'active (agent-shell-queue-item-status it)))
                                   all-items))
              (should (seq-find (lambda (it) (equal "hello world" (agent-shell-queue-item-args it)))
                                all-items))
              (should (seq-find (lambda (it) (equal "(+ 1 2)" (agent-shell-queue-item-args it)))
                                all-items))
              (let ((emacs-item (seq-find (lambda (it) (eq 'emacs (agent-shell-queue-item-kind it)))
                                          all-items)))
                (should emacs-item)
                (should (eq t (agent-shell-queue-item-background emacs-item))))))
        (ignore-errors (delete-file archive-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Idle flush

(ert-deftest agent-shell-queue/idle-flush-noop-when-not-loaded ()
  "idle-flush does not call --save when the queue has not been loaded."
  (let ((agent-shell-queue--loaded nil)
        (saved nil))
    (cl-letf (((symbol-function 'agent-shell-queue--save)
               (lambda () (setq saved t))))
      (agent-shell-queue--idle-flush)
      (should-not saved))))

(ert-deftest agent-shell-queue/idle-flush-saves-when-loaded ()
  "idle-flush calls --save when the queue is loaded."
  (agent-shell-queue-test/with-persist-file
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-shell-queue--save)
                 (lambda () (setq saved t))))
        (agent-shell-queue--idle-flush)
        (should saved)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Safe-save pruning

(defmacro agent-shell-queue-test/with-backup-dir (&rest body)
  "Run BODY with a temp backup directory bound as `dir'."
  (declare (indent 0))
  `(let ((dir (make-temp-file "asq-backup-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(defun agent-shell-queue-test/make-backup (dir timestamp ext &optional mtime)
  "Create a placeholder backup file in DIR named for TIMESTAMP and EXT.
When MTIME is non-nil, set the file modification time to that value."
  (let ((file (expand-file-name
               (format "agent-shell-queue-archive-%d%s" timestamp ext)
               dir)))
    (with-temp-file file (insert "placeholder"))
    (when mtime
      (set-file-times file mtime))
    file))

(defun agent-shell-queue-test/backup-count (dir ext)
  "Count backup files in DIR matching the standard pattern for EXT."
  (length (directory-files
           dir nil
           (concat "\\`agent-shell-queue-archive-[0-9]+" (regexp-quote ext) "\\'"))))

(ert-deftest agent-shell-queue/safe-save-prune-noop-when-nil ()
  "Pruning is a no-op when safe-save-max-files is nil."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files nil))
      (agent-shell-queue-test/make-backup dir 1000 ".el")
      (agent-shell-queue-test/make-backup dir 1001 ".el")
      (agent-shell-queue-test/make-backup dir 1002 ".el")
      (agent-shell-queue--safe-save-prune dir ".el")
      (should (= 3 (agent-shell-queue-test/backup-count dir ".el"))))))

(ert-deftest agent-shell-queue/safe-save-prune-noop-at-limit ()
  "Pruning does nothing when the count equals the limit."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files 3))
      (agent-shell-queue-test/make-backup dir 1000 ".el")
      (agent-shell-queue-test/make-backup dir 1001 ".el")
      (agent-shell-queue-test/make-backup dir 1002 ".el")
      (agent-shell-queue--safe-save-prune dir ".el")
      (should (= 3 (agent-shell-queue-test/backup-count dir ".el"))))))

(ert-deftest agent-shell-queue/safe-save-prune-removes-oldest ()
  "Pruning removes the single oldest file when count exceeds the limit."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files 2))
      (let ((oldest (agent-shell-queue-test/make-backup
                     dir 1000 ".el" (seconds-to-time 1000000)))
            (_mid   (agent-shell-queue-test/make-backup
                     dir 1001 ".el" (seconds-to-time 1001000)))
            (_new   (agent-shell-queue-test/make-backup
                     dir 1002 ".el" (seconds-to-time 1002000))))
        (agent-shell-queue--safe-save-prune dir ".el")
        (should (= 2 (agent-shell-queue-test/backup-count dir ".el")))
        (should-not (file-exists-p oldest))))))

(ert-deftest agent-shell-queue/safe-save-prune-removes-one-at-a-time ()
  "Pruning removes exactly one file per call even when multiple exceed the limit."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files 1))
      (agent-shell-queue-test/make-backup dir 1000 ".el" (seconds-to-time 1000000))
      (agent-shell-queue-test/make-backup dir 1001 ".el" (seconds-to-time 1001000))
      (agent-shell-queue-test/make-backup dir 1002 ".el" (seconds-to-time 1002000))
      (agent-shell-queue--safe-save-prune dir ".el")
      (should (= 2 (agent-shell-queue-test/backup-count dir ".el"))))))

(ert-deftest agent-shell-queue/safe-save-prune-ignores-unrelated-files ()
  "Pruning ignores files that do not match the backup filename pattern."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files 1))
      (agent-shell-queue-test/make-backup dir 1000 ".el")
      (with-temp-file (expand-file-name "other.el" dir) (insert "x"))
      (with-temp-file (expand-file-name "agent-shell-queue-state.el" dir) (insert "x"))
      (agent-shell-queue--safe-save-prune dir ".el")
      (should (= 1 (agent-shell-queue-test/backup-count dir ".el"))))))

(ert-deftest agent-shell-queue/safe-save-prune-respects-ext ()
  "Pruning only counts and removes files matching the given extension."
  (agent-shell-queue-test/with-backup-dir
    (let ((agent-shell-queue-safe-save-max-files 1))
      (agent-shell-queue-test/make-backup dir 1000 ".el"   (seconds-to-time 1000000))
      (agent-shell-queue-test/make-backup dir 1001 ".el"   (seconds-to-time 1001000))
      (agent-shell-queue-test/make-backup dir 2000 ".json" (seconds-to-time 2000000))
      (agent-shell-queue--safe-save-prune dir ".el")
      (should (= 1 (agent-shell-queue-test/backup-count dir ".el")))
      (should (= 1 (agent-shell-queue-test/backup-count dir ".json"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Safe-save prune integrated with --save

(ert-deftest agent-shell-queue/safe-save-prune-fires-on-save ()
  "safe-save-max-files is enforced each time --save writes a backup."
  (agent-shell-queue-test/with-persist-file
    (agent-shell-queue-test/with-backup-dir
      (let ((agent-shell-queue-safe-save t)
            (agent-shell-queue-safe-save-directory dir)
            (agent-shell-queue-safe-save-max-files 2))
        ;; Pre-populate with 2 older backups so the next save pushes count to 3.
        (agent-shell-queue-test/make-backup dir 1000 ".el" (seconds-to-time 1000000))
        (agent-shell-queue-test/make-backup dir 1001 ".el" (seconds-to-time 1001000))
        ;; --save writes a new backup then prunes — count should stay at 2.
        (agent-shell-queue--save)
        (should (= 2 (agent-shell-queue-test/backup-count dir ".el")))))))

(provide 'test-agent-shell-queue-persistence)
;;; test-agent-shell-queue-persistence.el ends here
