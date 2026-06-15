;;; test-agent-shell-queue-types.el --- Tests for item-type registry -*- lexical-binding: t -*-

;; Tests for the agent-shell-queue-item-type registry, buffer predicates,
;; validation, dispatch functions, and enqueue helpers added in the
;; capability-aware queue item system.
;;
;; Batch run:
;;   cask exec ert-runner -L /tmp/annotated-completing-read \
;;     test/test-agent-shell-queue-types.el

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue)
(require 'test-helper)

;;; Isolation macro (mirrors test-agent-shell-queue.el)

(defmacro asq-types-test/isolate (&rest body)
  "Execute BODY with isolated queue state and a clean item-type registry."
  `(let* ((agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file nil))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store
            :paused nil
            :session-paused nil
            :editing-ids nil
            :interjection-pending nil))
          (agent-shell-queue--loaded t))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
               ((symbol-function 'alert) #'ignore))
       ,@body)))

;;; ─────────────────────────────────────────────────────────────
;;; Registry: register-item-type / type-for-kind

(ert-deftest agent-shell-queue-types/register-basic ()
  "Registering a type makes it retrievable by kind."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'test-kind
     :label "test-label"
     :buffer-pred nil
     :dispatch-fn #'ignore
     :input-spec '(:kind none))
    (let ((type (agent-shell-queue--type-for-kind 'test-kind)))
      (should type)
      (should (eq 'test-kind (agent-shell-queue-item-type-kind type)))
      (should (equal "test-label" (agent-shell-queue-item-type-label type)))
      (should (null (agent-shell-queue-item-type-buffer-pred type))))))

(ert-deftest agent-shell-queue-types/register-replaces-existing ()
  "Re-registering the same kind replaces the old entry."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'foo :label "first" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (agent-shell-queue-register-item-type
     :kind 'foo :label "second" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (should (= 1 (length agent-shell-queue--item-types)))
    (should (equal "second"
                   (agent-shell-queue-item-type-label
                    (agent-shell-queue--type-for-kind 'foo))))))

(ert-deftest agent-shell-queue-types/type-for-kind-unknown ()
  "type-for-kind returns nil for an unregistered kind."
  (let ((agent-shell-queue--item-types nil))
    (should (null (agent-shell-queue--type-for-kind 'nonexistent)))))

(ert-deftest agent-shell-queue-types/type-for-kind-nil ()
  "type-for-kind with nil returns nil without error."
  (should (null (agent-shell-queue--type-for-kind nil))))

;;; ─────────────────────────────────────────────────────────────
;;; Registry: types-for-buffer

(ert-deftest agent-shell-queue-types/types-for-buffer-nil-accepts-all ()
  "nil buffer (unassigned) accepts all registered types."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'restricted :label "r" :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (agent-shell-queue-register-item-type
     :kind 'open :label "o" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (let ((types (agent-shell-queue--types-for-buffer nil)))
      (should (= 2 (length types))))))

(ert-deftest agent-shell-queue-types/types-for-buffer-filters-by-pred ()
  "types-for-buffer only includes types whose pred returns t for the buffer."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'yes :label "y" :buffer-pred (lambda (_) t) :dispatch-fn #'ignore :input-spec nil)
    (agent-shell-queue-register-item-type
     :kind 'no :label "n" :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (agent-shell-queue-register-item-type
     :kind 'any :label "a" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (with-temp-buffer
      (let ((types (agent-shell-queue--types-for-buffer (current-buffer))))
        (should (= 2 (length types)))
        (should (seq-find (lambda (t) (eq 'yes (agent-shell-queue-item-type-kind t))) types))
        (should (seq-find (lambda (t) (eq 'any (agent-shell-queue-item-type-kind t))) types))
        (should (null (seq-find (lambda (t) (eq 'no (agent-shell-queue-item-type-kind t))) types)))))))

;;; ─────────────────────────────────────────────────────────────
;;; Buffer predicates

(ert-deftest agent-shell-queue-types/agent-shell-buffer-p-rejects-non-shell ()
  "agent-shell-buffer-p returns nil for non-agent-shell buffers."
  (with-temp-buffer
    (should (null (agent-shell-queue--agent-shell-buffer-p (current-buffer))))))

(ert-deftest agent-shell-queue-types/agent-shell-buffer-p-rejects-dead ()
  "agent-shell-buffer-p returns nil for dead buffers."
  (let ((buf (generate-new-buffer " *dead-test*")))
    (kill-buffer buf)
    (should (null (agent-shell-queue--agent-shell-buffer-p buf)))))

(ert-deftest agent-shell-queue-types/eshell-buffer-p-accepts-eshell ()
  "eshell-buffer-p returns t for a live buffer in eshell-mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'eshell-mode)
               (lambda () (setq major-mode 'eshell-mode))))
      (eshell-mode)
      (should (agent-shell-queue--eshell-buffer-p (current-buffer))))))

(ert-deftest agent-shell-queue-types/eshell-buffer-p-rejects-others ()
  "eshell-buffer-p returns nil for non-eshell buffers."
  (with-temp-buffer
    (should (null (agent-shell-queue--eshell-buffer-p (current-buffer))))))

(ert-deftest agent-shell-queue-types/eat-buffer-p-rejects-non-eat ()
  "eat-buffer-p returns nil for non-eat buffers."
  (with-temp-buffer
    (should (null (agent-shell-queue--eat-buffer-p (current-buffer))))))

;;; ─────────────────────────────────────────────────────────────
;;; Validation

(ert-deftest agent-shell-queue-types/validate-nil-buffer-always-passes ()
  "validate-kind-for-buffer returns t when buffer is nil (unassigned)."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'strict :label "s" :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (should (eq t (agent-shell-queue--validate-kind-for-buffer 'strict nil)))))

(ert-deftest agent-shell-queue-types/validate-compatible-buffer-passes ()
  "validate-kind-for-buffer returns t when pred accepts the buffer."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'accept-all :label "a" :buffer-pred (lambda (_) t) :dispatch-fn #'ignore :input-spec nil)
    (with-temp-buffer
      (should (eq t (agent-shell-queue--validate-kind-for-buffer 'accept-all (current-buffer)))))))

(ert-deftest agent-shell-queue-types/validate-incompatible-buffer-errors ()
  "validate-kind-for-buffer signals user-error when pred rejects the buffer."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'reject-all :label "r"
     :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (with-temp-buffer
      (should-error
       (agent-shell-queue--validate-kind-for-buffer 'reject-all (current-buffer))
       :type 'user-error))))

(ert-deftest agent-shell-queue-types/validate-unknown-kind-passes ()
  "validate-kind-for-buffer returns t for unknown kinds (no type registered)."
  (let ((agent-shell-queue--item-types nil))
    (with-temp-buffer
      (should (eq t (agent-shell-queue--validate-kind-for-buffer 'unknown-kind (current-buffer)))))))

(ert-deftest agent-shell-queue-types/validate-nil-pred-always-passes ()
  "validate-kind-for-buffer returns t when type has no buffer-pred (any buffer ok)."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'any-buf :label "a" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (with-temp-buffer
      (should (eq t (agent-shell-queue--validate-kind-for-buffer 'any-buf (current-buffer)))))))

;;; ─────────────────────────────────────────────────────────────
;;; kind-needs-session-p

(ert-deftest agent-shell-queue-types/kind-needs-session-p-true-for-agent-shell ()
  "kind-needs-session-p returns t when pred is agent-shell-buffer-p."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'session-kind :label "s"
     :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
     :dispatch-fn #'ignore :input-spec nil)
    (should (agent-shell-queue--kind-needs-session-p 'session-kind))))

(ert-deftest agent-shell-queue-types/kind-needs-session-p-false-for-nil-pred ()
  "kind-needs-session-p returns nil when type has no buffer-pred."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'open-kind :label "o" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (should (null (agent-shell-queue--kind-needs-session-p 'open-kind)))))

(ert-deftest agent-shell-queue-types/kind-needs-session-p-false-for-eshell ()
  "kind-needs-session-p returns nil for eshell kinds."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'eshell-kind :label "e"
     :buffer-pred #'agent-shell-queue--eshell-buffer-p
     :dispatch-fn #'ignore :input-spec nil)
    (should (null (agent-shell-queue--kind-needs-session-p 'eshell-kind)))))

(ert-deftest agent-shell-queue-types/kind-needs-session-p-unknown-kind ()
  "kind-needs-session-p returns nil for unregistered kinds."
  (let ((agent-shell-queue--item-types nil))
    (should (null (agent-shell-queue--kind-needs-session-p 'nonexistent)))))

;;; ─────────────────────────────────────────────────────────────
;;; candidate-buffers-for-kind

(ert-deftest agent-shell-queue-types/candidate-buffers-any-pred-returns-nil ()
  "candidate-buffers-for-kind returns nil when kind accepts any buffer."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'any :label "a" :buffer-pred nil :dispatch-fn #'ignore :input-spec nil)
    (should (null (agent-shell-queue--candidate-buffers-for-kind 'any)))))

(ert-deftest agent-shell-queue-types/candidate-buffers-filters-live ()
  "candidate-buffers-for-kind only returns live buffers matching pred."
  (let ((agent-shell-queue--item-types nil))
    (agent-shell-queue-register-item-type
     :kind 'text-only :label "t"
     :buffer-pred (lambda (b) (with-current-buffer b (derived-mode-p 'text-mode)))
     :dispatch-fn #'ignore :input-spec nil)
    (with-temp-buffer
      (text-mode)
      (let ((candidates (agent-shell-queue--candidate-buffers-for-kind 'text-only)))
        (should (seq-find (lambda (b) (eq b (current-buffer))) candidates))))))

(ert-deftest agent-shell-queue-types/candidate-buffers-unknown-kind ()
  "candidate-buffers-for-kind returns nil for unknown kinds."
  (let ((agent-shell-queue--item-types nil))
    (should (null (agent-shell-queue--candidate-buffers-for-kind 'unknown)))))

;;; ─────────────────────────────────────────────────────────────
;;; pick-buffer-for-kind: fallthrough behavior

(ert-deftest agent-shell-queue-types/pick-buffer-no-candidates-strict-errors ()
  "pick-buffer-for-kind signals error when strict-assignment and no candidates."
  (let ((agent-shell-queue--item-types nil)
        (agent-shell-queue-strict-buffer-assignment t))
    (agent-shell-queue-register-item-type
     :kind 'never-matches :label "n"
     :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (should-error
     (agent-shell-queue--pick-buffer-for-kind 'never-matches)
     :type 'user-error)))

(ert-deftest agent-shell-queue-types/pick-buffer-no-candidates-fallthrough ()
  "pick-buffer-for-kind returns nil (unassigned) when no candidates and non-strict."
  (let ((agent-shell-queue--item-types nil)
        (agent-shell-queue-strict-buffer-assignment nil))
    (agent-shell-queue-register-item-type
     :kind 'never-matches :label "n"
     :buffer-pred (lambda (_) nil) :dispatch-fn #'ignore :input-spec nil)
    (should (null (agent-shell-queue--pick-buffer-for-kind 'never-matches)))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch: emacs-lisp

(defvar agent-shell-queue-types-test--dispatch-sentinel nil
  "Dynamic scratch variable for dispatch-emacs-lisp-evaluates-form test.")

(ert-deftest agent-shell-queue-types/dispatch-emacs-lisp-evaluates-form ()
  "dispatch-emacs-lisp evaluates the item's args as a Lisp form."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-el-1"
                 :args "(setq agent-shell-queue-types-test--dispatch-sentinel t)"
                 :status 'running
                 :kind 'emacs-lisp :created 1000.0))
          (pair (list "buf" item))
          completed-id)
     (setq agent-shell-queue-types-test--dispatch-sentinel nil)
     (cl-letf (((symbol-function 'agent-shell-queue--item-by-id)
                (lambda (id) pair))
               ((symbol-function 'agent-shell-queue--complete-item)
                (lambda (item _) (setq completed-id (agent-shell-queue-item-id item)))))
       (agent-shell-queue--dispatch-emacs-lisp item "buf")
       (should agent-shell-queue-types-test--dispatch-sentinel)
       (should (equal "q-el-1" completed-id))))))

(ert-deftest agent-shell-queue-types/dispatch-emacs-lisp-errors-dont-crash ()
  "dispatch-emacs-lisp messages on error but still completes the item."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-el-err" :args "(error \"intentional\")" :status 'running
                 :kind 'emacs-lisp :created 1000.0))
          (pair (list "buf" item))
          completed-p messages)
     (cl-letf (((symbol-function 'agent-shell-queue--item-by-id)
                (lambda (id) pair))
               ((symbol-function 'agent-shell-queue--complete-item)
                (lambda (_ __) (setq completed-p t)))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
       (agent-shell-queue--dispatch-emacs-lisp item "buf")
       (should completed-p)
       (should (seq-some (lambda (m) (string-match-p "emacs-lisp" m)) messages))))))

(ert-deftest agent-shell-queue-types/dispatch-emacs-lisp-malformed-form-errors ()
  "dispatch-emacs-lisp handles malformed Lisp expressions without crashing."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-bad" :args "((((invalid" :status 'running
                 :kind 'emacs-lisp :created 1000.0))
          (pair (list "buf" item))
          completed-p)
     (cl-letf (((symbol-function 'agent-shell-queue--item-by-id)
                (lambda (_) pair))
               ((symbol-function 'agent-shell-queue--complete-item)
                (lambda (_ __) (setq completed-p t)))
               ((symbol-function 'message) #'ignore))
       (agent-shell-queue--dispatch-emacs-lisp item "buf")
       (should completed-p)))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch: emacs-command

(ert-deftest agent-shell-queue-types/dispatch-emacs-command-invokes ()
  "dispatch-emacs-command calls the named command interactively."
  (asq-types-test/isolate
   (let* ((invoked nil)
          (item (agent-shell-queue-item--make
                 :id "q-cmd-1" :args "agent-shell-queue-types/test-cmd"
                 :status 'running :kind 'emacs-command :created 1000.0))
          (pair (list "buf" item))
          completed-p)
     (fset 'agent-shell-queue-types/test-cmd
           (lambda () (interactive) (setq invoked t)))
     (cl-letf (((symbol-function 'agent-shell-queue--item-by-id)
                (lambda (_) pair))
               ((symbol-function 'agent-shell-queue--complete-item)
                (lambda (_ __) (setq completed-p t))))
       (agent-shell-queue--dispatch-emacs-command item "buf")
       (should invoked)
       (should completed-p)))))

(ert-deftest agent-shell-queue-types/dispatch-emacs-command-error-completes ()
  "dispatch-emacs-command messages on error and still completes the item."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-cmd-err" :args "undefined-test-command-xyz"
                 :status 'running :kind 'emacs-command :created 1000.0))
          (pair (list "buf" item))
          completed-p)
     (cl-letf (((symbol-function 'agent-shell-queue--item-by-id)
                (lambda (_) pair))
               ((symbol-function 'agent-shell-queue--complete-item)
                (lambda (_ __) (setq completed-p t)))
               ((symbol-function 'message) #'ignore))
       (agent-shell-queue--dispatch-emacs-command item "buf")
       (should completed-p)))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch: pause / compact

(ert-deftest agent-shell-queue-types/dispatch-pause-pauses-session ()
  "dispatch-pause-compact pauses the queue for the target buffer."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-pause-1" :args "" :status 'running
                 :kind 'pause :created 1000.0))
          paused-buf)
     (cl-letf (((symbol-function 'agent-shell-queue--pause-and-save)
                (lambda (name) (setq paused-buf name))))
       (agent-shell-queue--dispatch-pause-compact item "mybuf")
       (should (equal "mybuf" paused-buf))
       (should (member (cons "mybuf" "q-pause-1")
                       agent-shell-queue--compact-running))))))

(ert-deftest agent-shell-queue-types/dispatch-compact-pauses-and-alerts ()
  "dispatch-pause-compact pauses and sends alert for compact items."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-cmp-1" :args "do the thing" :status 'running
                 :kind 'compact :created 1000.0))
          alert-text paused-p)
     (cl-letf (((symbol-function 'agent-shell-queue--pause-and-save)
                (lambda (_) (setq paused-p t)))
               ((symbol-function 'alert)
                (lambda (text &rest _) (setq alert-text text))))
       (agent-shell-queue--dispatch-pause-compact item "buf")
       (should paused-p)
       (should (string-match-p "do the thing" alert-text))))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch: shell-eshell (buffer-gone path)

(ert-deftest agent-shell-queue-types/dispatch-shell-eshell-gone-buffer-messages ()
  "dispatch-shell-eshell messages when the target buffer no longer exists."
  (asq-types-test/isolate
   (let* ((item (agent-shell-queue-item--make
                 :id "q-esh-1" :args "ls -la" :status 'running
                 :kind 'shell-eshell :created 1000.0))
          msg)
     (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
       (agent-shell-queue--dispatch-shell-eshell item "dead-buf")
       (should (string-match-p "dead-buf" msg))
       (should (string-match-p "q-esh-1" msg))))))

;;; ─────────────────────────────────────────────────────────────
;;; enqueue-args

(ert-deftest agent-shell-queue-types/enqueue-args-to-bucket ()
  "enqueue-args places item in the named buffer's bucket."
  (asq-types-test/isolate
   (let* ((buf (get-buffer-create " *asq-types-test*")))
     (unwind-protect
         (progn
           (agent-shell-queue--enqueue-args "echo hello" 'shell-eshell buf)
           (let* ((items (cdr (assoc (buffer-name buf)
                                     (agent-shell-queue-store-items
                                      agent-shell-queue--store))))
                  (item (car items)))
             (should (= 1 (length items)))
             (should (equal "echo hello" (agent-shell-queue-item-args item)))
             (should (eq 'shell-eshell (agent-shell-queue-item-kind item)))))
       (kill-buffer buf)))))

(ert-deftest agent-shell-queue-types/enqueue-args-nil-buf-goes-to-unassigned ()
  "enqueue-args with nil buf places item in the unassigned bucket."
  (asq-types-test/isolate
   (agent-shell-queue--enqueue-args "my emacs form" 'emacs-lisp nil)
   (let* ((items (cdr (assoc agent-shell-queue--unassigned-key
                             (agent-shell-queue-store-items
                              agent-shell-queue--store))))
          (item (car items)))
     (should (= 1 (length items)))
     (should (equal "my emacs form" (agent-shell-queue-item-args item)))
     (should (eq 'emacs-lisp (agent-shell-queue-item-kind item))))))

(ert-deftest agent-shell-queue-types/enqueue-args-sets-directory ()
  "enqueue-args records the buffer's default-directory on the item."
  (asq-types-test/isolate
   (let* ((buf (get-buffer-create " *asq-dir-test*")))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (setq-local default-directory "/tmp/test-dir/"))
           (agent-shell-queue--enqueue-args "pwd" 'shell-eshell buf)
           (let* ((items (cdr (assoc (buffer-name buf)
                                     (agent-shell-queue-store-items
                                      agent-shell-queue--store))))
                  (item (car items)))
             (should (equal "/tmp/test-dir/"
                            (agent-shell-queue-item-directory item)))))
       (kill-buffer buf)))))

;;; ─────────────────────────────────────────────────────────────
;;; capture-confirm respects --capture-kind

(ert-deftest agent-shell-queue-types/capture-confirm-uses-capture-kind ()
  "capture-confirm creates an item with the kind set in --capture-kind."
  (asq-types-test/isolate
   (let* ((buf (get-buffer-create " *asq-capture-kind-test*"))
          (capture-buf (get-buffer-create " *asq-capture-buf*"))
          created-kind)
     (unwind-protect
         (with-current-buffer capture-buf
           (insert "echo test")
           (setq agent-shell-queue--capture-target buf
                 agent-shell-queue--capture-background-task nil
                 agent-shell-queue--capture-after-id nil
                 agent-shell-queue--capture-kind 'shell-eshell)
           (cl-letf (((symbol-function 'quit-window) #'ignore)
                     ((symbol-function 'agent-shell-queue-add)
                      (lambda (_ b bg kind)
                        (setq created-kind kind)))
                     ((symbol-function 'agent-shell-queue--make-item)
                      (lambda (args bg kind)
                        (setq created-kind kind)
                        (agent-shell-queue-item--make
                         :id "q-cc-1" :args args :status 'active
                         :kind kind :created 1000.0)))
                     ((symbol-function 'agent-shell-queue--add-item-to-bucket)
                      #'ignore)
                     ((symbol-function 'agent-shell-queue--ensure-subscription)
                      #'ignore)
                     ((symbol-function 'agent-shell-queue--send-next-for-buffer)
                      #'ignore)
                     ((symbol-function 'message) #'ignore))
             (agent-shell-queue-capture-confirm)
             (should (eq 'shell-eshell created-kind))))
       (kill-buffer buf)
       (when (buffer-live-p capture-buf) (kill-buffer capture-buf))))))

;;; ─────────────────────────────────────────────────────────────
;;; Built-in type registrations (smoke tests)

(ert-deftest agent-shell-queue-types/builtin-prompt-registered ()
  "The prompt kind is registered with agent-shell-buffer-p pred."
  (let ((type (agent-shell-queue--type-for-kind 'prompt)))
    (should type)
    (should (equal "agent-shell-prompt" (agent-shell-queue-item-type-label type)))
    (should (eq #'agent-shell-queue--agent-shell-buffer-p
                (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-emacs-lisp-registered ()
  "The emacs-lisp kind is registered with nil buffer-pred (any buffer)."
  (let ((type (agent-shell-queue--type-for-kind 'emacs-lisp)))
    (should type)
    (should (equal "emacs-lisp" (agent-shell-queue-item-type-label type)))
    (should (null (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-shell-eshell-registered ()
  "The shell-eshell kind is registered with eshell-buffer-p pred."
  (let ((type (agent-shell-queue--type-for-kind 'shell-eshell)))
    (should type)
    (should (equal "shell-eshell" (agent-shell-queue-item-type-label type)))
    (should (eq #'agent-shell-queue--eshell-buffer-p
                (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-shell-eat-registered ()
  "The shell-eat kind is registered with eat-buffer-p pred."
  (let ((type (agent-shell-queue--type-for-kind 'shell-eat)))
    (should type)
    (should (equal "shell-eat" (agent-shell-queue-item-type-label type)))
    (should (eq #'agent-shell-queue--eat-buffer-p
                (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-pause-accepts-any-buffer ()
  "pause kind has nil buffer-pred so it accepts any buffer."
  (let ((type (agent-shell-queue--type-for-kind 'pause)))
    (should type)
    (should (null (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-compact-requires-agent-shell ()
  "compact kind requires an agent-shell buffer."
  (let ((type (agent-shell-queue--type-for-kind 'compact)))
    (should type)
    (should (eq #'agent-shell-queue--agent-shell-buffer-p
                (agent-shell-queue-item-type-buffer-pred type)))))

(ert-deftest agent-shell-queue-types/builtin-all-have-dispatch-fn ()
  "Every registered built-in type has a non-nil dispatch-fn."
  (seq-do (lambda (type)
            (should (agent-shell-queue-item-type-dispatch-fn type)))
          agent-shell-queue--item-types))

(ert-deftest agent-shell-queue-types/builtin-all-have-labels ()
  "Every registered built-in type has a non-empty label."
  (seq-do (lambda (type)
            (should (stringp (agent-shell-queue-item-type-label type)))
            (should (not (string-empty-p (agent-shell-queue-item-type-label type)))))
          agent-shell-queue--item-types))

(provide 'test-agent-shell-queue-types)
;;; test-agent-shell-queue-types.el ends here
