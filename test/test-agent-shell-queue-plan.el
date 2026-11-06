;;; test-agent-shell-queue-plan.el --- Tests for agent-shell-queue-plan -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -L ../hitl -L . -L test -l ert -l test/test-agent-shell-queue-plan.el \
;;        -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'agent-shell-queue)
(require 'agent-shell-queue-plan)
(require 'hitl-plan)
(require 'test-helper)

(defvar asq-plan-test--locks nil)
(defvar asq-plan-test--applied nil)

(defmacro asq-plan-test/with-env (&rest body)
  "Run BODY with isolated queue, plan directory, and a test domain."
  `(let* ((agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file nil))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store :session-paused nil
            :editing-ids nil :interjection-pending nil))
          (agent-shell-queue--loaded t)
          (hitl-plan-directory (make-temp-file "asq-plan-" t))
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan--domain-registry (copy-hash-table hitl-plan--domain-registry))
          (hitl-plan--op-registry (copy-hash-table hitl-plan--op-registry))
          (asq-plan-test--locks nil)
          (asq-plan-test--applied nil)
          (buf (generate-new-buffer "*asq-plan-test*")))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
                   ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
                   ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
                   ((symbol-function 'alert) #'ignore))
           (hitl-plan-register-domain 'asq-test
             :resolve-target (lambda (tgt) tgt)
             :lock-batch (lambda (b id) (push (list 'lock b id) asq-plan-test--locks))
             :unlock-batch (lambda (b) (push (list 'unlock b) asq-plan-test--locks)))
           (hitl-plan-register-op 'asq-test 'good
             :queueable t
             :apply (lambda (&rest _) (push 'ran asq-plan-test--applied)))
           (hitl-plan-register-op 'asq-test 'bad :apply #'ignore)
           (with-current-buffer buf (setq agent-shell-queue-plan-domain 'asq-test))
           ,@body)
       (kill-buffer buf)
       (delete-directory hitl-plan-directory t))))

(defun asq-plan-test--plan (&rest ops)
  (hitl-plan-create :domain 'asq-test :targets '(a b)
                    :ops (or ops '(good))))

(defun asq-plan-test--items (buf)
  (cdr (assoc (buffer-name buf)
              (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest asq-plan/registered ()
  (let ((type (agent-shell-queue--type-for-kind 'hitl-plan)))
    (should type)
    (should (eq (agent-shell-queue-item-type-dispatch-fn type)
                #'agent-shell-queue-plan-dispatch))))

(ert-deftest asq-plan/buffer-pred ()
  (asq-plan-test/with-env
   (should (agent-shell-queue-plan-buffer-p buf))
   (with-temp-buffer
     (should-not (agent-shell-queue-plan-buffer-p (current-buffer))))
   (let ((agent-shell-queue-plan-buffer-accept-functions (list (lambda (_b) 'asq-test))))
     (with-temp-buffer
       (should (agent-shell-queue-plan-buffer-p (current-buffer)))))))

(ert-deftest asq-plan/enqueue-locks-and-queues ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     (agent-shell-queue-plan-enqueue buf plan)
     (should (equal (list 'lock buf (hitl-plan-id plan)) (car asq-plan-test--locks)))
     (should (= 1 (length (asq-plan-test--items buf))))
     (should (eq 'hitl-plan (agent-shell-queue-item-kind (car (asq-plan-test--items buf)))))
     (should (file-exists-p (hitl-plan-file (hitl-plan-id plan)))))))

(ert-deftest asq-plan/rejects-non-queueable ()
  (asq-plan-test/with-env
   (should-error (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan 'good 'bad))
                 :type 'user-error)
   (should-not asq-plan-test--locks)
   (should-not (asq-plan-test--items buf))))

(ert-deftest asq-plan/per-buffer-cap ()
  (asq-plan-test/with-env
   (let ((agent-shell-queue-plan-max-items 2))
     (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
     (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
     (should-error (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
                   :type 'user-error)
     (should (= 2 (length (asq-plan-test--items buf)))))))

(ert-deftest asq-plan/delay-after ()
  (asq-plan-test/with-env
   (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan) 7)
   (should (= 7 (agent-shell-queue-item-delay-after
                 (car (last (asq-plan-test--items buf))))))))

(ert-deftest asq-plan/dispatch-completes-and-unlocks-last ()
  (asq-plan-test/with-env
   (let ((completed nil))
     (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
     (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
     (setq asq-plan-test--locks nil)
     (cl-letf (((symbol-function 'agent-shell-queue-complete-item)
                (lambda (item _name)
                  (push item completed)
                  (setf (agent-shell-queue-item-status item) 'done))))
       (let ((items (asq-plan-test--items buf)))
         (agent-shell-queue-plan-dispatch (car items) (buffer-name buf))
         (should (= 1 (length completed)))
         (should-not asq-plan-test--locks)
         (agent-shell-queue-plan-dispatch (cadr items) (buffer-name buf))
         (should (= 2 (length completed)))
         (should (equal (list 'unlock buf) (car asq-plan-test--locks)))
         (should (= 4 (length asq-plan-test--applied)))
         (should (cl-every (lambda (p) (eq 'complete (hitl-plan-status p)))
                           (hitl-plan-list))))))))

(ert-deftest asq-plan/dispatch-missing-plan-still-completes ()
  (asq-plan-test/with-env
   (let (completed)
     (cl-letf (((symbol-function 'agent-shell-queue-complete-item)
                (lambda (&rest _) (setq completed t))))
       (should-error (agent-shell-queue-plan-dispatch
                      (agent-shell-queue--make-item "(:plan-id \"nope\")" nil 'hitl-plan)
                      (buffer-name buf)))
       (should completed)))))

(ert-deftest asq-plan/dispatch-finds-plan-in-its-own-directory ()
  "A plan stored outside `hitl-plan-directory' is found again at dispatch."
  (asq-plan-test/with-env
   (let ((other (make-temp-file "asq-plan-other-" t))
         (completed nil))
     (unwind-protect
         (let* ((plan (hitl-plan-create :domain 'asq-test :targets '(a b)
                                        :ops '(good) :directory other))
                (id (hitl-plan-id plan)))
           (agent-shell-queue-plan-enqueue buf plan)
           (let ((item (car (asq-plan-test--items buf))))
             (should (equal other (plist-get (read (agent-shell-queue-item-args item))
                                             :directory)))
             ;; Forget the plan, as after a restart: only the file remains.
             (clrhash hitl-plan--store)
             (should-not (file-exists-p (hitl-plan-file id)))
             (cl-letf (((symbol-function 'agent-shell-queue-complete-item)
                        (lambda (&rest _) (setq completed t))))
               (agent-shell-queue-plan-dispatch item (buffer-name buf)))
             (should completed)
             (should (= 2 (length asq-plan-test--applied)))
             (should (eq 'complete (hitl-plan-status (hitl-plan-get id other))))))
       (delete-directory other t)))))

(ert-deftest asq-plan/input-offers-plans-from-domain-directory ()
  (asq-plan-test/with-env
   (let* ((other (make-temp-file "asq-plan-other-" t))
          (picked nil))
     (setf (hitl-plan-domain-directory (hitl-plan-get-domain 'asq-test))
           (lambda () other))
     (unwind-protect
         (let ((plan (hitl-plan-create :domain 'asq-test :targets '(a)
                                       :ops '(good))))
           (clrhash hitl-plan--store)
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (_p table &rest _)
                        (setq picked (all-completions "" table))
                        (car picked))))
             (agent-shell-queue-plan--input buf))
           (should (equal picked (list (hitl-plan-id plan))))
           (should (= 1 (length (asq-plan-test--items buf)))))
       (delete-directory other t)))))

(ert-deftest asq-plan/rejects-bad-status ()
  (asq-plan-test/with-env
   (dolist (st '(running complete discarded))
     (let ((plan (asq-plan-test--plan)))
       (setf (hitl-plan-status plan) st)
       (should-error (agent-shell-queue-plan-enqueue buf plan) :type 'user-error)))
   (let ((plan (asq-plan-test--plan)))
     (setf (hitl-plan-status plan) 'paused)
     (agent-shell-queue-plan-enqueue buf plan))
   (should (= 1 (length (asq-plan-test--items buf))))))

(ert-deftest asq-plan/rejects-already-queued ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     (agent-shell-queue-plan-enqueue buf plan)
     (should-error (agent-shell-queue-plan-enqueue buf plan) :type 'user-error)
     (should (= 1 (length (asq-plan-test--items buf)))))))

(ert-deftest asq-plan/enqueue-by-id-searches-domain-store ()
  (asq-plan-test/with-env
   (let ((other (make-temp-file "asq-plan-other-" t)))
     (setf (hitl-plan-domain-directory (hitl-plan-get-domain 'asq-test)) other)
     (unwind-protect
         (let ((plan (hitl-plan-create :domain 'asq-test :targets '(a) :ops '(good))))
           (clrhash hitl-plan--store)
           (agent-shell-queue-plan-enqueue buf (hitl-plan-id plan))
           (should (= 1 (length (asq-plan-test--items buf)))))
       (delete-directory other t)))))

(ert-deftest asq-plan/max-items-nil-or-nonpositive-is-unlimited ()
  (dolist (max '(nil 0 -1))
    (asq-plan-test/with-env
     (let ((agent-shell-queue-plan-max-items max))
       (dotimes (_ 7) (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan)))
       (should (= 7 (length (asq-plan-test--items buf))))))))


(ert-deftest asq-plan-test/accept-buffer-slot ()
  (asq-plan-test/with-env
    (let ((other (generate-new-buffer "*asq-accept*")))
      (unwind-protect
          (progn
            (should-not (agent-shell-queue-plan-buffer-p other))
            (hitl-plan-register-domain 'asq-acc
              :accept-buffer (lambda (b) (equal (buffer-name b) "*asq-accept*")))
            (should (agent-shell-queue-plan-buffer-p other))
            ;; fallback to buffer-local mechanism still works
            (should (agent-shell-queue-plan-buffer-p buf)))
        (kill-buffer other)))))

;;; Review follow-ups

(ert-deftest asq-plan/accept-functions-must-name-a-domain ()
  "A hook function accepts by returning a registered domain name."
  (asq-plan-test/with-env
   (with-temp-buffer
     (let ((buf2 (current-buffer)))
       (dolist (result '(t "not-a-domain" unregistered 42 nil))
         (let ((agent-shell-queue-plan-buffer-accept-functions (list (lambda (_b) result))))
           (should-not (agent-shell-queue-plan-buffer-p buf2))))
       (let ((agent-shell-queue-plan-buffer-accept-functions (list (lambda (_b) 'asq-test))))
         (should (eq (hitl-plan-get-domain 'asq-test)
                     (agent-shell-queue-plan--domain-for-buffer buf2))))
       ;; A string name works, and a rejecting function does not block the next.
       (let ((agent-shell-queue-plan-buffer-accept-functions
              (list (lambda (_b) nil) (lambda (_b) t) (lambda (_b) "asq-test"))))
         (should (eq (hitl-plan-get-domain 'asq-test)
                     (agent-shell-queue-plan--domain-for-buffer buf2))))))))

(ert-deftest asq-plan/domain-order-is-registration-order ()
  "Several accepting domains: the first registered one owns the buffer."
  (asq-plan-test/with-env
   (hitl-plan-register-domain 'asq-a :accept-buffer (lambda (_b) t))
   (hitl-plan-register-domain 'asq-b :accept-buffer (lambda (_b) t))
   (with-temp-buffer
     (should (eq 'asq-a (hitl-plan-domain-name
                         (agent-shell-queue-plan--domain-for-buffer (current-buffer))))))))

(ert-deftest asq-plan/enqueue-via-hook-accepted-buffer ()
  (asq-plan-test/with-env
   (with-temp-buffer
     (rename-buffer "*asq-hook-buf*" t)
     (let ((agent-shell-queue-plan-buffer-accept-functions (list (lambda (_b) 'asq-test)))
           (plan (asq-plan-test--plan)))
       (agent-shell-queue-plan-enqueue (current-buffer) plan)
       (should (equal (list 'lock (current-buffer) (hitl-plan-id plan)) (car asq-plan-test--locks)))
       (should (= 1 (length (asq-plan-test--items (current-buffer)))))))))

(ert-deftest asq-plan/item-args-carry-no-domain ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     (agent-shell-queue-plan-enqueue buf plan)
     (let ((args (read (agent-shell-queue-item-args (car (asq-plan-test--items buf))))))
       (should (equal (hitl-plan-id plan) (plist-get args :plan-id)))
       (should-not (plist-member args :domain))))))

(ert-deftest asq-plan/unlock-on-enqueue-failure ()
  "If queueing fails after the lock, the batch is unlocked and the error escapes."
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     (cl-letf (((symbol-function 'agent-shell-queue--enqueue-args)
                (lambda (&rest _) (error "queue boom"))))
       (should-error (agent-shell-queue-plan-enqueue buf plan)))
     (should (equal (list (list 'unlock buf) (list 'lock buf (hitl-plan-id plan)))
                    asq-plan-test--locks))
     (should-not (asq-plan-test--items buf)))))

(ert-deftest asq-plan/delay-after-ignores-non-numbers-and-targets-new-item ()
  (asq-plan-test/with-env
   (let ((agent-shell-queue-plan-max-items nil))
   (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan) 9)
   (dolist (bad '(nil "5" 0 -3 foo))
     (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan) bad))
   (let ((delays (mapcar #'agent-shell-queue-item-delay-after (asq-plan-test--items buf))))
     (should (eql 9 (car delays)))
     (should-not (delq nil (mapcar (lambda (d) (and (numberp d) (> d 0) d)) (cdr delays))))))))

(ert-deftest asq-plan/parse-args ()
  (should (equal '(:plan-id "p1") (agent-shell-queue-plan--parse-args '(:plan-id "p1"))))
  (should (equal '(:plan-id "p1" :directory "/d")
                 (agent-shell-queue-plan--parse-args "(:plan-id \"p1\" :directory \"/d\")")))
  (should (equal '(:plan-id "bare") (agent-shell-queue-plan--parse-args "  bare  ")))
  ;; Malformed or non-plist text is taken as the ID.
  (should (equal '(:plan-id "(:plan-id \"x\"") (agent-shell-queue-plan--parse-args "(:plan-id \"x\"")))
  (should (equal '(:plan-id "(1 2 3)") (agent-shell-queue-plan--parse-args "(1 2 3)")))
  (should-not (agent-shell-queue-plan--parse-args nil))
  (should-not (agent-shell-queue-plan--parse-args 42)))

(ert-deftest asq-plan/parse-args-does-not-eval ()
  (defvar asq-plan-test--evaled nil)
  (setq asq-plan-test--evaled nil)
  (let ((r (agent-shell-queue-plan--parse-args
            "(:plan-id #.(setq asq-plan-test--evaled t))")))
    (should-not asq-plan-test--evaled)
    (should (plist-get r :plan-id))))

(ert-deftest asq-plan/pending-count ()
  (asq-plan-test/with-env
   (should (= 0 (agent-shell-queue-plan--pending-count (buffer-name buf))))
   (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
   (agent-shell-queue-plan-enqueue buf (asq-plan-test--plan))
   (agent-shell-queue--enqueue-args "echo hi" 'shell-eshell buf)
   (should (= 2 (agent-shell-queue-plan--pending-count (buffer-name buf))))
   (should (= 0 (agent-shell-queue-plan--pending-count "*other*")))
   (let ((item (car (asq-plan-test--items buf))))
     (setf (agent-shell-queue-item-status item) 'running)
     (should (= 2 (agent-shell-queue-plan--pending-count (buffer-name buf))))
     (setf (agent-shell-queue-item-status item) 'done)
     (should (= 1 (agent-shell-queue-plan--pending-count (buffer-name buf)))))
   (let ((agent-shell-queue--store nil))
     (should (= 0 (agent-shell-queue-plan--pending-count (buffer-name buf)))))))

(ert-deftest asq-plan/queued-p ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     (should-not (agent-shell-queue-plan--queued-p (hitl-plan-id plan)))
     (agent-shell-queue-plan-enqueue buf plan)
     (should (agent-shell-queue-plan--queued-p (hitl-plan-id plan)))
     (should-not (agent-shell-queue-plan--queued-p "other-id"))
     (setf (agent-shell-queue-item-status (car (asq-plan-test--items buf))) 'done)
     (should-not (agent-shell-queue-plan--queued-p (hitl-plan-id plan)))
     (let ((agent-shell-queue--store nil))
       (should-not (agent-shell-queue-plan--queued-p (hitl-plan-id plan)))))))

(ert-deftest asq-plan/find ()
  (asq-plan-test/with-env
   (let ((other (make-temp-file "asq-plan-other-" t)))
     (unwind-protect
         (progn
           (setf (hitl-plan-domain-directory (hitl-plan-get-domain 'asq-test)) other)
           (let ((in-default (hitl-plan-create :domain 'asq-test :targets '(a) :ops '(good)
                                               :directory hitl-plan-directory))
                 (in-store (hitl-plan-create :domain 'asq-test :targets '(a) :ops '(good))))
             (clrhash hitl-plan--store)
             (should (equal (hitl-plan-id in-default)
                            (hitl-plan-id (agent-shell-queue-plan--find (hitl-plan-id in-default)))))
             (clrhash hitl-plan--store)
             (should (equal (hitl-plan-id in-store)
                            (hitl-plan-id (agent-shell-queue-plan--find (hitl-plan-id in-store)))))
             (should-not (agent-shell-queue-plan--find "nope"))))
       (delete-directory other t)))))

(ert-deftest asq-plan/unlock ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--plan)))
     ;; Through the plan's own domain.
     (agent-shell-queue-plan--unlock buf plan)
     (should (equal (list (list 'unlock buf)) asq-plan-test--locks))
     ;; Without a plan: the domain that accepts the buffer.
     (setq asq-plan-test--locks nil)
     (agent-shell-queue-plan--unlock buf nil)
     (should (equal (list (list 'unlock buf)) asq-plan-test--locks))
     ;; A domain without :unlock-batch, or no domain at all, is a no-op.
     (setq asq-plan-test--locks nil)
     (hitl-plan-register-domain 'asq-plain :resolve-target #'identity)
     (agent-shell-queue-plan--unlock buf (hitl-plan-create :domain 'asq-plain :targets '(a)
                                                           :ops '(good)))
     (with-temp-buffer (agent-shell-queue-plan--unlock (current-buffer) nil))
     (should-not asq-plan-test--locks))))

(ert-deftest asq-plan/candidates ()
  (asq-plan-test/with-env
   (let* ((other (make-temp-file "asq-plan-other-" t))
          (mine (asq-plan-test--plan))
          (paused (asq-plan-test--plan))
          (done (asq-plan-test--plan))
          (foreign nil))
     (unwind-protect
         (progn
           (hitl-plan-register-op 'other-dom 'good :queueable t :apply #'ignore)
           (hitl-plan-register-domain 'other-dom :resolve-target #'identity
                                      :directory other)
           (setq foreign (hitl-plan-create :domain 'other-dom :targets '(a) :ops '(good)))
           (setf (hitl-plan-status paused) 'paused
                 (hitl-plan-status done) 'complete)
           ;; The buffer belongs to asq-test: only its draft or paused plans.
           (should (equal (sort (list (hitl-plan-id mine) (hitl-plan-id paused)) #'string<)
                          (sort (mapcar #'hitl-plan-id (agent-shell-queue-plan--candidates buf))
                                #'string<)))
           ;; No accepting domain: every domain's plans, each listed once.
           (with-temp-buffer
             (let ((ids (mapcar #'hitl-plan-id
                                (agent-shell-queue-plan--candidates (current-buffer)))))
               (should (member (hitl-plan-id foreign) ids))
               (should (member (hitl-plan-id mine) ids))
               (should-not (member (hitl-plan-id done) ids))
               (should (= (length ids) (length (delete-dups (copy-sequence ids))))))))
       (delete-directory other t)))))

;;; Security regression tests

(defun asq-plan-test--mcp-plan ()
  (hitl-plan-create :domain 'asq-test :targets '(a b) :ops '(good)
                    :metadata '(:origin mcp)))

(defun asq-plan-test--dispatch (plan buf)
  "Dispatch an item for PLAN on BUF with item completion stubbed."
  (cl-letf (((symbol-function 'agent-shell-queue-complete-item) #'ignore))
    (agent-shell-queue-plan-dispatch
     (agent-shell-queue--make-item
      (prin1-to-string (list :plan-id (hitl-plan-id plan))) nil 'hitl-plan)
     (buffer-name buf))))

(ert-deftest asq-plan/sec-dispatch-refuses-unapproved-mcp-plan ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--mcp-plan))
         (hitl-plan-agent-may-apply nil))
     (should-error (asq-plan-test--dispatch plan buf))
     (should-not asq-plan-test--applied)
     (should (eq 'draft (hitl-plan-status plan))))))

(ert-deftest asq-plan/sec-dispatch-runs-mcp-plan-when-may-apply ()
  (asq-plan-test/with-env
   (let ((plan (asq-plan-test--mcp-plan))
         (hitl-plan-agent-may-apply t))
     (asq-plan-test--dispatch plan buf)
     (should (= 2 (length asq-plan-test--applied))))))

(ert-deftest asq-plan/sec-dispatch-requires-draft-or-paused ()
  (asq-plan-test/with-env
   (dolist (status '(complete discarded running))
     (let ((plan (hitl-plan-create :domain 'asq-test :targets '(a) :ops '(good)
                                   :status status)))
       (should-error (asq-plan-test--dispatch plan buf))
       (should-not asq-plan-test--applied)))))

(ert-deftest asq-plan/sec-enqueue-approves-only-when-asked ()
  (asq-plan-test/with-env
   (let ((p1 (asq-plan-test--mcp-plan))
         (p2 (asq-plan-test--mcp-plan))
         (hitl-plan-agent-may-apply nil))
     (agent-shell-queue-plan-enqueue buf p1)
     (should-not (plist-get (hitl-plan-metadata p1) :approved))
     (agent-shell-queue-plan-enqueue buf p2 nil t)
     (should (plist-get (hitl-plan-metadata p2) :approved))
     (should (eq 'mcp (plist-get (hitl-plan-metadata p2) :origin)))
     ;; persisted
     (clrhash hitl-plan--store)
     (should (plist-get (hitl-plan-metadata (hitl-plan-load (hitl-plan-id p2))) :approved))
     ;; The approved one dispatches; the approval is consumed.
     (setq p2 (hitl-plan-get (hitl-plan-id p2)))
     (asq-plan-test--dispatch p2 buf)
     (should (= 2 (length asq-plan-test--applied)))
     (should-not (plist-get (hitl-plan-metadata (hitl-plan-get (hitl-plan-id p2))) :approved))
     ;; The unapproved one is refused.
     (setq asq-plan-test--applied nil)
     (should-error (asq-plan-test--dispatch p1 buf))
     (should-not asq-plan-test--applied))))

(ert-deftest asq-plan/sec-reenqueue-guard ()
  (asq-plan-test/with-env
   (let* ((hitl-plan-agent-may-apply nil)
          (plan (asq-plan-test--mcp-plan))
          (item nil))
     (agent-shell-queue-plan-enqueue buf plan nil t)
     (setq item (car (last (asq-plan-test--items buf))))
     ;; Run once so the item is done and the plan complete.
     (asq-plan-test--dispatch plan buf)
     (setf (agent-shell-queue-item-status item) 'done)
     (should (eq 'complete (hitl-plan-status plan)))
     (should-error (agent-shell-queue-reenqueue (agent-shell-queue-item-id item)))
     (should (= 1 (length (asq-plan-test--items buf))))
     ;; A paused mcp plan that is no longer approved is refused too.
     (setf (hitl-plan-status plan) 'paused)
     (should-error (agent-shell-queue-reenqueue (agent-shell-queue-item-id item)))
     ;; ... but passes again when the user approves it.
     (setf (hitl-plan-metadata plan) '(:origin mcp :approved t))
     (should (agent-shell-queue-reenqueue (agent-shell-queue-item-id item)))
     (should (= 2 (length (asq-plan-test--items buf)))))))

(ert-deftest asq-plan/sec-reenqueue-guard-slot ()
  (let ((type (agent-shell-queue--type-for-kind 'hitl-plan)))
    (should (functionp (agent-shell-queue-item-type-reenqueue-guard type)))))

(provide 'test-agent-shell-queue-plan)
;;; test-agent-shell-queue-plan.el ends here
