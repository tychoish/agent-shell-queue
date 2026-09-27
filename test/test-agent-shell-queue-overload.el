;;; test-agent-shell-queue-overload.el --- Tests for agent-shell-queue-overload -*- lexical-binding: t -*-

;; Author: tycho garen
;; Keywords: tools, test

;;; Commentary:
;; ERT test suite for agent-shell-queue-overload module.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'agent-shell-queue-core)
(require 'agent-shell-queue-ui)
(require 'agent-shell-prompt-queue nil t)
(require 'agent-shell-queue-overload)

(defmacro with-test-queue-overload-env (&rest body)
  "Run BODY in an isolated test environment with a clean store."
  `(let* ((temp-dir (make-temp-file "asq-overload-test-" t))
          (temp-file (expand-file-name "test-queue.json" temp-dir))
          (agent-shell-queue-file temp-file)
          (agent-shell-queue-state-file-function (lambda () temp-file))
          (agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'json :file temp-file))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store
            :session-paused nil
            :editing-ids nil
            :interjection-pending nil))
          (agent-shell-queue--loaded t)
          (agent-shell-queue-overload-mode nil)
          (agent-shell-queue-overload-entry-method 'minibuffer)
          (test-buf (generate-new-buffer "*asq-test-shell*")))
     (unwind-protect
         (with-current-buffer test-buf
           (setq major-mode 'agent-shell-mode)
           ,@body)
       (when (buffer-live-p test-buf)
         (kill-buffer test-buf))
       (when agent-shell-queue-overload-mode
         (agent-shell-queue-overload-mode -1))
       (delete-directory temp-dir t))))

;;; Mode Lifecycle Tests

(ert-deftest asq-overload/mode-toggle-lifecycle ()
  "Test enabling and disabling `agent-shell-queue-overload-mode'."
  (with-test-queue-overload-env
   ;; Initial state
   (should-not agent-shell-queue-overload-mode)

   ;; Enable mode
   (agent-shell-queue-overload-mode 1)
   (should agent-shell-queue-overload-mode)

   ;; Check upstream hook variables if bound
   (when (boundp 'agent-shell-prompt-queue-enqueue-function)
     (should (eq agent-shell-prompt-queue-enqueue-function #'agent-shell-queue-overload-enqueue)))
   (when (boundp 'agent-shell-prompt-queue-resume-function)
     (should (eq agent-shell-prompt-queue-resume-function #'agent-shell-queue-overload-resume)))
   (when (boundp 'agent-shell-prompt-queue-remove-function)
     (should (eq agent-shell-prompt-queue-remove-function #'agent-shell-queue-overload-remove)))
   (when (boundp 'agent-shell-prompt-queue-function)
     (should (eq agent-shell-prompt-queue-function #'agent-shell-queue-overload-prompt-queue)))
   (when (boundp 'agent-shell-prompt-queue-read-function)
     (should (eq agent-shell-prompt-queue-read-function #'agent-shell-queue-overload-read)))

   ;; Check minibuffer setup hook
   (should (memq #'agent-shell-queue-overload-setup-minibuffer
                 agent-shell-prompt-queue-setup-minibuffer-functions))

   ;; Disable mode
   (agent-shell-queue-overload-mode -1)
   (should-not agent-shell-queue-overload-mode)

   ;; Check hooks cleared
   (when (boundp 'agent-shell-prompt-queue-enqueue-function)
     (should-not agent-shell-prompt-queue-enqueue-function))
   (when (boundp 'agent-shell-prompt-queue-resume-function)
     (should-not agent-shell-prompt-queue-resume-function))
   (when (boundp 'agent-shell-prompt-queue-remove-function)
     (should-not agent-shell-prompt-queue-remove-function))
   (should-not (memq #'agent-shell-queue-overload-setup-minibuffer
                     agent-shell-prompt-queue-setup-minibuffer-functions))))

;;; Enqueueing Tests

(ert-deftest asq-overload/enqueue-when-busy-persists-to-asq ()
  "Enqueueing while shell is busy creates an ASQ item with disk persistence."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t)))
     (let ((item (agent-shell-queue-overload-enqueue "Deploy application to production" test-buf)))
       (should item)
       (should (agent-shell-queue-item-p item))
       (should (equal (agent-shell-queue-item-args item) "Deploy application to production"))
       (should (eq (agent-shell-queue-item-status item) 'active))
       (should (string-prefix-p "q" (agent-shell-queue-item-id item)))

       ;; Verify item is present in ASQ store under test-buf's bucket
       (let ((stored (agent-shell-queue-overload--active-items test-buf)))
         (should (= (length stored) 1))
         (should (equal (agent-shell-queue-item-id (car stored))
                        (agent-shell-queue-item-id item))))

       ;; Verify file persistence
       (should (file-exists-p temp-file))))))

(ert-deftest asq-overload/enqueue-when-idle-submits-immediately ()
  "Enqueueing while shell is idle submits directly without queuing in ASQ."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((submitted nil))
     (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
               ((symbol-function 'agent-shell--insert-to-shell-buffer)
                (lambda (&rest args) (setq submitted (plist-get args :text)))))
       (agent-shell-queue-overload-enqueue "Show git status" test-buf)
       (should (equal submitted "Show git status"))
       ;; Store should remain empty
       (should (null (agent-shell-queue-overload--active-items test-buf)))))))

;;; Escape to Capture Buffer Tests

(ert-deftest asq-overload/minibuffer-escape-to-capture ()
  "Minibuffer escape hands off typed contents to an ASQ capture buffer."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((captured-target nil)
         (captured-text nil))
     ;; Mock open capture
     (cl-letf (((symbol-function 'agent-shell-queue--open-capture)
                (lambda (target &optional _origin text &rest _args)
                  (setq captured-target target
                        captured-text text))))
       ;; Simulate typing into minibuffer and pressing escape key
       (let ((agent-shell-queue-overload--current-shell-buffer test-buf)
             (agent-shell-queue-overload--escape-handoff nil))
         (cl-letf (((symbol-function 'minibuffer-contents)
                    (lambda () "Partially typed prompt text that needs multi-line edits"))
                   ((symbol-function 'abort-recursive-edit)
                    (lambda () (signal 'quit nil))))
           ;; Trigger escape
           (condition-case nil
               (agent-shell-queue-overload-escape-to-capture)
             (quit nil))
           (should agent-shell-queue-overload--escape-handoff)
           (should (eq (car agent-shell-queue-overload--escape-handoff) test-buf))
           (should (equal (cdr agent-shell-queue-overload--escape-handoff)
                          "Partially typed prompt text that needs multi-line edits"))

           ;; Test that reader catches handoff and opens capture buffer
           (cl-letf (((symbol-function 'read-string)
                      (lambda (&rest _)
                        (setq agent-shell-queue-overload--escape-handoff
                              (cons test-buf "Prompt transferred to capture buffer"))
                        (signal 'quit nil))))
             (agent-shell-queue-overload-read)
             (should (eq captured-target test-buf))
             (should (equal captured-text "Prompt transferred to capture buffer")))))))))

(ert-deftest asq-overload/capture-buffer-entry-method ()
  "When `agent-shell-queue-overload-entry-method' is capture-buffer, opens capture directly."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((agent-shell-queue-overload-entry-method 'capture-buffer)
         (opened-target nil))
     (cl-letf (((symbol-function 'agent-shell-queue--open-capture)
                (lambda (target &rest _) (setq opened-target target)))
               ((symbol-function 'agent-shell--shell-buffer)
                (lambda (&rest _) test-buf)))
       (agent-shell-queue-overload-prompt-queue :capture-buffer)
       ;; Also test via reader
       (agent-shell-queue-overload-read)
       (should (eq opened-target test-buf))))))

;;; Introspection & Display Tests

(ert-deftest asq-overload/display-renders-asq-items ()
  "Displaying queue shows pending ASQ items in fragment or messages."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((item1 (agent-shell-queue-add "First task" test-buf))
         (item2 (agent-shell-queue-add "Second task" test-buf))
         (fragment-body nil))
     (cl-letf (((symbol-function 'agent-shell--shell-buffer) (lambda (&rest _) test-buf))
               ((symbol-function 'agent-shell--state) (lambda () '((:request-count . 5))))
               ((symbol-function 'agent-shell--update-fragment)
                (lambda (&rest args) (setq fragment-body (plist-get args :body)))))
       (agent-shell-queue-overload-display)
       (should fragment-body)
       (should (string-match-p "Pending prompts: 2 (agent-shell-queue)" fragment-body))
       (should (string-match-p (agent-shell-queue-item-id item1) fragment-body))
       (should (string-match-p (agent-shell-queue-item-id item2) fragment-body))
       (should (string-match-p "First task" fragment-body))
       (should (string-match-p "Second task" fragment-body))))))

;;; Pausing & Resume Tests

(ert-deftest asq-overload/resume-unpauses-session ()
  "Resuming an ASQ-paused session restores and unpauses it."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((buf-name (buffer-name test-buf))
         (resumed nil))
     ;; Pause session in ASQ
     (agent-shell-queue--session-pause-name buf-name)
     (should (member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))

     (cl-letf (((symbol-function 'agent-shell--shell-buffer) (lambda (&rest _) test-buf))
               ((symbol-function 'agent-shell-queue-session-resume)
                (lambda (b) (setq resumed b))))
       (agent-shell-queue-overload-resume)
       (should (equal resumed buf-name))))))

;;; Removal Tests

(ert-deftest asq-overload/remove-specific-item ()
  "Removing an item by index removes it from the ASQ store."
  (with-test-queue-overload-env
   (agent-shell-queue-overload-mode 1)
   (let ((item1 (agent-shell-queue-add "Item 1" test-buf))
         (item2 (agent-shell-queue-add "Item 2" test-buf)))
     (should (= (length (agent-shell-queue-overload--active-items test-buf)) 2))
     (cl-letf (((symbol-function 'agent-shell--shell-buffer) (lambda (&rest _) test-buf))
               ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
       ;; Remove index 0 (item1)
       (agent-shell-queue-overload-remove 0)
       (let ((remaining (agent-shell-queue-overload--active-items test-buf)))
         (should (= (length remaining) 1))
         (should (equal (agent-shell-queue-item-id (car remaining))
                        (agent-shell-queue-item-id item2))))))))

(provide 'test-agent-shell-queue-overload)
;;; test-agent-shell-queue-overload.el ends here
