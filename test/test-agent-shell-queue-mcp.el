;;; test-agent-shell-queue-mcp.el --- Tests for agent-shell-queue-mcp -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-shell-queue-mcp)

(defun asq-mcp-test--call (tool args)
  "Invoke asq TOOL's handler with ARGS and return its result."
  (let ((tool-obj (map-elt (mcpkit-service-tools agent-shell-queue-mcp-service) (format "%s" tool)))
        (outcome nil))
    (funcall (mcpkit-tool-handler tool-obj) args
             (lambda (err res) (setq outcome (cons err res))))
    (when (car outcome) (error "%s" (car outcome)))
    (cdr outcome)))

(defmacro asq-mcp-test--with-stubs (item &rest body)
  "Run BODY with the queue accessors stubbed around a single ITEM in \"buf\"."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-queue-mcp--ensure-queue) #'ignore)
             ((symbol-function 'agent-shell-queue-get-item-by-id)
              (lambda (id) (and (equal id "known") (cons "buf" ,item)))))
     ,@body))

(ert-deftest asq-mcp-tools-registered-test ()
  (dolist (tool '(asq_list_items asq_get_item asq_enqueue asq_reenqueue asq_interject
                  asq_cancel asq_defer asq_finish asq_edit asq_unblock asq_move
                  asq_set_background asq_pause asq_resume))
    (should (map-elt (mcpkit-service-tools agent-shell-queue-mcp-service) (format "%s" tool)))))

(ert-deftest asq-mcp-unknown-id-errors-test ()
  (asq-mcp-test--with-stubs nil
    (dolist (tool '(asq_edit asq_unblock asq_move asq_set_background))
      (let ((res (asq-mcp-test--call tool '(:id "nope" :prompt "p" :direction "up" :background t))))
        (should (equal (plist-get res :status) "error"))))))

(ert-deftest asq-mcp-move-dispatch-test ()
  (let (moved)
    (asq-mcp-test--with-stubs nil
      (cl-letf (((symbol-function 'agent-shell-queue-move-up) (lambda (id) (push (list 'up id) moved)))
                ((symbol-function 'agent-shell-queue-move-down) (lambda (id) (push (list 'down id) moved))))
        (asq-mcp-test--call 'asq_move '(:id "known" :direction "down"))
        (should (equal moved '((down "known"))))
        (should (equal (plist-get (asq-mcp-test--call 'asq_move '(:id "known" :direction "sideways"))
                                  :status)
                       "error"))))))

(ert-deftest asq-mcp-pause-resume-scope-test ()
  (let (calls)
    (cl-letf (((symbol-function 'agent-shell-queue-mcp--ensure-queue) #'ignore)
              ((symbol-function 'agent-shell-queue-pause) (lambda () (push 'pause calls)))
              ((symbol-function 'agent-shell-queue-resume) (lambda () (push 'resume calls)))
              ((symbol-function 'agent-shell-queue-session-pause) (lambda (_) (push 'spause calls)))
              ((symbol-function 'agent-shell-queue-session-resume) (lambda (_) (push 'sresume calls))))
      (should (equal (plist-get (asq-mcp-test--call 'asq_pause nil) :scope) "global"))
      (should (equal (plist-get (asq-mcp-test--call 'asq_resume nil) :scope) "global"))
      (with-temp-buffer
        (rename-buffer "asq-mcp-test-buf" t)
        (let ((name (buffer-name)))
          (should (equal (plist-get (asq-mcp-test--call 'asq_pause `(:buffer ,name)) :scope) "session"))
          (should (equal (plist-get (asq-mcp-test--call 'asq_resume `(:buffer ,name)) :scope) "session"))))
      (should (equal (plist-get (asq-mcp-test--call 'asq_pause '(:buffer "no-such-buffer")) :status)
                     "error"))
      (should (equal (nreverse calls) '(pause resume spause sresume))))))

(provide 'test-agent-shell-queue-mcp)
;;; test-agent-shell-queue-mcp.el ends here
