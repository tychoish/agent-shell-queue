;;; test-agent-shell-queue-org-capture.el --- Org capture tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify raw Org capture and compatibility with the former refile command.

;;; Code:

(require 'ert)
(require 'agent-shell-queue-org)

(ert-deftest agent-shell-queue-org-capture/keeps-source-by-default ()
  "Capture preserves the source and forwards the complete raw subtree."
  (let ((original (symbol-function 'agent-shell-queue--open-capture))
        captured)
    (unwind-protect
        (progn
          (fset 'agent-shell-queue--open-capture
                (lambda (_target _origin content &rest _)
                  (setq captured content)))
          (with-temp-buffer
            (org-mode)
            (insert "* Task\nText\n** Child\nBody\n* Next\n")
            (goto-char (point-min))
            (agent-shell-queue-org-capture-from-heading)
            (should (equal "* Task\nText\n** Child\nBody" captured))
            (should (equal "* Task" (buffer-substring 1 7)))))
      (fset 'agent-shell-queue--open-capture original))))

(ert-deftest agent-shell-queue-org-capture/obsolete-refile-preserves-prefix ()
  "The obsolete alias retains the explicit remove-original behavior."
  (let ((original (symbol-function 'agent-shell-queue--open-capture)))
    (unwind-protect
        (progn
          (fset 'agent-shell-queue--open-capture #'ignore)
          (with-temp-buffer
            (org-mode)
            (insert "* Task\nBody\n* Next\n")
            (goto-char (point-min))
            (funcall 'agent-shell-queue-org-refile-from-heading t)
            (should (equal "* Next\n" (buffer-string)))))
      (fset 'agent-shell-queue--open-capture original))))

(ert-deftest agent-shell-queue-org-capture/requires-org ()
  "Raw capture validates its source mode."
  (with-temp-buffer
    (should-error (agent-shell-queue-org-capture-from-heading)
                  :type 'user-error)))

(provide 'test-agent-shell-queue-org-capture)
;;; test-agent-shell-queue-org-capture.el ends here
