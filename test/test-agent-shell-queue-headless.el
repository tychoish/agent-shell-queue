;;; test-agent-shell-queue-headless.el --- Tests for headless queue core -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue-core)

(ert-deftest agent-shell-queue-headless/core-loads ()
  "Verify agent-shell-queue-core is loaded and operational."
  (should (featurep 'agent-shell-queue-core)))

(ert-deftest agent-shell-queue-headless/basic-enqueue-and-item-ops ()
  "Verify core queue data structures and operations function headlessly."
  (let* ((agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--queue
          (agent-shell-queue-queue--make))
         (item (agent-shell-queue--make-item "headless task" nil 'prompt)))
    (should (agent-shell-queue-item-p item))
    (should (equal (agent-shell-queue-item-args item) "headless task"))
    (should (eq (agent-shell-queue-item-status item) 'active))

    ;; Add to bucket
    (agent-shell-queue--add-item-to-bucket "test-shell" item)
    (let ((items (cdr (assoc "test-shell" (agent-shell-queue-store-items agent-shell-queue--store)))))
      (should (= (length items) 1))
      (should (equal (agent-shell-queue-item-id (car items)) (agent-shell-queue-item-id item))))

    ;; Safe refresh buffer does not crash
    (agent-shell-queue--refresh-buffer)

    ;; Status string formatting works headlessly
    (should (equal "scheduled" (agent-shell-queue--status-string item)))

    ;; Modify item status
    (setf (agent-shell-queue-item-status item) 'done)
    (should (equal "done" (agent-shell-queue--status-string item)))))

(ert-deftest agent-shell-queue-headless/serialization-helpers ()
  "Verify serialization helpers operate in headless mode."
  (let* ((item (agent-shell-queue--make-item "test-headless-item" nil 'prompt))
         (plist (agent-shell-queue--serialize-single-item item "test-target" 'plist))
         (json (agent-shell-queue--serialize-single-item item "test-target" 'json)))
    (should (stringp plist))
    (should (string-search "test-headless-item" plist))
    (should (stringp json))
    (should (string-search "test-headless-item" json))))

(provide 'test-agent-shell-queue-headless)
;;; test-agent-shell-queue-headless.el ends here
