;;; test-agent-shell-ask.el --- ERT unit tests for agent-shell-ask -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add repository directory to load-path
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'agent-shell-ask)
(require 'agent-shell-queue)

(ert-deftest agent-shell-ask-test-create-and-get ()
  "Test creating and retrieving questions in agent-shell-ask store."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q (agent-shell-ask-create
              :prompt "Proceed with deployment?"
              :kind 'boolean
              :id "test-q1")))
      (should (equal (agent-shell-ask-question-id q) "test-q1"))
      (should (eq (agent-shell-ask-question-kind q) 'boolean))
      (should (eq (agent-shell-ask-question-status q) 'pending))
      (should (equal (agent-shell-ask-get "test-q1") q)))))

(ert-deftest agent-shell-ask-test-cursor-iteration ()
  "Test cursor-driven queue iteration (agent-shell-ask-cursor-next)."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal))
        (agent-shell-ask-cursors (make-hash-table :test #'equal)))
    (let ((q1 (agent-shell-ask-create :prompt "Question 1" :id "q1"))
          (q2 (agent-shell-ask-create :prompt "Question 2" :id "q2")))
      ;; First cursor call should yield q1
      (let ((c1 (agent-shell-ask-cursor-next "c1")))
        (should (equal (agent-shell-ask-question-id c1) "q1")))
      ;; Second cursor call should yield q2
      (let ((c2 (agent-shell-ask-cursor-next "c1")))
        (should (equal (agent-shell-ask-question-id c2) "q2")))
      ;; Answering q1 should keep q2 as next unread for new cursor
      (agent-shell-ask-answer "q1" "ans1")
      (let ((c3 (agent-shell-ask-cursor-next "c2")))
        (should (equal (agent-shell-ask-question-id c3) "q2"))))))

(ert-deftest agent-shell-ask-test-answering-and-followup ()
  "Test answering a question and executing follow-up function callback."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal))
        (called-arg nil))
    (defalias 'test-ask-callback (lambda (resp &rest _args) (setq called-arg resp)))
    (let ((q (agent-shell-ask-create
              :prompt "Choose target:"
              :kind 'single-choice
              :options '("staging" "production")
              :id "q-followup"
              :followup-action '(:type :function :function test-ask-callback))))
      (agent-shell-ask-answer "q-followup" "production")
      (should (eq (agent-shell-ask-question-status q) 'answered))
      (should (equal (agent-shell-ask-question-response q) "production"))
      (should (equal called-arg "production")))))

(ert-deftest agent-shell-ask-test-multi-choice-support ()
  "Test multi-choice question kind and response handling."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q (agent-shell-ask-create
              :prompt "Select features to enable:"
              :kind 'multi-choice
              :options '("featA" "featB" "featC")
              :id "q-multi")))
      (should (eq (agent-shell-ask-question-kind q) 'multi-choice))
      (agent-shell-ask-answer "q-multi" '("featA" "featC"))
      (should (equal (agent-shell-ask-question-response q) '("featA" "featC"))))))

(ert-deftest agent-shell-ask-test-serialization ()
  "Test question store serialization and deserialization."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (agent-shell-ask-create :prompt "P1" :id "q1" :kind 'text)
    (agent-shell-ask-create :prompt "P2" :id "q2" :kind 'boolean)
    (let ((serialized (agent-shell-ask-serialize-store)))
      (should (= (length serialized) 2))
      (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
        (agent-shell-ask-deserialize-store serialized)
        (should (agent-shell-ask-get "q1"))
        (should (agent-shell-ask-get "q2"))
        (should (eq (agent-shell-ask-question-kind (agent-shell-ask-get "q2")) 'boolean))))))

(ert-deftest agent-shell-ask-test-shell-resurrection ()
  "Test agent-shell-queue--resurrect-shell returns live buffer or spawns new shell."
  (let ((live-buf (get-buffer-create "*test-resurrect-live*")))
    (with-current-buffer live-buf
      (setq default-directory "/tmp/"))
    (let ((res (agent-shell-queue--resurrect-shell "*test-resurrect-live*")))
      (should (equal res live-buf))
      (kill-buffer live-buf))))

(provide 'test-agent-shell-ask)

;;; test-agent-shell-ask.el ends here
