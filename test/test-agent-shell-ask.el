;;; test-agent-shell-ask.el --- ERT unit tests for agent-shell-ask -*- lexical-binding: t; -*-\n
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
      ;; Resetting cursor allows starting again from beginning
      (agent-shell-ask-cursor-reset "c1")
      (let ((c1-again (agent-shell-ask-cursor-next "c1")))
        (should (equal (agent-shell-ask-question-id c1-again) "q1")))
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

(ert-deftest agent-shell-ask-test-cancel ()
  "Test cancelling a question marks its status as cancelled."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q (agent-shell-ask-create :prompt "Cancel me" :id "q-cancel")))
      (should (eq (agent-shell-ask-question-status q) 'pending))
      (agent-shell-ask-cancel "q-cancel" "user requested abort")
      (should (eq (agent-shell-ask-question-status q) 'cancelled))
      (should (equal (agent-shell-ask-question-response q) "Cancelled: user requested abort"))
      ;; Cancelled item is no longer in pending list
      (should-not (member q (agent-shell-ask-list-pending))))))

(ert-deftest agent-shell-ask-test-pending-and-list-filtering ()
  "Test listing questions and filtering pending questions by target shell."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q1 (agent-shell-ask-create :prompt "Q1" :id "q1" :target-shell "shell-a"))
          (q2 (agent-shell-ask-create :prompt "Q2" :id "q2" :target-shell "shell-b"))
          (q3 (agent-shell-ask-create :prompt "Q3" :id "q3" :target-shell "shell-a")))
      (should (= (length (agent-shell-ask-list-all)) 3))
      (should (= (length (agent-shell-ask-list-pending)) 3))
      (should (= (length (agent-shell-ask-list-pending "shell-a")) 2))
      ;; Answering q1 removes it from pending
      (agent-shell-ask-answer "q1" "done")
      (should (= (length (agent-shell-ask-list-pending "shell-a")) 1))
      (should (= (length (agent-shell-ask-list-all)) 3)))))

(ert-deftest agent-shell-ask-test-followup-dispatch-and-enqueue ()
  "Test followup actions for :enqueue."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal))
        enqueued-args)
    (cl-letf (((symbol-function 'agent-shell-queue-enqueue)
               (lambda (prompt &rest rest)
                 (setq enqueued-args (cons prompt rest)))))
      ;; :enqueue followup
      (let ((q (agent-shell-ask-create
                :prompt "Queue next?"
                :id "q-f2"
                :target-shell "shell-a"
                :followup-action '(:type :enqueue :prompt "Step %s" :bucket "b1"))))
        (agent-shell-ask-execute-followup q "two")
        (should (equal (car enqueued-args) "Step two"))
        (should (equal (plist-get (cdr enqueued-args) :bucket) "b1"))))))

(ert-deftest agent-shell-ask-test-prompt-interactive-boolean ()
  "Test agent-shell-ask-prompt for boolean questions."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q (agent-shell-ask-create :prompt "Confirm?" :kind 'boolean :id "q-bool")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
        (should (eq (agent-shell-ask-prompt-question q) t))
        (agent-shell-ask-prompt "q-bool")
        (should (eq (agent-shell-ask-question-response q) t))
        (should (eq (agent-shell-ask-question-status q) 'answered))))))

(ert-deftest agent-shell-ask-test-prompt-interactive-text ()
  "Test agent-shell-ask-prompt for text questions."
  (let ((agent-shell-ask-store (make-hash-table :test #'equal)))
    (let ((q (agent-shell-ask-create :prompt "Enter name:" :kind 'text :id "q-text")))
      (cl-letf (((symbol-function 'read-string) (lambda (_prompt &optional def) (or def "Alice"))))
        (should (equal (agent-shell-ask-prompt-question q) "Alice"))
        (agent-shell-ask-prompt "q-text")
        (should (equal (agent-shell-ask-question-response q) "Alice"))
        (should (eq (agent-shell-ask-question-status q) 'answered))))))

(ert-deftest agent-shell-ask-test-plist-roundtrip ()
  "Test question-to-plist and question-from-plist round-trip."
  (let* ((q (agent-shell-ask-create
             :prompt "What is your quest?"
             :kind 'text
             :id "q-grail"
             :options '("opt1" "opt2")
             :default-value "Grail"
             :target-shell "shell-roundtrip"
             :directory "/tmp/"
             :timeout 30.0
             :followup-action '(:type :enqueue :prompt "ok")))
         (plist (agent-shell-ask-question-to-plist q))
         (restored (agent-shell-ask-question-from-plist plist)))
    (should (equal (agent-shell-ask-question-id restored) "q-grail"))
    (should (equal (agent-shell-ask-question-prompt restored) "What is your quest?"))
    (should (eq (agent-shell-ask-question-kind restored) 'text))
    (should (equal (agent-shell-ask-question-default-value restored) "Grail"))
    (should (equal (agent-shell-ask-question-target-shell restored) "shell-roundtrip"))
    (should (equal (agent-shell-ask-question-directory restored) "/tmp/"))
    (should (equal (agent-shell-ask-question-timeout restored) 30.0))))

(provide 'test-agent-shell-ask)

;;; test-agent-shell-ask.el ends here
