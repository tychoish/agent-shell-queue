;;; test-agent-shell-prompt.el --- Tests for agent-shell-prompt -*- lexical-binding: t; no-byte-compile: t; -*-

;; Tests for the prompt registry, template rendering, pre/post
;; execution engine, dispatch routing, queue integration, and the ACR
;; picker added by agent-shell-prompt / agent-shell-prompt-menu /
;; agent-shell-prompt-library.
;;
;; Batch run:
;;   ./scripts/run-tests.el

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'agent-shell-queue)
(require 'agent-shell-menu)
(require 'agent-shell-prompt)
(require 'agent-shell-prompt-menu)
(require 'agent-shell-prompt-library)
(require 'test-helper)

;;; Isolation macro (mirrors asq-types-test/isolate)

(defmacro asp-test/isolate (&rest body)
  "Execute BODY with a clean prompt registry and isolated queue state."
  `(let ((agent-shell-prompt-registry (make-hash-table :test #'eq))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--queue
          (agent-shell-queue-queue--make
           :store 'agent-shell-queue--store
           :session-paused nil
           :editing-ids nil
           :interjection-pending nil))
         (agent-shell-queue--loaded t))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
       ,@body)))

;;; ─────────────────────────────────────────────────────────────
;;; Registry

(ert-deftest agent-shell-prompt/def-registers-spec ()
  "agent-shell-prompt-def registers a retrievable spec."
  (asp-test/isolate
   (register-agent-shell-prompt sample
     :doc "A sample prompt"
     :category "Testing"
     :args ((thing :prompt "Thing: "))
     :template "Do {{args.thing}}"
     :target :session-reuse)
   (let ((spec (agent-shell-prompt-get 'sample)))
     (should spec)
     (should (equal (agent-shell-prompt-spec-doc spec) "A sample prompt"))
     (should (equal (agent-shell-prompt-spec-category spec) "Testing"))
     (should (eq (agent-shell-prompt-spec-target spec) :session-reuse)))))

(ert-deftest agent-shell-prompt/def-alias-works ()
  "agent-shell-prompt-def alias works for backward compatibility."
  (asp-test/isolate
   (agent-shell-prompt-def sample-alias
     :template "test alias")
   (let ((spec (agent-shell-prompt-get 'sample-alias)))
     (should spec)
     (should (equal (agent-shell-prompt-spec-template spec) "test alias")))))

(ert-deftest agent-shell-prompt/def-defaults-category-and-target ()
  "Category defaults to General and target defaults to :ask when omitted."
  (asp-test/isolate
   (register-agent-shell-prompt bare :template "hi")
   (let ((spec (agent-shell-prompt-get 'bare)))
     (should (equal (agent-shell-prompt-spec-category spec) "General"))
     (should (eq (agent-shell-prompt-spec-target spec) :ask)))))

(ert-deftest agent-shell-prompt/def-requires-template ()
  "Registering without :template signals an error."
  (asp-test/isolate
   (should-error (agent-shell-prompt-register :id 'no-template))))

(ert-deftest agent-shell-prompt/redefine-replaces-entry ()
  "Re-registering an existing id replaces rather than duplicates it."
  (asp-test/isolate
   (register-agent-shell-prompt dup :template "one")
   (register-agent-shell-prompt dup :template "two")
   (should (= 1 (hash-table-count agent-shell-prompt-registry)))
   (should (equal (agent-shell-prompt-spec-template (agent-shell-prompt-get 'dup)) "two")))

(ert-deftest agent-shell-prompt/list-returns-all-specs ()
  "agent-shell-prompt-list returns every registered spec."
  (asp-test/isolate
   (register-agent-shell-prompt a :template "a")
   (register-agent-shell-prompt b :template "b")
   (should (= 2 (length (agent-shell-prompt-list))))))

;;; ─────────────────────────────────────────────────────────────
;;; Template rendering

(ert-deftest agent-shell-prompt/render-substitutes-args ()
  "{{args.KEY}} resolves against the :args sub-plist."
  (should (equal (agent-shell-prompt-render "Hello {{args.name}}" (list :args (list :name "world")))
                 "Hello world")))

(ert-deftest agent-shell-prompt/render-substitutes-top-level ()
  "{{key}} without an args. prefix resolves against the top-level ctx."
  (should (equal (agent-shell-prompt-render "Log:\n{{ci-log}}" (list :ci-log "boom"))
                 "Log:\nboom")))

(ert-deftest agent-shell-prompt/render-missing-key-is-empty ()
  "An unresolved placeholder renders as an empty string, not an error."
  (should (equal (agent-shell-prompt-render "[{{missing}}]" nil) "[]")))

(ert-deftest agent-shell-prompt/render-multiple-placeholders ()
  "Multiple placeholders in one template all resolve correctly.
Regression test: `split-string' inside the replacer must not clobber the
match-data `replace-regexp-in-string' relies on for subsequent matches."
  (should (equal (agent-shell-prompt-render
                  "{{args.repo}}#{{args.run-id}} {{ci-summary}}"
                  (list :args (list :repo "acme/x" :run-id 42) :ci-summary "OK"))
                 "acme/x#42 OK")))

(ert-deftest agent-shell-prompt/render-numeric-arg-stringified ()
  "A non-string value (e.g. an integer arg) is stringified for substitution."
  (should (equal (agent-shell-prompt-render "run {{args.run-id}}" (list :args (list :run-id 7)))
                 "run 7")))

;;; ─────────────────────────────────────────────────────────────
;;; Argument collection

(ert-deftest agent-shell-prompt/collect-args-skips-provided ()
  "Arguments already present in the provided plist are not re-read."
  (asp-test/isolate
   (register-agent-shell-prompt needs-arg
     :args ((thing :prompt "Thing: "))
     :template "{{args.thing}}")
   (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "should not prompt"))))
     (should (equal (agent-shell-prompt--collect-args
                     (agent-shell-prompt-get 'needs-arg) (list :thing "given"))
                    (list :thing "given"))))))

(ert-deftest agent-shell-prompt/collect-args-reads-missing ()
  "A missing argument is read interactively via its declared prompt."
  (asp-test/isolate
   (register-agent-shell-prompt needs-arg
     :args ((thing :prompt "Thing: "))
     :template "{{args.thing}}")
   (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "typed")))\n     (should (equal (agent-shell-prompt--collect-args (agent-shell-prompt-get 'needs-arg) nil)
                    (list :thing "typed"))))))

(ert-deftest agent-shell-prompt/collect-args-normalizes-bare-symbol-names ()
  "Bare-symbol arg-spec names normalize to keyword plist keys."
  (asp-test/isolate
   (should (eq (agent-shell-prompt--arg-key 'thing) :thing))
   (should (eq (agent-shell-prompt--arg-key :thing) :thing))))

(ert-deftest agent-shell-prompt/read-arg-integer-type-uses-read-number ()
  "An arg-spec of :type integer reads its value via `read-number'."
  (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 7)))
    (should (equal (agent-shell-prompt--read-arg '(run-id :type integer)) '(:run-id . 7)))))

(ert-deftest agent-shell-prompt/read-arg-symbol-type-uses-completing-read ()
  "An arg-spec of :type symbol reads its value via `completing-read', interned."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "chosen")))
    (should (equal (agent-shell-prompt--read-arg '(mode :type symbol)) '(:mode . chosen)))))

(ert-deftest agent-shell-prompt/read-arg-required-empty-signals-error ()
  "A required (non-optional) arg-spec errors when the typed value is empty."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should-error (agent-shell-prompt--read-arg '(thing :prompt "Thing: ")) :type 'user-error)))

(ert-deftest agent-shell-prompt/read-arg-optional-empty-is-allowed ()
  "An :optional arg-spec permits an empty typed value without erroring."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should (equal (agent-shell-prompt--read-arg '(thing :optional t)) '(:thing . "")))))

;;; ─────────────────────────────────────────────────────────────
;;; Pre/post execution engine

(ert-deftest agent-shell-prompt/exec-pre-noop-without-pre-op ()
  "With no pre-op, exec-pre calls back with ctx unchanged."
  (asp-test/isolate
   (register-agent-shell-prompt no-pre :template "x")
   (let (result)
     (agent-shell-prompt-exec-pre (agent-shell-prompt-get 'no-pre) '(:a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal result '(:a 1))))))

(ert-deftest agent-shell-prompt/exec-pre-synchronous ()
  "A one-argument pre-op runs synchronously and its return value is passed on."
  (asp-test/isolate
   (register-agent-shell-prompt sync-pre
     :pre-op (lambda (ctx) (plist-put ctx :extra "added"))
     :template "x")
   (let (result)
     (agent-shell-prompt-exec-pre (agent-shell-prompt-get 'sync-pre) (list :a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal (plist-get result :extra) "added")))))

(ert-deftest agent-shell-prompt/exec-pre-asynchronous ()
  "A two-argument pre-op is treated as async and must invoke its own callback."
  (asp-test/isolate
   (register-agent-shell-prompt async-pre
     :pre-op (lambda (ctx callback) (funcall callback (plist-put ctx :extra "async")))
     :template "x")
   (let (result)
     (agent-shell-prompt-exec-pre (agent-shell-prompt-get 'async-pre) (list :a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal (plist-get result :extra) "async")))))

(ert-deftest agent-shell-prompt/exec-post-defaults-to-done ()
  "With no post-op, exec-post returns :done."
  (asp-test/isolate
   (register-agent-shell-prompt no-post :template "x")
   (should (eq (agent-shell-prompt-exec-post (agent-shell-prompt-get 'no-post) nil nil "resp")
              :done))))

(ert-deftest agent-shell-prompt/exec-post-invokes-post-op ()
  "The post-op is called with shell-buffer, ctx, and response text."
  (asp-test/isolate
   (let (seen)
     (register-agent-shell-prompt with-post
       :post-op (lambda (buf ctx resp) (setq seen (list buf ctx resp)) :close)
       :template "x")
     (should (eq (agent-shell-prompt-exec-post (agent-shell-prompt-get 'with-post) 'buf '(:a 1) "resp")
                :close))
     (should (equal seen (list 'buf '(:a 1) "resp"))))))

(ert-deftest agent-shell-prompt/apply-post-result-drop-context ()
  ":drop-context enqueues a clear command for the shell buffer."
  (asp-test/isolate
   (let ((called nil))
     (cl-letf (((symbol-function 'agent-shell-queue-enqueue-clear)
                (lambda (buf) (setq called buf))))
       (agent-shell-prompt--apply-post-result :drop-context 'my-buf)
       (should (eq called 'my-buf))))))

(ert-deftest agent-shell-prompt/apply-post-result-close-kills-buffer ()
  ":close kills a live shell buffer."
  (asp-test/isolate
   (let ((buf (generate-new-buffer "asp-test-close")))
     (unwind-protect
         (progn
           (agent-shell-prompt--apply-post-result :close buf)
           (should-not (buffer-live-p buf)))
       (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-shell-prompt/apply-post-result-chain-dispatches-next ()
  "(:chain ID ARGS) dispatches the next prompt with session-reuse and submit."
  (asp-test/isolate
   (let (seen)
     (cl-letf (((symbol-function 'agent-shell-prompt-dispatch)
                (lambda (id &rest keys) (setq seen (cons id keys)))))
       (agent-shell-prompt--apply-post-result '(:chain next-id (:x 1)) 'buf)
       (should (eq (car seen) 'next-id))
       (should (equal (plist-get (cdr seen) :target) :session-reuse))
       (should (plist-get (cdr seen) :submit))))))

(ert-deftest agent-shell-prompt/apply-post-result-done-is-noop ()
  ":done (and any unrecognized value) does nothing observable."
  (asp-test/isolate
   (should (null (agent-shell-prompt--apply-post-result :done 'buf)))
   (should (null (agent-shell-prompt--apply-post-result :unknown-flag 'buf))))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch routing

(ert-deftest agent-shell-prompt/resolve-target-passes-through-concrete-target ()
  "A concrete target resolves to itself without prompting."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "should not prompt"))))
    (should (eq (agent-shell-prompt--resolve-target :session-new) :session-new))))

(ert-deftest agent-shell-prompt/resolve-target-ask-prompts-via-completing-read ()
  ":ask resolves interactively via `completing-read', interned to a keyword."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ":queue")))
    (should (eq (agent-shell-prompt--resolve-target :ask) :queue))))

(ert-deftest agent-shell-prompt/dispatch-unknown-id-errors ()
  "Dispatching an unregistered id signals an error."
  (asp-test/isolate
   (should-error (agent-shell-prompt-dispatch 'does-not-exist))))

(ert-deftest agent-shell-prompt/dispatch-queue-target-enqueues ()
  "A :queue target enqueues a prompt-library item instead of inserting."
  (asp-test/isolate
   (register-agent-shell-prompt queued
     :args ((thing :prompt "Thing: "))
     :template "do {{args.thing}}"
     :target :queue)
   (let (enqueued)
     (cl-letf (((symbol-function 'agent-shell-queue--enqueue-args)
                (lambda (args kind buf) (setq enqueued (list args kind buf)))))
       (agent-shell-prompt-dispatch 'queued :args (list :thing "x"))
       (should (eq (nth 1 enqueued) 'prompt-library))
       (should (null (nth 2 enqueued)))
       (should (equal (plist-get (read (nth 0 enqueued)) :rendered) "do x"))))))

(ert-deftest agent-shell-prompt/dispatch-session-inserts-rendered-text ()
  "A session target renders the template and inserts it via agent-shell-insert."
  (asp-test/isolate
   (register-agent-shell-prompt direct
     :args ((thing :prompt "Thing: "))
     :template "do {{args.thing}}"
     :target :session-new
     :submit t)
   (let (inserted)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert)
                (lambda (&rest keys) (setq inserted keys))))
       (agent-shell-prompt-dispatch 'direct :args (list :thing "x"))
       (should (equal (plist-get inserted :text) "do x"))
       (should (plist-get inserted :submit))
       (should (eq (plist-get inserted :shell-buffer) 'new-buf))))))

(ert-deftest agent-shell-prompt/dispatch-session-reuse-prefers-existing-buffer ()
  ":session-reuse picks an existing directory-scoped buffer over creating one."
  (asp-test/isolate
   (register-agent-shell-prompt reuse-target :template "hi" :target :session-reuse :submit t)
   (let* ((buf (generate-new-buffer "asp-test-reuse"))
          new-shell-called)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-prompt--project-buffers) (lambda (_dir) (list buf)))
                   ((symbol-function 'agent-shell-new-shell) (lambda (&rest _) (setq new-shell-called t)))
                   ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                   ((symbol-function 'agent-shell-insert) #'ignore))
           (agent-shell-prompt-dispatch 'reuse-target)
           (should-not new-shell-called))
       (kill-buffer buf)))))

(ert-deftest agent-shell-prompt/dispatch-with-post-op-subscribes-using-insertion-end ()
  "When a spec has a post-op, dispatch subscribes to turn-complete with a
:response-start position taken from agent-shell-insert's return value."
  (asp-test/isolate
   (register-agent-shell-prompt with-post
     :template "hi"
     :target :session-new
     :post-op (lambda (_buf _ctx _resp) :done))
   (let (subscribed-args)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert) (lambda (&rest _) (list (cons :end 42))))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest keys) (setq subscribed-args keys) 'token)))
       (agent-shell-prompt-dispatch 'with-post)
       (should (eq (plist-get subscribed-args :shell-buffer) 'new-buf))
       (should (eq (plist-get subscribed-args :event) 'turn-complete))))))

(ert-deftest agent-shell-prompt/dispatch-without-post-op-does-not-subscribe ()
  "When a spec has no post-op, dispatch never calls agent-shell-subscribe-to."
  (asp-test/isolate
   (register-agent-shell-prompt no-post-dispatch :template "hi" :target :session-new)
   (let (subscribe-called)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert) (lambda (&rest _) (list (cons :end 1))))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest _) (setq subscribe-called t))))
       (agent-shell-prompt-dispatch 'no-post-dispatch)
       (should-not subscribe-called)))))

(ert-deftest agent-shell-prompt/last-response-text-delegates-to-queue-walker ()
  "agent-shell-prompt--last-response-text reuses the queue's visible-text walk."
  (let ((buf (generate-new-buffer "asp-test-response")) seen)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue--collect-visible-response-text)
                   (lambda (sbuf start) (setq seen (list sbuf start)) "captured")))
          (should (equal (agent-shell-prompt--last-response-text buf 7) "captured"))
          (should (equal seen (list buf 7))))
      (kill-buffer buf))))

(ert-deftest agent-shell-prompt/last-response-text-nil-without-start-pos ()
  "With no :response-start (no post-op was registered), no text is captured."
  (let ((buf (generate-new-buffer "asp-test-response-nil")))
    (unwind-protect
        (should (null (agent-shell-prompt--last-response-text buf nil)))
      (kill-buffer buf))))

;;; ─────────────────────────────────────────────────────────────
;;; Queue integration

(ert-deftest agent-shell-prompt/queue-item-type-registered ()
  "The prompt-library item type is registered against agent-shell-queue."
  (should (agent-shell-queue--type-for-kind 'prompt-library)))

(ert-deftest agent-shell-prompt/dispatch-queue-item-inserts-rendered-text ()
  "Dispatching a queued prompt-library item inserts its stored rendered text."
  (asp-test/isolate
   (let* ((buf (generate-new-buffer "asp-test-queue-item"))
          (item (agent-shell-queue--make-item
                 (prin1-to-string (list :prompt-id 'x :rendered "queued text"))
                 nil 'prompt-library))
          inserted)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-insert)
                    (lambda (&rest keys) (setq inserted keys))))
           (agent-shell-prompt--dispatch-queue-item item (buffer-name buf))
           (should (equal (plist-get inserted :text) "queued text"))
           (should (plist-get inserted :submit)))
       (kill-buffer buf)))))

;;; ─────────────────────────────────────────────────────────────
;;; Built-in library prompts

(ert-deftest agent-shell-prompt/library-registers-built-ins ()
  "The example prompt library registers all four built-in workflows."
  (dolist (id '(create-commit fix-ci pr-review-patch expand-coverage refactor-module))
    (should (agent-shell-prompt-get id))))

(ert-deftest agent-shell-prompt/library-diff-summary-short-diff ()
  "Short diff (<= 5 lines) is returned in full."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest _) "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-a\n+b")))
    (let ((res (agent-shell-prompt-library--diff-summary '("HEAD") 5)))
      (should (string-match-p "\\+b" res))
      (should-not (string-match-p "Diff exceeds" res))))

(ert-deftest agent-shell-prompt/library-diff-summary-long-diff-uses-stat ()
  "Long diff (> 5 lines) uses diffstat and note."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest args)
               (if (member "--stat" args)
                   " foo.el | 10 +++++-----\n 1 file changed"
                 "line1\nline2\nline3\nline4\nline5\nline6\nline7"))))
    (let ((res (agent-shell-prompt-library--diff-summary '("HEAD") 5)))
      (should (string-match-p "foo\\.el | 10" res))
      (should (string-match-p "Diff exceeds 5 lines" res))))

(ert-deftest agent-shell-prompt/library-diff-summary-empty ()
  "Empty diff returns (no changes)."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest _) "")))
    (should (equal (agent-shell-prompt-library--diff-summary '("HEAD") 5) "(no changes)"))))

(ert-deftest agent-shell-prompt/library-fix-ci-pre-op-populates-ctx ()
  "fix-ci's pre-op fetches summary and log text via gh into ctx."
  (let ((tmp-dir (make-temp-file "asq-fix-ci-pre-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-prompt-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-prompt-library--shell)
                   (lambda (&rest args) (mapconcat #'identity args " "))))
          (let ((ctx (agent-shell-prompt-library--fix-ci-pre-op
                      (list :args (list :repo "acme/x" :run-id 9)))))
            (should (string-match-p "acme/x" (plist-get ctx :ci-summary)))
            (should (string-match-p "--log-failed" (plist-get ctx :ci-log))))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-prompt/library-fix-ci-saves-files-and-renders-template ()
  "fix-ci pre-op saves logs/jobs/index to filesystem and renders template with paths."
  (let ((tmp-dir (make-temp-file "asq-fix-ci-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function (quote agent-shell-prompt-library--project-root))
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function (quote agent-shell-prompt-library--shell))
                   (lambda (&rest args)
                     (cond
                      ((member "--log-failed" args)
                       "--- FAIL: TestWidget (0.05s)\n    widget_test.go:42: unexpected nil\nFAIL")
                      ((member "--json" args)
                       "{\"jobs\":[{\"name\":\"test\",\"conclusion\":\"failure\"}]}")
                      (t
                       "X Run #123 (Workflow: CI, Branch: main)")))))
          (let* ((spec (agent-shell-prompt-get (quote fix-ci)))
                 (ctx (agent-shell-prompt-library--fix-ci-pre-op
                       (list :args (list :repo "org/app" :run-id 123))))
                 (log-path (expand-file-name (plist-get ctx :ci-log-file) tmp-dir))
                 (jobs-path (expand-file-name (plist-get ctx :ci-jobs-file) tmp-dir))
                 (index-path (expand-file-name (plist-get ctx :ci-index-file) tmp-dir))
                 (rendered (agent-shell-prompt-render (agent-shell-prompt-spec-template spec) ctx)))
            ;; Files exist on disk
            (should (file-exists-p log-path))
            (should (file-exists-p jobs-path))
            (should (file-exists-p index-path))
            ;; Content was written
            (with-temp-buffer
              (insert-file-contents log-path)
              (should (string-match-p "TestWidget" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents index-path)
              (should (string-match-p "CI Triage Index" (buffer-string))))
            ;; Rendered template refers to files and gives triage guidance
            (should (string-match-p "\\.agent/fix-ci/run-123-logs\\.txt" rendered))
            (should (string-match-p "\\.agent/fix-ci/run-123-jobs\\.json" rendered))
            (should (string-match-p "\\.agent/fix-ci/ci-triage-index\\.md" rendered))
            (should (string-match-p "Do NOT read the entire log file into context" rendered))
            (should (string-match-p "150-200 lines" rendered))
            ;; Rendered template does NOT inline the entire log body or full page
            (should-not (string-match-p "widget_test\\.go:42" rendered))))
      (delete-directory tmp-dir t))))


(ert-deftest agent-shell-prompt/library-resolve-ci-run-auto-selects-failing ()
  "Auto-selects the latest run when its conclusion indicates failure."
  (let ((runs '(((databaseId . 34348785571) (displayTitle . "bump: dependencies") (status . "completed") (conclusion . "failure") (headBranch . "main") (headSha . "dd16f2f0107ebda9373d78cda7049397db797605"))
                ((databaseId . 34295328476) (displayTitle . "fix bug") (status . "completed") (conclusion . "success") (headBranch . "main") (headSha . "cdc9b486796226f379f82f32a66b385408cc19be")))))
    (cl-letf (((symbol-function 'agent-shell-prompt-library--fetch-runs) (lambda (&rest _) runs))
              ((symbol-function 'agent-shell-prompt-library--current-branch) (lambda () "main"))
              ((symbol-function 'annotated-completing-read) (lambda (&rest _) (error "should not prompt"))))
      (should (= 34348785571 (agent-shell-prompt-library--resolve-ci-run "acme/x" "main"))))))

(ert-deftest agent-shell-prompt/library-resolve-ci-run-uses-acr-picker-when-not-failing ()
  "Presents an ACR picker when the latest run is not failing."
  (let ((runs '(((databaseId . 111) (displayTitle . "feat: test") (status . "completed") (conclusion . "success") (headBranch . "main") (headSha . "abc1234567") (startedAt . "2026-09-09T10:00:00Z") (updatedAt . "2026-09-09T10:02:00Z") (createdAt . "2026-09-09T10:00:00Z"))
                ((databaseId . 222) (displayTitle . "fix: bug") (status . "completed") (conclusion . "failure") (headBranch . "main") (headSha . "def9876543") (startedAt . "2026-09-09T08:00:00Z") (updatedAt . "2026-09-09T08:01:30Z") (createdAt . "2026-09-09T08:00:00Z"))))
        acr-called)
    (cl-letf (((symbol-function 'agent-shell-prompt-library--fetch-runs) (lambda (&rest _) runs))
              ((symbol-function 'agent-shell-prompt-library--current-branch) (lambda () "main"))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _) (setq acr-called table) (caar table)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (setq acr-called table) (caar table))))
      (should (= 111 (agent-shell-prompt-library--resolve-ci-run "acme/x" "main")))
      (should acr-called)
      (should (string-match-p "success" (cdar acr-called))))))
(ert-deftest agent-shell-prompt/library-gather-runs-each-pair-into-ctx ()
  "agent-shell-prompt-library--gather stores each command's output under its key."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--shell)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-prompt-library--gather
                '(:a 1) (list (list :one "echo" "one") (list :two "echo" "two")))))
      (should (equal (plist-get ctx :one) "echo one"))
      (should (equal (plist-get ctx :two) "echo two"))
      (should (= 1 (plist-get ctx :a))))))

(ert-deftest agent-shell-prompt/library-pr-review-pre-op-populates-ctx ()
  "pr-review-patch's pre-op fetches PR comments via gh into ctx."
  (let ((tmp-dir (make-temp-file "asq-pr-pre-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-prompt-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-prompt-library--shell)
                   (lambda (&rest args) (mapconcat #'identity args " "))))
          (let ((ctx (agent-shell-prompt-library--pr-review-pre-op
                      (list :args (list :pr-number 42)))))
            (should (string-match-p "42" (plist-get ctx :pr-comments)))
            (should (string-match-p "--comments" (plist-get ctx :pr-comments))))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-prompt/library-pr-review-saves-files-and-renders-template ()
  "pr-review-patch pre-op saves markdown/JSON to filesystem and renders template with paths."
  (let ((tmp-dir (make-temp-file "asq-pr-review-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function (quote agent-shell-prompt-library--project-root))
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function (quote agent-shell-prompt-library--shell))
                   (lambda (&rest args)
                     (cond
                      ((member "--json" args)
                       "{\"title\":\"Support Foo\",\"author\":{\"login\":\"alice\"},\"url\":\"https://github.com/org/app/pull/55\",\"reviews\":[{\"author\":{\"login\":\"bob\"},\"state\":\"COMMENTED\",\"body\":\"Please simplify this function.\"}],\"comments\":[]}")
                      ((member "repos/org/app/pulls/55/comments" args)
                       "[{\"user\":{\"login\":\"carol\"},\"path\":\"widget.go\",\"line\":25,\"diff_hunk\":\"@@ -20,4 +20,5 @@\",\"body\":\"Handle error return here.\"}]")
                      ((member "--comments" args)
                       "Review by bob: Please simplify this function.")
                      (t ""))))))
          (let* ((spec (agent-shell-prompt-get (quote pr-review-patch)))
                 (ctx (agent-shell-prompt-library--pr-review-pre-op
                       (list :args (list :repo "org/app" :pr-number 55))))
                 (md-path (expand-file-name (plist-get ctx :pr-comments-file) tmp-dir))
                 (json-path (expand-file-name (plist-get ctx :pr-comments-json-file) tmp-dir))
                 (alias-md (expand-file-name ".agent/pr-comments/pr-comments.md" tmp-dir))
                 (rendered (agent-shell-prompt-render (agent-shell-prompt-spec-template spec) ctx)))
            ;; Files exist on disk
            (should (file-exists-p md-path))
            (should (file-exists-p json-path))
            (should (file-exists-p alias-md))
            ;; Markdown formatted properly
            (with-temp-buffer
              (insert-file-contents md-path)
              (let ((text (buffer-string)))
                (should (string-match-p "PR #55 Comments: Support Foo" text))
                (should (string-match-p "Review by @bob" text))
                (should (string-match-p "Inline Comment by @carol on `widget\\.go`" text))))
            ;; Rendered template directs agent to read files
            (should (string-match-p "\\.agent/pr-comments/pr-55-comments\\.md" rendered))
            (should (string-match-p "\\.agent/pr-comments/pr-55-comments\\.json" rendered))
            (should (string-match-p "Do NOT read all raw comment data into context" rendered))
            (should (string-match-p "change-required" rendered))
            (should (string-match-p "review-plan\\.md" rendered))
            ;; Rendered template does NOT inline the entire comments dump
            (should-not (string-match-p "Please simplify this function" rendered))))
      (delete-directory tmp-dir t))))


(ert-deftest agent-shell-prompt/library-coverage-pre-op-populates-ctx ()
  "expand-coverage's pre-op diffs :file against HEAD into ctx."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-prompt-library--coverage-pre-op
                (list :args (list :file "foo.el")))))
      (should (string-match-p "HEAD -- foo.el" (plist-get ctx :file-diff))))))

(ert-deftest agent-shell-prompt/library-refactor-pre-op-populates-ctx ()
  "refactor-module's pre-op gathers recent git log for :file into ctx."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-prompt-library--refactor-pre-op
                (list :args (list :file "foo.el")))))
      (should (string-match-p "log --oneline -n 5 -- foo.el" (plist-get ctx :recent-history))))))

(ert-deftest agent-shell-prompt/library-git-output-delegates-to-magit ()
  "agent-shell-prompt-library--git-output uses magit-git-output when available."
  (let (called-with)
    (cl-letf (((symbol-function 'magit-git-output)
               (lambda (&rest args) (setq called-with args) " magit-result \n")))
      (should (equal (agent-shell-prompt-library--git-output "status" "--short") "magit-result"))
      (should (equal called-with '("status" "--short"))))))

(ert-deftest agent-shell-prompt/library-git-output-falls-back-to-shell ()
  "agent-shell-prompt-library--git-output falls back to shell when magit is absent."
  (let (called-with)
    (cl-letf (((symbol-function 'magit-git-output) nil)
              ((symbol-function 'agent-shell-prompt-library--shell)
               (lambda (&rest args) (setq called-with args) "shell-result")))
      (should (equal (agent-shell-prompt-library--git-output "status" "--short") "shell-result"))
      (should (equal called-with '("git" "status" "--short"))))))

(ert-deftest agent-shell-prompt/library-create-commit-pre-op-without-files ()
  "create-commit pre-op gathers status, diff HEAD, and recent log without files arg."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-prompt-library--create-commit-pre-op nil)))
      (should (string-match-p "status --short" (plist-get ctx :git-status)))
      (should (string-match-p "HEAD" (plist-get ctx :git-diff)))
      (should (string-match-p "log --oneline -n 5" (plist-get ctx :recent-log))))))

(ert-deftest agent-shell-prompt/library-create-commit-pre-op-with-files ()
  "create-commit pre-op scopes status and diff to specified files."
  (cl-letf (((symbol-function 'agent-shell-prompt-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-prompt-library--create-commit-pre-op
                (list :args (list :files "src/lib.el")))))
      (should (string-match-p "status --short -- src/lib.el" (plist-get ctx :git-status)))
      (should (string-match-p "HEAD -- src/lib.el" (plist-get ctx :git-diff)))
      (should (string-match-p "log --oneline -n 5" (plist-get ctx :recent-log))))))

;;; ─────────────────────────────────────────────────────────────
;;; ACR menu

(ert-deftest agent-shell-prompt/select-dispatches-chosen-prompt ()
  "agent-shell-prompt-select dispatches whichever spec the picker returns."
  (asp-test/isolate
   (register-agent-shell-prompt choosable :template "hi")
   (let (dispatched)
     (cl-letf (((symbol-function 'annotated-completing-read)
                (lambda (table &rest _) (cddr (assoc "choosable" table))))
               ((symbol-function 'agent-shell-prompt-dispatch)
                (lambda (id &rest _) (setq dispatched id))))
       (agent-shell-prompt-select)
       (should (eq dispatched 'choosable))))))

(ert-deftest agent-shell-prompt/select-errors-when-registry-empty ()
  "Selecting from an empty registry signals a user-error."
  (asp-test/isolate
   (should-error (agent-shell-prompt-select) :type 'user-error)))

(ert-deftest agent-shell-prompt/select-filters-by-category ()
  "Passing a category to agent-shell-prompt-select narrows the candidates."
  (asp-test/isolate
   (register-agent-shell-prompt in-cat :category "CI/CD" :template "hi")
   (register-agent-shell-prompt other-cat :category "Testing" :template "hi")
   (should (= 1 (length (agent-shell-prompt--candidates "CI/CD"))))))

(ert-deftest agent-shell-prompt/dispatch-menu-key-integrity ()
  "No key in agent-shell-prompt-dispatch-menu conflicts or duplicates."
  (let ((keys (transient-test/collect-keys 'agent-shell-prompt-dispatch-menu)))
    (should (null (transient-test/key-prefix-conflicts keys)))
    (should (null (transient-test/duplicate-keys keys)))))

(ert-deftest agent-shell-prompt/dispatch-menu-suffix-registered ()
  "agent-shell-prompt-dispatch-menu is a transient prefix wrapping the ACR picker."
  (should (get 'agent-shell-prompt-dispatch-menu 'transient--layout))
  (should (commandp 'agent-shell-prompt-select)))

(provide 'test-agent-shell-prompt)

;;; test-agent-shell-prompt.el ends here
