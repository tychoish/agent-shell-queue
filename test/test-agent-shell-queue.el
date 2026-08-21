;;; test-agent-shell-queue.el --- ERT tests for agent-shell-queue -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-queue/")
;;
;; Batch run (requires agent-shell on the load path):
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-agent-shell-queue.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-queue/")'

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-queue)

;;; Test helpers

(defmacro agent-shell-queue-test/isolate (&rest body)
  "Execute BODY with fresh, isolated queue state.
Shadows both the live store and queue globals so no test touches real state."
  `(let* ((agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file nil))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store
            :session-paused nil
            :editing-ids nil
            :interjection-pending nil))
          (agent-shell-queue--loaded t)
          (agent-shell-queue--subscriptions nil)
          (agent-shell-queue--stale-item-ids nil)
          (agent-shell-queue--next-flush-time nil)
          (agent-shell-queue--wait-timers nil)
          (agent-shell-queue--compact-running nil)
          (agent-shell-queue--response-start-positions nil)
          (agent-shell-queue--last-flush-time nil))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'alert) #'ignore))
       ,@body)))

(defmacro agent-shell-queue-test/isolate-no-sub (&rest body)
  "Like `agent-shell-queue-test/isolate' but also stubs subscription management."
  `(agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--drop-subscription) #'ignore))
      ,@body)))

(defun agent-shell-queue-test/make-item (id prompt &optional status background)
  "Build a test item directly, bypassing ID generation."
  (agent-shell-queue-item--make
   :id id
   :args prompt
   :status (or status 'active)
   :kind 'prompt
   :background background
   :created 1000.0
   :dispatched nil
   :completed nil
   :response nil))

(defun agent-shell-queue-test/populate (&rest specs)
  "Return a fresh items alist from SPECS.
Each spec is (BUF-NAME (ID PROMPT STATUS BACKGROUND) ...)."
  (seq-map (lambda (spec)
             (cons (car spec)
                   (seq-map (lambda (i)
                              (agent-shell-queue-test/make-item
                               (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i)))
                            (cdr spec))))
           specs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--capture-response

(defun agent-shell-queue-test/make-response-buffer (&rest spans)
  "Build a temp buffer simulating an agent-shell response buffer.
The buffer contains a `field=input' prompt echo followed by SPANS.
Each span is (TEXT &optional STATE INVISIBLE) where STATE is placed on the
span as `agent-shell-ui-state' and INVISIBLE, when non-nil, sets the
`invisible' text property to t — simulating a truly-collapsed labeled block.
A `field=boundary' marker follows all spans.

Returns (BUFFER . START-POS) where START-POS is the position of the prompt
echo — the correct value for `agent-shell-queue--response-start-positions'.
Caller is responsible for killing the buffer."
  (let ((buf (generate-new-buffer "*test-response-buf*")))
    (with-current-buffer buf
      ;; Echoed prompt with field=input.  capture-response uses this to find
      ;; where the model response begins (first field change past start-pos).
      (let* ((beg (point))
             (start-pos beg))
        (insert "echoed-prompt")
        (put-text-property beg (point) 'field 'input)
        ;; Response spans follow the prompt.
        (dolist (span spans)
          (let* ((text (car span))
                 (state (cadr span))
                 (hidden (nth 2 span))
                 (sbeg (point)))
            (insert text)
            (when state
              (put-text-property sbeg (point) 'agent-shell-ui-state state))
            (when hidden
              (put-text-property sbeg (point) 'invisible t))))
        ;; End-of-output boundary required by capture logic.
        (let ((bbeg (point)))
          (insert "\n")
          (put-text-property bbeg (point) 'field 'boundary))
        (cons buf start-pos)))))

(ert-deftest agent-shell-queue/capture-response-collects-plain-text-with-ui-state ()
  "Regression: plain text stored via agent-shell-ui-update-text carries
agent-shell-ui-state without :collapsed.  capture-response must include it."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (plain-state '((:qualified-id . "msg-1")))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "Hello, world!" plain-state)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (should (equal "Hello, world!"
                        (agent-shell-queue-item-response item))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-skips-collapsible-blocks ()
  "Collapsible blocks (thinking, tool calls) have :collapsed in their state
AND the invisible property set.  capture-response must exclude them."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (block-state '((:qualified-id . "tool-1") (:collapsed . t)))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     ;; Third element t → sets invisible property (truly collapsed).
                     (list "[tool output]" block-state t)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (should-not (agent-shell-queue-item-response item)))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-cleans-prompts ()
  "Prompt artifacts and trailing prompts are removed from the response."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "<shell-maker-end-of-prompt>\nActual response.\nGemini>" nil nil)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (equal "Actual response." response))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-mixes-plain-and-block ()
  "All visible segments are captured; block titles (first line) are discarded; collapsed blocks are skipped."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (plain-state-1 '((:qualified-id . "msg-1")))
         (block-state   '((:qualified-id . "tool-1") (:collapsed . t)))
         (plain-state-2 '((:qualified-id . "msg-2")))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "First prose."  plain-state-1 nil)
                     ;; A visible block: first line should be discarded.
                     (list "Title\nBlock content." '((:qualified-id . "b1")) nil)
                     ;; An invisible block: should be skipped entirely.
                     (list "Title\n[hidden]"    block-state   t)
                     (list "Second prose." plain-state-2 nil)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (equal "First prose.\n\nBlock content.\n\nSecond prose." response))
           (should-not (string-match-p "Title" response))
           (should-not (string-match-p "hidden" response))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-collects-agent-message-chunks ()
  "Regression: agent message chunks have (:collapsed . t) in state but NO
invisible property — they are always visible and must be collected.
This was the root bug: assq :collapsed returned a truthy cons even for
visible agent-message spans, causing all response text to be dropped."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         ;; agent-shell-ui--insert-fragment always sets :collapsed in state
         ;; for agent_message chunks, but only sets invisible on labeled blocks.
         (msg-state '((:qualified-id . "msg-1") (:collapsed . t)))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     ;; No third element — invisible NOT set despite :collapsed in state.
                     (list "Response text from agent." msg-state)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (string-match-p "Response text from agent" response))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-collapsed-without-invisible-is-collected ()
  "A span with :collapsed t but no invisible property is always collected.
Only the invisible text property, not the :collapsed state key, determines
whether a block's content is hidden from the user."
  (let* ((item (agent-shell-queue-test/make-item "q02" "prompt" 'running))
         (collapsed-but-visible '((:qualified-id . "chunk-1") (:collapsed . t)))
         (truly-hidden           '((:qualified-id . "hidden-1") (:collapsed . t)))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "Visible text."   collapsed-but-visible nil)
                     (list "Hidden content." truly-hidden           t)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q02" start-pos)))
         (agent-shell-queue--capture-response "q02" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (string-match-p "Visible text" response))
           (should-not (string-match-p "Hidden content" response))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-no-start-position-returns-nil ()
  "capture-response is a no-op when no start position is recorded for the item."
  (let* ((item (agent-shell-queue-test/make-item "q99" "prompt" 'running))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list "some text" nil)))
         (shell-buf (car buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         ;; Deliberately omit recording a start position.
         (agent-shell-queue--capture-response "q99" (buffer-name shell-buf))
         (should-not (agent-shell-queue-item-response item)))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-truncates-long-text ()
  "Responses longer than 8192 characters are truncated with a suffix."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (long-text (make-string 9000 ?x))
         (buf+start (agent-shell-queue-test/make-response-buffer
                     (list long-text nil)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (agent-shell-queue--capture-response "q01" (buffer-name shell-buf))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (< (length response) 9000))
           (should (string-suffix-p "…[truncated]" response))))
      (kill-buffer shell-buf))))

;;; Full pipeline integration: mark-running-done → capture-response → item.response

(defun agent-shell-queue-test/make-shell-buffer-realistic (&rest spans)
  "Build a buffer matching the real shell-maker/agent-shell turn structure.
The structure is:
  field=input    — echoed user prompt
  field=boundary — shell-maker boundary (appears BEFORE the model response)
  field=output   — '<shell-maker-end-of-prompt>' boilerplate
  SPANS          — model response content (each with field=output)
Each span is (TEXT &optional STATE INVISIBLE).
Returns (BUFFER . START-POS) where START-POS is point-max right after the
boilerplate — matching what `agent-shell-queue--default-executor' records."
  (let ((buf (generate-new-buffer "*test-shell-buf-realistic*")))
    (with-current-buffer buf
      (let ((prompt-beg (point)))
        (insert "user prompt")
        (put-text-property prompt-beg (point) 'field 'input)
        (let ((bbeg (point)))
          (insert "\n")
          (put-text-property bbeg (point) 'field 'boundary))
        (let ((obeg (point)))
          (insert "<shell-maker-end-of-prompt>\n")
          (put-text-property obeg (point) 'field 'output))
        (let ((start-pos (point)))
          (seq-do (lambda (span)
                    (let* ((text (car span))
                           (state (cadr span))
                           (hidden (nth 2 span))
                           (sbeg (point)))
                      (insert text)
                      (put-text-property sbeg (point) 'field 'output)
                      (when state
                        (put-text-property sbeg (point) 'agent-shell-ui-state state))
                      (when hidden
                        (put-text-property sbeg (point) 'invisible t))))
                  spans)
          (cons buf start-pos))))))

(defun agent-shell-queue-test/make-shell-buffer (text)
  "Build a minimal mock agent-shell buffer containing TEXT as the model response.
Returns (BUFFER . START-POS).  The buffer has the shape that
`agent-shell-queue--capture-response' expects:
  - a field=input prompt echo at START-POS
  - TEXT as a plain (no agent-shell-ui-state) span
  - a field=boundary marker at the end
Caller is responsible for killing the buffer."
  (let ((buf (generate-new-buffer "*test-shell-buf*")))
    (with-current-buffer buf
      (let ((start-pos (point)))
        (insert ">>> user prompt")
        (put-text-property start-pos (point) 'field 'input)
        (insert text)
        (let ((bbeg (point)))
          (insert "\n")
          (put-text-property bbeg (point) 'field 'boundary))
        (cons buf start-pos)))))

(ert-deftest agent-shell-queue/mark-running-done-captures-response ()
  "Integration: `agent-shell-queue--mark-running-done' calls capture-response,
which populates item.response from the shell buffer content."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (shell-buf+start (agent-shell-queue-test/make-shell-buffer
                           " The answer is 42."))
         (shell-buf (car shell-buf+start))
         (start-pos (cdr shell-buf+start))
         (buf-name (buffer-name shell-buf)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list buf-name item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (cl-letf (((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                   ((symbol-function 'agent-shell-queue--maybe-dispatch-next) #'ignore)
                   ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore))
           (agent-shell-queue--mark-running-done buf-name))
         (should (eq 'done (agent-shell-queue-item-status item)))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (string-match-p "42" response))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/mark-running-done-captures-mixed-response ()
  "Integration: all visible segments are captured; only invisible blocks are discarded."
  (let* ((item (agent-shell-queue-test/make-item "q01" "prompt" 'running))
         (buf (generate-new-buffer "*test-shell-mixed*"))
         buf-name start-pos)
    (with-current-buffer buf
      (setq buf-name (buffer-name buf))
      (setq start-pos (point))
      (insert ">>> prompt")
      (put-text-property start-pos (point) 'field 'input)
      ;; Agent message text — visible but not the last segment.
      (let ((p (point)))
        (insert "The result is correct.")
        (put-text-property p (point) 'agent-shell-ui-state
                           '((:qualified-id . "msg-1") (:collapsed . t))))
      ;; Tool call block — collapsed and hidden.
      (let ((p (point)))
        (insert "[tool: bash]")
        (put-text-property p (point) 'agent-shell-ui-state
                           '((:qualified-id . "tool-1") (:collapsed . t)))
        (put-text-property p (point) 'invisible t))
      ;; Final text — the last visible segment.
      (insert " Done.")
      (let ((bbeg (point)))
        (insert "\n")
        (put-text-property bbeg (point) 'field 'boundary)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list buf-name item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q01" start-pos)))
         (cl-letf (((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                   ((symbol-function 'agent-shell-queue--maybe-dispatch-next) #'ignore)
                   ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore))
           (agent-shell-queue--mark-running-done buf-name))
         (let ((response (agent-shell-queue-item-response item)))
           (should (stringp response))
           (should (string-match-p "Done" response))
           (should (string-match-p "The result is correct" response))
           (should-not (string-match-p "tool: bash" response))))
      (kill-buffer buf))))

;;; Regression tests: capture-response with realistic shell-maker buffer structure
;;
;; In the real agent-shell/shell-maker buffer layout the field=boundary marker
;; appears BEFORE the model response (right after the user prompt), not after
;; it.  This means start-pos (recorded as point-max right after agent-shell-insert
;; returns) is already in the field=output region.  The old code called
;; next-single-property-change from start-pos to find where the field changes
;; away from field=input — but since we are already past field=input the call
;; returns the field change at the very end of the response (= end-pos), making
;; response-start = end-pos and collecting nothing.

(ert-deftest agent-shell-queue/capture-response-realistic-buffer-plain-text ()
  "Regression: capture works when start-pos is in field=output (real layout)."
  (let* ((item (agent-shell-queue-test/make-item "q-real" "prompt" 'running))
         (buf+start (agent-shell-queue-test/make-shell-buffer-realistic
                     (list "All 42 tests pass.")))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q-real" start-pos)))
         (agent-shell-queue--capture-response "q-real" (buffer-name shell-buf))
         (should (equal "All 42 tests pass."
                        (agent-shell-queue-item-response item))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-realistic-buffer-skips-collapsed ()
  "Regression: all visible segments are captured; only invisible (collapsed) blocks are discarded."
  (let* ((item (agent-shell-queue-test/make-item "q-real2" "prompt" 'running))
         (buf+start (agent-shell-queue-test/make-shell-buffer-realistic
                     (list "Visible text." nil nil)
                     (list "[tool call]" 'tool t)
                     (list " Done." nil nil)))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q-real2" start-pos)))
         (agent-shell-queue--capture-response "q-real2" (buffer-name shell-buf))
         (let ((resp (agent-shell-queue-item-response item)))
           (should (stringp resp))
           (should (string-match-p "Done" resp))
           (should (string-match-p "Visible text" resp))
           (should-not (string-match-p "tool call" resp))))
      (kill-buffer shell-buf))))

(ert-deftest agent-shell-queue/capture-response-realistic-buffer-no-response ()
  "Empty model output logs a message and leaves response nil."
  (let* ((item (agent-shell-queue-test/make-item "q-empty" "prompt" 'running))
         (buf+start (agent-shell-queue-test/make-shell-buffer-realistic))
         (shell-buf (car buf+start))
         (start-pos (cdr buf+start)))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (setf (agent-shell-queue-store-items agent-shell-queue--store)
               (list (list (buffer-name shell-buf) item)))
         (setq agent-shell-queue--response-start-positions
               (list (cons "q-empty" start-pos)))
         (agent-shell-queue--capture-response "q-empty" (buffer-name shell-buf))
         (should (null (agent-shell-queue-item-response item))))
      (kill-buffer shell-buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--clean-args (pure)

(ert-deftest agent-shell-queue/clean-args-strips-trailing-spaces ()
  "Trailing spaces on each line are removed."
  (should (equal "hello\nworld"
                 (agent-shell-queue--clean-args "hello  \nworld   "))))

(ert-deftest agent-shell-queue/clean-args-strips-trailing-tabs ()
  "Trailing tabs on each line are removed."
  (should (equal "a\nb"
                 (agent-shell-queue--clean-args "a\t\nb\t\t"))))

(ert-deftest agent-shell-queue/clean-args-preserves-internal-whitespace ()
  "Spaces within a line are untouched."
  (should (equal "hello world"
                 (agent-shell-queue--clean-args "hello world  "))))

(ert-deftest agent-shell-queue/clean-args-no-op-on-clean-string ()
  "Already-clean strings are returned unchanged."
  (should (equal "clean" (agent-shell-queue--clean-args "clean"))))

(ert-deftest agent-shell-queue/make-item-cleans-trailing-whitespace ()
  "Items created via --make-item have trailing whitespace stripped."
  (let ((item (agent-shell-queue--make-item "hello  \nworld   ")))
    (should (equal "hello\nworld" (agent-shell-queue-item-args item)))))

(ert-deftest agent-shell-queue/edit-cleans-trailing-whitespace ()
  "agent-shell-queue-edit strips trailing whitespace from the new args."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-e1" "old" active nil))))
    (agent-shell-queue-edit "q-e1" "new  \nvalue   ")
    (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (equal "new\nvalue" (agent-shell-queue-item-args item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Response persistence

(ert-deftest agent-shell-queue/response-persists-in-plist-roundtrip ()
  "Response text survives a to-plist / from-plist round-trip."
  (let* ((item (agent-shell-queue-test/make-item "q-resp" "prompt" 'done))
         (_ (setf (agent-shell-queue-item-response item) "the response text"))
         (restored (agent-shell-queue-item-from-plist
                    (agent-shell-queue-item-to-plist item))))
    (should (equal "the response text" (agent-shell-queue-item-response restored)))))

(ert-deftest agent-shell-queue/response-nil-persists-in-plist-roundtrip ()
  "Nil response survives a to-plist / from-plist round-trip as nil."
  (let* ((item (agent-shell-queue-test/make-item "q-nil-resp" "prompt" 'active))
         (restored (agent-shell-queue-item-from-plist
                    (agent-shell-queue-item-to-plist item))))
    (should (null (agent-shell-queue-item-response restored)))))

(ert-deftest agent-shell-queue/done-item-response-persists-across-save-load ()
  "Done items with responses are included in serialized queue state."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-done" "the prompt" done nil))))
    (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
      (setf (agent-shell-queue-item-response item) "saved response"))
    (let* ((str (agent-shell-queue-serialize agent-shell-queue--store))
           (loaded (agent-shell-queue-deserialize agent-shell-queue--store str))
           (restored-item (cadr (car loaded))))
      (should (equal "saved response" (agent-shell-queue-item-response restored-item)))
      (should (eq 'done (agent-shell-queue-item-status restored-item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--format-age (pure)

(ert-deftest agent-shell-queue/format-age-seconds ()
  (should (equal "30s" (agent-shell-queue--format-age 30.0))))

(ert-deftest agent-shell-queue/format-age-minutes ()
  (should (equal "5m" (agent-shell-queue--format-age (* 5 60.0)))))

(ert-deftest agent-shell-queue/format-age-hours ()
  (should (equal "2h" (agent-shell-queue--format-age (* 2 3600.0)))))

(ert-deftest agent-shell-queue/format-age-days ()
  (should (equal "3d" (agent-shell-queue--format-age (* 3 86400.0)))))

(ert-deftest agent-shell-queue/format-age-boundary-minute ()
  "59 seconds is still seconds; 60 seconds is 1m."
  (should (equal "59s" (agent-shell-queue--format-age 59.0)))
  (should (equal "1m"  (agent-shell-queue--format-age 60.0))))

(ert-deftest agent-shell-queue/format-age-boundary-hour ()
  (should (equal "59m" (agent-shell-queue--format-age (- 3600.0 60))))
  (should (equal "1h"  (agent-shell-queue--format-age 3600.0))))

;;; format-age with time-value inputs (seconds-to-time)

(ert-deftest agent-shell-queue/format-age-time-value-seconds ()
  (should (equal "1s"  (agent-shell-queue--format-age (seconds-to-time 1))))
  (should (equal "30s" (agent-shell-queue--format-age (seconds-to-time 30))))
  (should (equal "59s" (agent-shell-queue--format-age (seconds-to-time 59)))))

(ert-deftest agent-shell-queue/format-age-time-value-minutes ()
  (should (equal "1m"  (agent-shell-queue--format-age (seconds-to-time 60))))
  (should (equal "5m"  (agent-shell-queue--format-age (seconds-to-time 300))))
  (should (equal "59m" (agent-shell-queue--format-age (seconds-to-time 3599)))))

(ert-deftest agent-shell-queue/format-age-time-value-hours ()
  (should (equal "1h" (agent-shell-queue--format-age (seconds-to-time 3600))))
  (should (equal "2h" (agent-shell-queue--format-age (seconds-to-time 7200)))))

(ert-deftest agent-shell-queue/format-age-time-value-days ()
  (should (equal "1d" (agent-shell-queue--format-age (seconds-to-time 86400))))
  (should (equal "3d" (agent-shell-queue--format-age (seconds-to-time (* 3 86400))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--status-string (pure)

(ert-deftest agent-shell-queue/status-string-active ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (should (equal "scheduled" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred ()
  "Legacy deferred items display as blocked.skip (migration compat)."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred nil)))
      (should (equal "blocked.skip" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-blocked-skip ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'blocked.skip nil)))
      (should (equal "blocked.skip" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-active-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active t)))
      (should (equal "scheduled.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-deferred-background ()
  "Legacy deferred background items display as blocked.skip.bg."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'deferred t)))
      (should (equal "blocked.skip.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running nil)))
      (should (equal "running.active" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-running-background ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'running t)))
      (should (equal "running.active.bg" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-done ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'done nil)))
      (should (equal "done" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-editing ()
  "Items with ID in editing-ids show 'editing' regardless of their status."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
      (should (equal "editing" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/status-string-session-paused ()
  "Active item targeting a session-paused buffer shows 'blocked.runner'."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "blocked.runner"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-session-paused-not-deferred ()
  "Deferred/blocked items are not shown as blocked.runner even if session is paused."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'blocked.skip nil)))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "blocked.skip"
                     (agent-shell-queue--status-string item "paused-buf"))))))

(ert-deftest agent-shell-queue/status-string-editing-takes-priority-over-paused ()
  "Editing status takes priority over session-paused."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("paused-buf"))
      (should (equal "editing"
                     (agent-shell-queue--status-string item "paused-buf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--activity-state

(ert-deftest agent-shell-queue/activity-state-running ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" running nil))))
    (should (equal "running"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-waiting ()
  "Active items with none running → waiting."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
    (should (equal "waiting"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-idle ()
  "Empty queue → idle."
  (agent-shell-queue-test/isolate
    (should (equal "idle"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

(ert-deftest agent-shell-queue/activity-state-idle-only-done ()
  "Only done items → idle."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" done nil))))
    (should (equal "idle"
                   (substring-no-properties (agent-shell-queue--activity-state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-instance-name

(ert-deftest agent-shell-queue/instance-name-string ()
  "A string value is returned directly."
  (let ((agent-shell-queue-instance-name "my-instance"))
    (should (equal "my-instance"
                   (let ((n agent-shell-queue-instance-name))
                     (if (functionp n) (funcall n) n))))))

(ert-deftest agent-shell-queue/instance-name-function ()
  "A function value is called to produce the name."
  (let ((agent-shell-queue-instance-name (lambda () "from-fn")))
    (should (equal "from-fn"
                   (let ((n agent-shell-queue-instance-name))
                     (if (functionp n) (funcall n) n))))))

(ert-deftest agent-shell-queue/instance-name-symbol-function ()
  "A function symbol is also callable."
  (let ((agent-shell-queue-instance-name #'agent-shell-queue--default-instance-name))
    (should (stringp (let ((n agent-shell-queue-instance-name))
                       (if (functionp n) (funcall n) n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--make-item

(ert-deftest agent-shell-queue/make-item-has-string-id ()
  "Each --make-item call returns an item with a unique string ID starting with q."
  (agent-shell-queue-test/isolate
    (let ((id1 (agent-shell-queue-item-id (agent-shell-queue--make-item "hello")))
          (id2 (agent-shell-queue-item-id (agent-shell-queue--make-item "world"))))
      (should (stringp id1))
      (should (string-prefix-p "q" id1))
      (should (stringp id2))
      (should (not (equal id1 id2))))))

(ert-deftest agent-shell-queue/make-item-defaults ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue--make-item "hello")))
      (should (equal "hello" (agent-shell-queue-item-args item)))
      (should (eq 'active (agent-shell-queue-item-status item)))
      (should (null (agent-shell-queue-item-background item)))
      (should (null (agent-shell-queue-item-dispatched item)))
      (should (null (agent-shell-queue-item-completed item)))
      (should (numberp (agent-shell-queue-item-created item))))))

(ert-deftest agent-shell-queue/make-item-background-flag ()
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue--make-item "hello" t)))
      (should (agent-shell-queue-item-background item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--item-by-id

(ert-deftest agent-shell-queue/item-by-id-found ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "first" active nil) ("q-2" "second" active nil))
           '("buf2" ("q-3" "third" active nil))))
    (let ((result (agent-shell-queue--item-by-id "q-2")))
      (should result)
      (should (equal "buf1" (car result)))
      (should (equal "q-2" (agent-shell-queue-item-id (cdr result))))
      (should (equal "second" (agent-shell-queue-item-args (cdr result)))))))

(ert-deftest agent-shell-queue/item-by-id-not-found ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "first" active nil))))
    (should-not (agent-shell-queue--item-by-id "q-99"))))

(ert-deftest agent-shell-queue/item-by-id-empty-queue ()
  (agent-shell-queue-test/isolate
    (should-not (agent-shell-queue--item-by-id "q-1"))))

(ert-deftest agent-shell-queue/item-by-id-across-buckets ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil))
           '("buf2" ("q-2" "b" active nil))
           '("buf3" ("q-3" "c" active nil))))
    (should (equal "buf3" (car (agent-shell-queue--item-by-id "q-3"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-add

(ert-deftest agent-shell-queue/add-creates-bucket ()
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (item (agent-shell-queue-add "hello" buf)))
      (unwind-protect
          (progn
            (should (agent-shell-queue-item-p item))
            (should (equal "hello" (agent-shell-queue-item-args item)))
            (should (eq 'active (agent-shell-queue-item-status item)))
            (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
            (should (equal (buffer-name buf) (caar (agent-shell-queue-store-items agent-shell-queue--store))))
            (should (= 1 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-appends-to-existing-bucket ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-test-buf*")))
      (unwind-protect
          (progn
            (agent-shell-queue-add "first" buf)
            (agent-shell-queue-add "second" buf)
            (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
            (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
              (should (= 2 (length items)))
              (should (equal "first"  (agent-shell-queue-item-args (nth 0 items))))
              (should (equal "second" (agent-shell-queue-item-args (nth 1 items))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/add-background-flag-propagated ()
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (item (agent-shell-queue-add "hello" buf t)))
      (unwind-protect
          (should (agent-shell-queue-item-background item))
        (kill-buffer buf)))))
(ert-deftest agent-shell-queue/add-delays-propagated ()
  "agent-shell-queue-add preserves delay-before and delay-after on the created item."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (item (agent-shell-queue-add "hello" buf nil 5.0 10.0)))
      (unwind-protect
          (progn
            (should (= 5.0 (agent-shell-queue-item-delay-before item)))
            (should (= 10.0 (agent-shell-queue-item-delay-after item))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/enqueue-with-delay-before-queues-when-idle ()
  "agent-shell-queue-enqueue adds to queue instead of sending immediately when delay-before > 0."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((buf (get-buffer-create " *asq-test-buf*"))
           (inserted nil))
      (with-current-buffer buf
        (setq major-mode 'agent-shell-mode))
      (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                ((symbol-function 'agent-shell-insert) (lambda (&rest _) (setq inserted t))))
        (unwind-protect
            (progn
              (agent-shell-queue-enqueue "delayed prompt" buf nil 3.0 2.0)
              ;; Because delay-before is set, it was queued, not inserted immediately
              (should-not inserted)
              (let ((items (cdr (assoc (buffer-name buf)
                                       (agent-shell-queue-store-items agent-shell-queue--store)))))
                (should (= 1 (length items)))
                (let ((item (car items)))
                  (should (equal "delayed prompt" (agent-shell-queue-item-args item)))
                  (should (= 3.0 (agent-shell-queue-item-delay-before item)))
                  (should (= 2.0 (agent-shell-queue-item-delay-after item))))))
          (kill-buffer buf))))))


(ert-deftest agent-shell-queue/add-multiple-buffers ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf1 (get-buffer-create " *asq-test-buf1*"))
          (buf2 (get-buffer-create " *asq-test-buf2*")))
      (unwind-protect
          (progn
            (agent-shell-queue-add "for-one" buf1)
            (agent-shell-queue-add "for-two" buf2)
            (should (= 2 (length (agent-shell-queue-store-items agent-shell-queue--store)))))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest agent-shell-queue/add-calls-ensure-subscription ()
  (agent-shell-queue-test/isolate
    (let ((subscribed-to nil)
          (buf (get-buffer-create " *asq-test-buf*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription)
                     (lambda (b) (setq subscribed-to b))))
            (agent-shell-queue-add "hello" buf)
            (should (eq buf subscribed-to)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-remove

(ert-deftest agent-shell-queue/remove-single-item ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-1")
    (should-not (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest agent-shell-queue/remove-one-of-many ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-remove "q-2")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (= 2 (length items)))
      (should (equal "q-1" (agent-shell-queue-item-id (nth 0 items))))
      (should (equal "q-3" (agent-shell-queue-item-id (nth 1 items)))))))

(ert-deftest agent-shell-queue/remove-last-item-drops-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
        (agent-shell-queue-remove "q-1")
        (should (equal "buf1" dropped))))))

(ert-deftest agent-shell-queue/remove-not-last-preserves-subscription ()
  (agent-shell-queue-test/isolate
    (let ((dropped nil))
      (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription)
                 (lambda (name) (setq dropped name)))
                ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
        (agent-shell-queue-remove "q-1")
        (should-not dropped)))))

(ert-deftest agent-shell-queue/remove-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-remove "q-999")
    (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-defer

(ert-deftest agent-shell-queue/defer-active-to-deferred ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'blocked.skip (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/defer-deferred-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" blocked.skip nil))))
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/defer-twice-returns-to-active ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-1")
    (agent-shell-queue-defer "q-1")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-set-background-task

(ert-deftest agent-shell-queue/set-background-task-on ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-set-background-task "q-1" t)
    (should (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

(ert-deftest agent-shell-queue/set-background-task-off ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active t))))
    (agent-shell-queue-set-background-task "q-1" nil)
    (should-not (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

(ert-deftest agent-shell-queue/set-background-task-idempotent ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-set-background-task "q-1" t)
    (agent-shell-queue-set-background-task "q-1" t)
    (should (agent-shell-queue-item-background (cadar (agent-shell-queue-store-items agent-shell-queue--store))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-edit

(ert-deftest agent-shell-queue/edit-replaces-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-1" "new prompt")
    (should (equal "new prompt"
                   (agent-shell-queue-item-args (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/edit-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "old" active nil))))
    (agent-shell-queue-edit "q-999" "new")
    (should (equal "old"
                   (agent-shell-queue-item-args (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--move / move-up / move-down

(ert-deftest agent-shell-queue/move-up-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-up "q-3")
    (should (equal '("q-1" "q-3" "q-2")
                   (seq-map #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-down-middle ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-2" "q-1" "q-3")
                   (seq-map #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-up-at-top-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-up "q-1")
    (should (equal '("q-1" "q-2")
                   (seq-map #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-down-at-bottom-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (agent-shell-queue-move-down "q-2")
    (should (equal '("q-1" "q-2")
                   (seq-map #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/move-only-item-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "a" active nil))))
    (agent-shell-queue-move-up "q-1")
    (agent-shell-queue-move-down "q-1")
    (should (equal '("q-1")
                   (seq-map #'agent-shell-queue-item-id (cdar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pause: batch (all sessions) and per-session

(ert-deftest agent-shell-queue/pause-adds-every-known-buffer-to-session-paused ()
  "pause (batch) adds every buffer name in the store to --session-paused."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "a" active nil))
                                            '("buf2" ("q-2" "b" active nil))))
    (agent-shell-queue-pause)
    (should (member "buf1" (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
    (should (member "buf2" (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))))

(ert-deftest agent-shell-queue/pause-marks-active-items-as-blocked-runner ()
  "pause (batch) converts active items in every bucket to blocked.runner."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'active nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue-pause)
      (should (eq 'blocked.runner (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue/resume-is-alias-for-unpause-all-sessions ()
  "resume (batch) delegates to `agent-shell-queue-unpause-all-sessions'."
  (agent-shell-queue-test/isolate
    (let (called)
      (cl-letf (((symbol-function 'agent-shell-queue-unpause-all-sessions)
                 (lambda () (setq called t))))
        (agent-shell-queue-resume))
      (should called))))

(ert-deftest agent-shell-queue/pause-then-resume-round-trip-clears-session-paused ()
  "pause (batch) then resume (batch) leaves no buffer in --session-paused."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-resume-roundtrip*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (agent-shell-queue-pause)
            (should (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
            (agent-shell-queue-resume)
            (should-not (agent-shell-queue-queue-session-paused agent-shell-queue--queue))
            (should (eq 'active (agent-shell-queue-item-status
                                  (car (cdr (assoc (buffer-name buf) (agent-shell-queue-store-items agent-shell-queue--store))))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-pause-adds-name ()
  "session-pause adds the buffer name to --session-paused."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (agent-shell-queue-session-pause buf)
            (should (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-resume-removes-name ()
  "session-resume removes the buffer name from --session-paused."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
            (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
              (agent-shell-queue-session-resume buf))
            (should-not (member (buffer-name buf) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/unpause-all-sessions-clears-list ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("buf1" "buf2" "buf3"))
    (agent-shell-queue-unpause-all-sessions)
    (should-not (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))

(ert-deftest agent-shell-queue/unpause-all-sessions-restores-blocked-runner-items ()
  "unpause-all-sessions converts blocked.runner items back to active."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'blocked.runner nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue-unpause-all-sessions)
      (should (eq 'active (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue/unpause-all-sessions-triggers-dispatch ()
  "unpause-all-sessions calls --send-next-for-buffer for each live buffer."
  (agent-shell-queue-test/isolate
    (let ((dispatched nil)
          (buf (get-buffer-create " *asq-unpause-all-dispatch-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'blocked.runner nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  (list (buffer-name buf)))
            (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer)
                       (lambda (b) (push (buffer-name b) dispatched))))
              (agent-shell-queue-unpause-all-sessions))
            (should (member (buffer-name buf) dispatched)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/unpause-all-sessions-skips-dispatch-for-dead-buffers ()
  "unpause-all-sessions does not crash or dispatch for buffer names with no live buffer."
  (agent-shell-queue-test/isolate
    (let ((dispatched nil))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list " *asq-dead-buf*"
                        (agent-shell-queue-test/make-item "q-1" "p" 'blocked.runner nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer)
                 (lambda (b) (push (buffer-name b) dispatched))))
        (agent-shell-queue-unpause-all-sessions))
      (should-not dispatched))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch: editing-ids and buffer-paused prevent sends

(ert-deftest agent-shell-queue/send-next-skips-editing-item ()
  "An item in (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) is never dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-edit-skip-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "edit me" 'active nil))))
            (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-next-skips-buffer-paused ()
  "Items targeting a session-paused buffer are not dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-buf-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-send-item

(ert-deftest agent-shell-queue/send-item-marks-running-and-sets-dispatched ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-send-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "prompt" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-insert) #'ignore)
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
                (should (eq 'running (agent-shell-queue-item-status item)))
                (should (numberp (agent-shell-queue-item-dispatched item))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-calls-insert-with-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((inserted-text nil)
          (inserted-buf nil)
          (buf (get-buffer-create " *asq-send-test2*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "the prompt" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (setq inserted-text (plist-get args :text)
                               inserted-buf  (plist-get args :shell-buffer))))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (should (equal "the prompt" inserted-text))
              (should (eq buf inserted-buf))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-background-wraps-prompt ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((inserted-text nil)
          (agent-shell-queue-background-prefix "/bg ")
          (buf (get-buffer-create " *asq-bg-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "do thing" 'active t))))
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (setq inserted-text (plist-get args :text))))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue-send-item "q-1")
              (should (equal "/bg do thing" inserted-text))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-dead-buffer-messages-no-pause ()
  "Dead target during dispatch emits a message but does not pause the session
queue or raise a user-error.  The queue is left running so other sessions
can continue dispatching."
  (agent-shell-queue-test/isolate-no-sub
    (let (msg-emitted)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("dead-buf" ("q-1" "hello" active nil))))
      (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg-emitted (apply #'format fmt args)))))
        (should-not (condition-case err
                        (progn (agent-shell-queue-send-item "q-1") nil)
                      (user-error err)))
        (should msg-emitted)
        (should (string-match-p "dead-buf" msg-emitted))
        (should-not (member "dead-buf"
                            (agent-shell-queue-queue-session-paused
                             agent-shell-queue--queue)))))))

;;; agent-shell-queue--check-stall / --schedule-stall-check

(ert-deftest agent-shell-queue/check-stall-alerts-when-still-running ()
  (agent-shell-queue-test/isolate-no-sub
    (let (alert-args
          (buf (get-buffer-create " *asq-stall-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "prompt" 'running nil))))
            (cl-letf (((symbol-function 'alert)
                       (lambda (&rest args) (setq alert-args args)))
                      ((symbol-function 'shell-maker-busy) (lambda () t)))
              (agent-shell-queue--check-stall "q-1")
              (should alert-args)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/check-stall-no-op-when-done ()
  (agent-shell-queue-test/isolate-no-sub
    (let (alert-args
          (buf (get-buffer-create " *asq-stall-test2*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "prompt" 'done nil))))
            (cl-letf (((symbol-function 'alert)
                       (lambda (&rest args) (setq alert-args args))))
              (agent-shell-queue--check-stall "q-1")
              (should-not alert-args)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/check-stall-no-op-when-item-missing ()
  (agent-shell-queue-test/isolate-no-sub
    (let (alert-args)
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest args) (setq alert-args args))))
        (agent-shell-queue--check-stall "no-such-id")
        (should-not alert-args)))))

(ert-deftest agent-shell-queue/schedule-stall-check-schedules-timer-when-enabled ()
  (let (scheduled-args
        (agent-shell-queue-stall-timeout 180))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest args) (setq scheduled-args args))))
      (agent-shell-queue--schedule-stall-check "q-1")
      (should (equal (list 180 nil #'agent-shell-queue--check-stall "q-1")
                     scheduled-args)))))

(ert-deftest agent-shell-queue/schedule-stall-check-noop-when-disabled ()
  (let (scheduled-args
        (agent-shell-queue-stall-timeout nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest args) (setq scheduled-args args))))
      (agent-shell-queue--schedule-stall-check "q-1")
      (should-not scheduled-args))))

(ert-deftest agent-shell-queue/send-item-uses-executor-when-set ()
  "When item has a non-nil executor, send-item calls it instead of agent-shell-insert."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((called-with-item nil)
           (called-with-args nil)
           (buf (get-buffer-create " *asq-executor-test*"))
           (executor (lambda (item args)
                       (setq called-with-item item
                             called-with-args args))))
      (unwind-protect
          (progn
            (let ((item (agent-shell-queue-item--make
                         :id "q-exec" :args "exec-args" :status 'active
                         :kind 'prompt :created 1000.0 :executor executor)))
              (setf (agent-shell-queue-store-items agent-shell-queue--store)
                    (list (list (buffer-name buf) item)))
              (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
                        ((symbol-function 'agent-shell-insert)
                         (lambda (&rest _) (error "agent-shell-insert should not be called"))))
                (agent-shell-queue-send-item "q-exec")
                (should (eq called-with-item item))
                (should (equal "exec-args" called-with-args)))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-item-skips-mode-block-check-for-executor ()
  "Items with an executor bypass the session-mode-blocked check."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((executor-called nil)
           (buf (get-buffer-create " *asq-executor-mode-test*"))
           (executor (lambda (_item _args) (setq executor-called t))))
      (unwind-protect
          (progn
            (let ((item (agent-shell-queue-item--make
                         :id "q-exec2" :args "text" :status 'active
                         :kind 'prompt :created 1000.0 :executor executor)))
              (setf (agent-shell-queue-store-items agent-shell-queue--store)
                    (list (list (buffer-name buf) item)))
              (cl-letf (((symbol-function 'buffer-live-p) (lambda (_) t))
                        ((symbol-function 'agent-shell-queue--session-mode-blocked-p)
                         (lambda (_) t)))
                (agent-shell-queue-send-item "q-exec2")
                (should executor-called))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Executor registry

(ert-deftest agent-shell-queue/register-executor-stores-and-retrieves ()
  "Registering an executor under a name lets it be found by that name."
  (let ((agent-shell-queue--executors nil))
    (agent-shell-queue-register-executor "test-exec" #'ignore)
    (let ((entry (agent-shell-queue--find-executor "test-exec")))
      (should entry)
      (should (eq #'ignore (agent-shell-queue-executor-executor entry))))))

(ert-deftest agent-shell-queue/register-executor-with-capture ()
  "Registering with a capture function stores it in the entry."
  (let ((agent-shell-queue--executors nil))
    (agent-shell-queue-register-executor "cap-exec" #'ignore #'ignore)
    (let ((entry (agent-shell-queue--find-executor "cap-exec")))
      (should entry)
      (should (eq #'ignore (agent-shell-queue-executor-capture entry))))))

(ert-deftest agent-shell-queue/register-executor-replaces-existing ()
  "Re-registering under the same name replaces the entry."
  (let ((agent-shell-queue--executors nil))
    (agent-shell-queue-register-executor "dup" #'ignore)
    (agent-shell-queue-register-executor "dup" #'identity)
    (should (= 1 (length agent-shell-queue--executors)))
    (should (eq #'identity (agent-shell-queue-executor-executor
                            (agent-shell-queue--find-executor "dup"))))))

(ert-deftest agent-shell-queue/executor-name-returns-nil-for-unregistered ()
  "executor-name returns nil for functions not in the registry."
  (let ((agent-shell-queue--executors nil))
    (should (null (agent-shell-queue--executor-name #'ignore)))))

(ert-deftest agent-shell-queue/executor-name-returns-name-when-registered ()
  "executor-name returns the name string for a registered function."
  (let ((agent-shell-queue--executors nil))
    (agent-shell-queue-register-executor "my-exec" #'ignore)
    (should (equal "my-exec" (agent-shell-queue--executor-name #'ignore)))))

(ert-deftest agent-shell-queue/executor-from-plist-returns-nil-for-nil ()
  "executor-from-plist returns nil when given nil (no executor)."
  (let ((agent-shell-queue--executors nil))
    (should (null (agent-shell-queue--executor-from-plist nil)))))

(ert-deftest agent-shell-queue/executor-from-plist-returns-nil-for-unknown-name ()
  "executor-from-plist returns nil and warns for an unknown name."
  (let ((agent-shell-queue--executors nil))
    (should (null (agent-shell-queue--executor-from-plist "no-such-executor")))))

(ert-deftest agent-shell-queue/executor-plist-roundtrip ()
  "A registered executor survives a to-plist / from-plist round-trip."
  (let ((agent-shell-queue--executors nil))
    (agent-shell-queue-register-executor "rtrip-exec" #'ignore)
    (let* ((item (agent-shell-queue-item--make
                  :id "q-rt" :args "hi" :status 'active :kind 'prompt
                  :created 1000.0 :executor #'ignore))
           (restored (agent-shell-queue-item-from-plist
                      (agent-shell-queue-item-to-plist item))))
      (should (eq #'ignore (agent-shell-queue-item-executor restored))))))

(ert-deftest agent-shell-queue/executor-plist-roundtrip-unregistered-becomes-nil ()
  "An unregistered closure serializes as nil and deserializes as nil."
  (let ((agent-shell-queue--executors nil)
        (closure (lambda (_item _args) nil)))
    (let* ((item (agent-shell-queue-item--make
                  :id "q-cl" :args "hi" :status 'active :kind 'prompt
                  :created 1000.0 :executor closure))
           (restored (agent-shell-queue-item-from-plist
                      (agent-shell-queue-item-to-plist item))))
      (should (null (agent-shell-queue-item-executor restored))))))

(ert-deftest agent-shell-queue/default-executor-is-auto-registered ()
  "agent-shell-queue--default-executor is pre-registered."
  (let ((entry (agent-shell-queue--find-executor
                "agent-shell-queue--default-executor")))
    (should entry)
    (should (eq #'agent-shell-queue--default-executor
                (agent-shell-queue-executor-executor entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--mark-running-done

(ert-deftest agent-shell-queue/mark-running-done-sets-status ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (eq 'done (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/mark-running-done-sets-completed-time ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (numberp (agent-shell-queue-item-completed (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/mark-running-done-only-running-items ()
  "Active items are not affected."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf1" ("q-1" "p" running nil) ("q-2" "p2" active nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (eq 'done   (agent-shell-queue-item-status (nth 0 items))))
      (should (eq 'active (agent-shell-queue-item-status (nth 1 items)))))))

(ert-deftest agent-shell-queue/mark-running-done-alert-only-when-marked ()
  "Alert fires only when at least one item was actually marked done."
  (agent-shell-queue-test/isolate
    (let ((alert-fired nil))
      (cl-letf (((symbol-function 'agent-shell-queue--alert-if-empty)
                 (lambda () (setq alert-fired t))))
        ;; No running items — mark should NOT fire alert
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--mark-running-done "buf1")
        (should-not alert-fired)
        ;; Running item — mark SHOULD fire alert
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-2" "p" running nil))))
        (agent-shell-queue--mark-running-done "buf1")
        (should alert-fired)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--alert-if-empty

(ert-deftest agent-shell-queue/alert-if-empty-fires-when-no-work ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" done nil))))
        (agent-shell-queue--alert-if-empty)
        (should alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-active-work ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
        (agent-shell-queue--alert-if-empty)
        (should-not alerted)))))

(ert-deftest agent-shell-queue/alert-if-empty-silent-when-running ()
  (agent-shell-queue-test/isolate
    (let ((alerted nil))
      (cl-letf (((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
        (agent-shell-queue--alert-if-empty)
        (should-not alerted)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-reenqueue

(ert-deftest agent-shell-queue/reenqueue-creates-new-item ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-reenq-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "done prompt" 'done nil))))
            (setf (agent-shell-queue-item-completed (cadar (agent-shell-queue-store-items agent-shell-queue--store))) 2000.0)
            (agent-shell-queue-reenqueue "q-1")
            ;; Original done item still there + new active item
            (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
              (should (= 2 (length items)))
              (let ((new-item (nth 1 items)))
                (should (equal "done prompt" (agent-shell-queue-item-args new-item)))
                (should (eq 'active (agent-shell-queue-item-status new-item))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-non-done-errors ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-reenq-err-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "active" 'active nil))))
            (should-error (agent-shell-queue-reenqueue "q-1") :type 'user-error))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-dead-buffer-prompts-for-replacement ()
  "When the original target buffer is dead, reenqueue prompts for a live one."
  (agent-shell-queue-test/isolate-no-sub
    (let ((live-buf (get-buffer-create " *asq-reenq-live*")))
      (unwind-protect
          (progn
            ;; Item stored under a dead buffer name
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list " *asq-dead-buf*"
                              (agent-shell-queue-test/make-item "q-1" "hello" 'done nil))))
            (cl-letf (((symbol-function 'agent-shell-queue--pick-buffer)
                       (lambda (_prompt) live-buf)))
              (agent-shell-queue-reenqueue "q-1"))
            (let ((all-items (seq-mapcat #'cdr
                                         (agent-shell-queue-store-items agent-shell-queue--store))))
              ;; A new active item exists targeting live-buf
              (should (seq-find (lambda (it)
                                  (and (equal "hello" (agent-shell-queue-item-args it))
                                       (eq 'active (agent-shell-queue-item-status it))))
                                all-items))))
        (kill-buffer live-buf)))))

(ert-deftest agent-shell-queue/reenqueue-dead-buffer-no-live-buffers-errors ()
  "When target is dead and no live buffers exist, reenqueue signals user-error."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (list (list " *asq-gone*"
                      (agent-shell-queue-test/make-item "q-1" "hello" 'done nil))))
    (cl-letf (((symbol-function 'agent-shell-queue--pick-buffer)
               (lambda (_prompt) nil)))
      (should-error (agent-shell-queue-reenqueue "q-1") :type 'user-error))))

(ert-deftest agent-shell-queue/redirect-dead-target-alerts-and-pauses ()
  "Dead target during automatic dispatch emits a high-severity alert and
pauses the session queue instead of prompting interactively."
  (agent-shell-queue-test/isolate
    (let (alerted paused-sessions)
      (cl-letf (((symbol-function 'alert)
                 (lambda (msg &rest args)
                   (setq alerted (list msg (plist-get args :severity)))))
                ((symbol-function 'agent-shell-queue--save) #'ignore)
                ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore))
        (agent-shell-queue--redirect-dead-target "q-1" " *dead-buf*")
        (setq paused-sessions
              (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
      (should alerted)
      (should (eq 'high (cadr alerted)))
      (should (member " *dead-buf*" paused-sessions)))))

(ert-deftest agent-shell-queue/item-view-reenqueue-no-stray-form ()
  "Regression: a bare URL was accidentally left after (interactive) in
`agent-shell-queue-item-view-reenqueue', causing Symbol's value as variable
is void: https://... when the command was invoked on an aborted item."
  (agent-shell-queue-test/isolate-no-sub
    (let ((buf (get-buffer-create " *asq-iv-reenq-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "https://example.com/pr/42" 'aborted nil))))
            (with-temp-buffer
              (setq-local agent-shell-queue--item-view-id "q-1")
              (cl-letf (((symbol-function 'quit-window) #'ignore))
                (should-not (condition-case err
                                (progn (agent-shell-queue-item-view-reenqueue) nil)
                              (void-variable err))))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--assign-item

(ert-deftest agent-shell-queue/retarget-moves-item ()
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'agent-shell-queue--drop-subscription) #'ignore)
              ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
      (agent-shell-queue--assign-item "q-1" "buf2")
      (should-not (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store)))
      (let ((bucket (assoc "buf2" (agent-shell-queue-store-items agent-shell-queue--store))))
        (should bucket)
        (should (equal "q-1" (agent-shell-queue-item-id (cadr bucket))))))))

(ert-deftest agent-shell-queue/retarget-same-buffer-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue--assign-item "q-1" "buf1")
    (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
    (should (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence: save/load roundtrip

(ert-deftest agent-shell-queue/save-load-roundtrip ()
  (let* ((tmp (make-temp-file "asq-test"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--loaded t)
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (agent-shell-queue-test/populate
                 '("mybuf" ("q-5" "persisted" blocked.skip t))))
          (agent-shell-queue--save)
          (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
          (agent-shell-queue--load)
          (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
          (let* ((pair (car (agent-shell-queue-store-items agent-shell-queue--store)))
                 (item (cadr pair)))
            (should (equal "mybuf" (car pair)))
            (should (equal "q-5" (agent-shell-queue-item-id item)))
            (should (equal "persisted" (agent-shell-queue-item-args item)))
            (should (eq 'blocked.skip (agent-shell-queue-item-status item)))
            (should (agent-shell-queue-item-background item)))
          ;; save sets flush time
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/save-sets-flush-time ()
  (let* ((tmp (make-temp-file "asq-flush"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--last-flush-time nil))
    (unwind-protect
        (progn
          (agent-shell-queue--save)
          (should (numberp agent-shell-queue--last-flush-time)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-missing-file-is-noop ()
  (let* ((agent-shell-queue-state-file-function
          (lambda () "/tmp/asq-test-definitely-does-not-exist-xyz"))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (should-not (condition-case err
                    (progn (agent-shell-queue--load) nil)
                  (error err)))
    (should-not (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest agent-shell-queue/load-corrupt-file-is-noop ()
  (let* ((tmp (make-temp-file "asq-corrupt"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "this is not valid elisp )))"))
          (should-not (condition-case err
                          (progn (agent-shell-queue--load) nil)
                        (error err)))
          (should-not (agent-shell-queue-store-items agent-shell-queue--store)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/load-restores-multiple-items ()
  "Loading persisted state restores all items across buckets."
  (let* ((tmp (make-temp-file "asq-multi"))
         (agent-shell-queue-state-file-function (lambda () tmp))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (unwind-protect
        (progn
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (agent-shell-queue-test/populate
                 '("buf" ("q-7" "a" active nil) ("q-12" "b" active nil))))
          (agent-shell-queue--save)
          (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
          (agent-shell-queue--load)
          (should (= 1 (length (agent-shell-queue-store-items agent-shell-queue--store))))
          (should (= 2 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialization: plist format

(ert-deftest agent-shell-queue/serialize-plist-roundtrip ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'plist)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-3" "hello plist" deferred t))))
    (let* ((str   (agent-shell-queue-serialize agent-shell-queue--store))
           (items (agent-shell-queue-deserialize agent-shell-queue--store str))
           (item  (cadr (car items))))
      (should (equal "buf"         (caar items)))
      (should (equal "q-3"         (agent-shell-queue-item-id item)))
      (should (equal "hello plist" (agent-shell-queue-item-args item)))
      (should (eq 'deferred        (agent-shell-queue-item-status item)))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/serialize-plist-symbols-survive ()
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'plist)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("b" ("q-1" "a" active nil) ("q-2" "b" deferred nil))))
    (let* ((items (agent-shell-queue-deserialize agent-shell-queue--store
                                                 (agent-shell-queue-serialize agent-shell-queue--store)))
           (list  (cdar items)))
      (should (eq 'active   (agent-shell-queue-item-status (nth 0 list))))
      (should (eq 'deferred (agent-shell-queue-item-status (nth 1 list))))
      (should-not (agent-shell-queue-item-background (nth 0 list))))))

;;; Serialization: item plist struct accessors

(ert-deftest agent-shell-queue/item-to-plist-fields ()
  "to-plist includes all struct fields including dispatched."
  (let* ((item (agent-shell-queue-test/make-item "q-1" "prompt" 'active t))
         (pl   (agent-shell-queue-item-to-plist item)))
    (should (equal "q-1"    (plist-get pl :id)))
    (should (equal "prompt" (plist-get pl :args)))
    (should (eq 'active     (plist-get pl :status)))
    (should (eq t           (plist-get pl :background)))
    (should (numberp        (plist-get pl :created)))
    (should (null           (plist-get pl :dispatched)))
    (should (null           (plist-get pl :completed)))))

(ert-deftest agent-shell-queue/item-from-plist-roundtrip ()
  (let* ((orig (agent-shell-queue-test/make-item "q-7" "text" 'deferred nil))
         (item (agent-shell-queue-item-from-plist
                (agent-shell-queue-item-to-plist orig))))
    (should (equal (agent-shell-queue-item-id         orig) (agent-shell-queue-item-id         item)))
    (should (equal (agent-shell-queue-item-args     orig) (agent-shell-queue-item-args     item)))
    (should (eq    (agent-shell-queue-item-status     orig) (agent-shell-queue-item-status     item)))
    (should (eq    (agent-shell-queue-item-background orig) (agent-shell-queue-item-background item)))
    (should (=     (agent-shell-queue-item-created    orig) (agent-shell-queue-item-created    item)))))

(ert-deftest agent-shell-queue/item-from-plist-old-prompt-alias ()
  "from-plist accepts old-style :prompt key for backward compatibility."
  (let* ((old-plist (list :id "q-alias" :prompt "backward-compat" :status 'active
                          :kind 'prompt :background nil :created 1000.0
                          :dispatched nil :completed nil :response nil :outcome nil
                          :directory nil))
         (item (agent-shell-queue-item-from-plist old-plist)))
    (should (equal "backward-compat" (agent-shell-queue-item-args item)))
    (should (equal "q-alias" (agent-shell-queue-item-id item)))))

;;; Serialization: JSON format

(ert-deftest agent-shell-queue/serialize-json-roundtrip ()
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'json)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-4" "hello json" deferred t))))
    (let* ((str   (agent-shell-queue-serialize agent-shell-queue--store))
           (items (agent-shell-queue-deserialize agent-shell-queue--store str))
           (item  (cadr (car items))))
      (should (equal "buf"        (caar items)))
      (should (equal "q-4"        (agent-shell-queue-item-id item)))
      (should (equal "hello json" (agent-shell-queue-item-args item)))
      (should (eq 'deferred       (agent-shell-queue-item-status item)))
      (should (agent-shell-queue-item-background item)))))

(ert-deftest agent-shell-queue/serialize-json-status-is-symbol ()
  (skip-unless (fboundp 'json-serialize))
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-format agent-shell-queue--store) 'json)
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("b" ("q-1" "p" active nil))))
    (let ((item (cadr (car (agent-shell-queue-deserialize
                            agent-shell-queue--store
                            (agent-shell-queue-serialize agent-shell-queue--store))))))
      (should (eq 'active (agent-shell-queue-item-status item)))
      (should (symbolp    (agent-shell-queue-item-status item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Archive

(ert-deftest agent-shell-queue/write-archive-appends-jsonl ()
  "Write archive writes a valid JSON line to the file."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-archive"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (agent-shell-queue-instance-name "test-instance")
         (buf (get-buffer-create " *asq-archive-buf*")))
    (unwind-protect
        (progn
          (let* ((item (agent-shell-queue-test/make-item "q-1" "archive me" 'done nil)))
            (setf (agent-shell-queue-item-dispatched item) 1000.5)
            (setf (agent-shell-queue-item-completed  item) 1060.5)
            (cl-letf (((symbol-function 'lock-file) #'ignore)
                      ((symbol-function 'unlock-file) #'ignore))
              (agent-shell-queue--write-archive (buffer-name buf) item)))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist)))
            (should (equal "q-1"           (plist-get parsed :id)))
            (should (equal "archive me"    (plist-get parsed :args)))
            (should (equal "test-instance" (plist-get parsed :instance)))
            (should (eq t                  (plist-get parsed :ran)))
            (should (= 60.0                (plist-get parsed :runtime)))))
      (ignore-errors (delete-file tmp))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/write-archive-noop-when-disabled ()
  "Write archive does nothing when archiving is disabled."
  (let ((agent-shell-queue-archive-enabled nil))
    (should-not (condition-case err
                    (progn
                      (agent-shell-queue--write-archive
                       "buf" (agent-shell-queue-test/make-item "q-1" "p" 'done nil))
                      nil)
                  (error err)))))

(ert-deftest agent-shell-queue/write-archive-ran-false-when-not-dispatched ()
  "Items never dispatched have :ran false in archive."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-archive-ran"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (agent-shell-queue-instance-name "ti"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'lock-file) #'ignore)
                    ((symbol-function 'unlock-file) #'ignore))
            (agent-shell-queue--write-archive
             "buf" (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist
                                            :false-object nil)))
            (should-not (plist-get parsed :ran))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subscriptions

(ert-deftest agent-shell-queue/ensure-subscription-registers ()
  (agent-shell-queue-test/isolate
    (let ((subscribe-calls nil)
          (buf (get-buffer-create " *asq-sub-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest args)
                       (push args subscribe-calls)
                       42)))
            (agent-shell-queue--ensure-subscription buf)
            (should (= 2 (length subscribe-calls)))
            (let ((events (seq-map (lambda (a) (plist-get a :event)) subscribe-calls)))
              (should (member 'turn-complete events))
              (should (member 'clean-up events)))
            (should (assoc (buffer-name buf) agent-shell-queue--subscriptions)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/ensure-subscription-no-duplicates ()
  (agent-shell-queue-test/isolate
    (let ((call-count 0)
          (buf (get-buffer-create " *asq-sub-dup-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest _) (cl-incf call-count) call-count)))
            (agent-shell-queue--ensure-subscription buf)
            (agent-shell-queue--ensure-subscription buf)
            (should (= 2 call-count)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-removes-entry ()
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-drop-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--subscriptions
                  (list (cons (buffer-name buf) 99)))
            (cl-letf (((symbol-function 'agent-shell-unsubscribe) #'ignore))
              (agent-shell-queue--drop-subscription (buffer-name buf))
              (should-not (assoc (buffer-name buf) agent-shell-queue--subscriptions))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-safe-on-dead-buffer ()
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions (list (cons "dead-buf" 99)))
    (should-not (condition-case err
                    (progn (agent-shell-queue--drop-subscription "dead-buf") nil)
                  (error err)))
    (should-not (assoc "dead-buf" agent-shell-queue--subscriptions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-send (backup timer)

(ert-deftest agent-shell-queue/auto-send-skips-deferred ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-auto-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "def" 'deferred nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-skips-busy-buffer ()
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-busy-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "hello" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-only-first-active ()
  "auto-send sends at most one item per bucket per call."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent-count 0)
          (buf (get-buffer-create " *asq-first-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "a" 'active nil)
                              (agent-shell-queue-test/make-item "q-2" "b" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (cl-incf sent-count))))
              (agent-shell-queue--auto-send)
              (should (= 1 sent-count))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--refresh-buffer guard chain
;; Note: these tests call --refresh-buffer directly without the isolate
;; macro, since isolate stubs out the function under test.

(ert-deftest agent-shell-queue/refresh-buffer-skips-dead-buffer ()
  "No error and no refresh when the queue buffer does not exist."
  (let ((refreshed nil))
    (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
               (lambda () (setq refreshed t)))
              ((symbol-function 'get-buffer)
               (lambda (_) nil)))
      (agent-shell-queue--refresh-buffer)
      (should-not refreshed))))

(ert-deftest agent-shell-queue/refresh-buffer-skips-wrong-mode ()
  "No refresh when the queue buffer is live but not in agent-shell-queue-mode."
  (let ((refreshed nil)
        (buf (get-buffer-create " *asq-refresh-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'get-buffer)
                   (lambda (_) buf))
                  ((symbol-function 'derived-mode-p)
                   (lambda (_) nil)))
          (agent-shell-queue--refresh-buffer)
          (should-not refreshed))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/refresh-buffer-calls-refresh-when-live-and-mode ()
  "Refresh is called when the buffer is live and in agent-shell-queue-mode."
  (let ((refreshed nil)
        (buf (get-buffer-create " *asq-refresh-live-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue-buffer-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'get-buffer)
                   (lambda (_) buf))
                  ((symbol-function 'derived-mode-p)
                   (lambda (_) t)))
          (agent-shell-queue--refresh-buffer)
          (should refreshed))
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--session-mode-blocked-p guard chain

(ert-deftest agent-shell-queue/session-mode-blocked-dead-buffer ()
  "Returns nil when the buffer is dead."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-dead*")))
      (kill-buffer buf)
      (should-not (agent-shell-queue--session-mode-blocked-p buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-no-mode-id ()
  "Returns nil when map-nested-elt finds no :session :mode-id."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-no-id*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) nil)))
            (should-not (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-mode-not-in-list ()
  "Returns nil when mode-id is not in blocked-session-modes."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-not-in*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) "other-mode")))
            (should-not (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/session-mode-blocked-mode-in-list ()
  "Returns non-nil when mode-id is in blocked-session-modes."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-mode-blocked-in*"))
          (agent-shell-queue-blocked-session-modes '("blocked-mode" "other")))
      (unwind-protect
          (cl-letf (((symbol-function 'map-nested-elt)
                     (lambda (&rest _) "blocked-mode")))
            (should (agent-shell-queue--session-mode-blocked-p buf)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--drop-subscription — setq is unconditional

(ert-deftest agent-shell-queue/drop-subscription-cleans-up-dead-buffer ()
  "Subscription entry is removed from registry even when the buffer is dead."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions
          (list (cons "dead-buf" 'fake-token)))
    (cl-letf (((symbol-function 'get-buffer) (lambda (_) nil)))
      (agent-shell-queue--drop-subscription "dead-buf"))
    (should-not agent-shell-queue--subscriptions)))

(ert-deftest agent-shell-queue/drop-subscription-cleans-up-non-agent-shell-mode ()
  "Subscription entry is removed even when buffer is live but wrong mode."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-drop-sub-test*")))
      (unwind-protect
          (progn
            (setq agent-shell-queue--subscriptions
                  (list (cons (buffer-name buf) 'fake-token)))
            (cl-letf (((symbol-function 'derived-mode-p) (lambda (_) nil))
                      ((symbol-function 'agent-shell-unsubscribe) #'ignore))
              (agent-shell-queue--drop-subscription (buffer-name buf)))
            (should-not agent-shell-queue--subscriptions))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/drop-subscription-unknown-buf-name-noop ()
  "Calling with an unregistered name does not error and leaves registry intact."
  (agent-shell-queue-test/isolate
    (setq agent-shell-queue--subscriptions
          (list (cons "other-buf" 'token)))
    (agent-shell-queue--drop-subscription "no-such-buf")
    (should (= 1 (length agent-shell-queue--subscriptions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-buffer-open-shell

(ert-deftest agent-shell-queue/buffer-open-shell-pops-live-buffer ()
  "Pops to the shell buffer directly when it is live."
  (agent-shell-queue-test/isolate
    (let ((buf (get-buffer-create " *asq-shell-live-test*"))
          (popped nil))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-live" "hello" 'active nil))))
            (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "q-live"))
                      ((symbol-function 'pop-to-buffer) (lambda (b) (setq popped b))))
              (agent-shell-queue-buffer-open-shell)
              (should (eq popped buf))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/buffer-open-shell-errors-when-dead-no-create ()
  "Signals user-error when buffer is dead and executor has no create function."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("dead-buf" ("q-dead" "hello" active nil))))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "q-dead"))
              ((symbol-function 'get-buffer) (lambda (_) nil)))
      (should-error (agent-shell-queue-buffer-open-shell) :type 'user-error))))

(ert-deftest agent-shell-queue/buffer-open-shell-creates-when-dead-and-user-confirms ()
  "Calls create-fn and pops to the result when buffer is dead and user answers yes."
  (agent-shell-queue-test/isolate
    (let* ((agent-shell-queue--executors nil)
           (exec-fn (lambda (_item _args) nil))
           (new-buf (get-buffer-create " *asq-shell-created*"))
           (popped nil))
      (unwind-protect
          (progn
            (agent-shell-queue-register-executor "test-exec" exec-fn nil (lambda () new-buf))
            (let ((item (agent-shell-queue-test/make-item "q-create" "hello" 'active nil)))
              (setf (agent-shell-queue-item-executor item) exec-fn)
              (setf (agent-shell-queue-store-items agent-shell-queue--store)
                    (list (list "dead-buf" item))))
            (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "q-create"))
                      ((symbol-function 'get-buffer) (lambda (_) nil))
                      ((symbol-function 'y-or-n-p) (lambda (_) t))
                      ((symbol-function 'pop-to-buffer) (lambda (b) (setq popped b))))
              (agent-shell-queue-buffer-open-shell)
              (should (eq popped new-buf))))
        (kill-buffer new-buf)))))

(ert-deftest agent-shell-queue/buffer-open-shell-noop-when-dead-and-user-declines ()
  "Does nothing when buffer is dead, create-fn exists, but user answers no."
  (agent-shell-queue-test/isolate
    (let* ((agent-shell-queue--executors nil)
           (exec-fn (lambda (_item _args) nil))
           (created nil)
           (popped nil))
      (agent-shell-queue-register-executor
       "test-exec" exec-fn nil
       (lambda () (setq created t) (get-buffer-create " *asq-shell-declined*")))
      (let ((item (agent-shell-queue-test/make-item "q-decline" "hello" 'active nil)))
        (setf (agent-shell-queue-item-executor item) exec-fn)
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (list (list "dead-buf" item))))
      (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "q-decline"))
                ((symbol-function 'get-buffer) (lambda (_) nil))
                ((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'pop-to-buffer) (lambda (b) (setq popped b))))
        (agent-shell-queue-buffer-open-shell)
        (should-not created)
        (should-not popped)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-defer — unknown id is noop

(ert-deftest agent-shell-queue/defer-unknown-id-is-noop ()
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (agent-shell-queue-defer "q-999")
    (should (eq 'active (agent-shell-queue-item-status (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--auto-send — when-let* short-circuits

(ert-deftest agent-shell-queue/auto-send-skips-session-paused ()
  "Items targeting a session-paused buffer are not dispatched."
  (agent-shell-queue-test/isolate-no-sub
    (let ((sent nil)
          (buf (get-buffer-create " *asq-sess-pause-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'active nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) (list (buffer-name buf)))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert)
                       (lambda (&rest _) (setq sent t))))
              (agent-shell-queue--auto-send)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/auto-send-dispatches-next-item ()
  "auto-send dispatches the first active item for an idle buffer."
  (agent-shell-queue-test/isolate-no-sub
    (let ((dispatched-id nil)
          (buf (get-buffer-create " *asq-auto-send-dispatch*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "first" 'active nil)
                              (agent-shell-queue-test/make-item "q-2" "second" 'active nil))))
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-queue-send-item)
                       (lambda (id) (setq dispatched-id id))))
              (agent-shell-queue--auto-send)
              (should (equal "q-1" dispatched-id))))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--write-archive — :ran field follows dispatched

(ert-deftest agent-shell-queue/write-archive-ran-when-dispatched ()
  ":ran is t when dispatched is non-nil."
  (let* ((tmp (make-temp-file "asq-archive-ran"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (item (agent-shell-queue-item--make
                :id "q-1" :args "p" :status 'done
                :background nil :created 1000.0
                :dispatched 2000.0 :completed 3000.0)))
    (unwind-protect
        (progn
          (agent-shell-queue--write-archive "buf" item)
          (let* ((raw (with-temp-buffer
                        (insert-file-contents tmp)
                        (buffer-string)))
                 (parsed (json-parse-string raw)))
            (should (eq t (gethash "ran" parsed)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/write-archive-not-ran-when-no-dispatched ()
  ":ran is :false when dispatched is nil."
  (let* ((tmp (make-temp-file "asq-archive-norun"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (item (agent-shell-queue-item--make
                :id "q-2" :args "p" :status 'done
                :background nil :created 1000.0
                :dispatched nil :completed nil)))
    (unwind-protect
        (progn
          (agent-shell-queue--write-archive "buf" item)
          (let* ((raw (with-temp-buffer
                        (insert-file-contents tmp)
                        (buffer-string)))
                 (parsed (json-parse-string raw)))
            (should (eq :false (gethash "ran" parsed)))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-with-paused-session — macro

(ert-deftest agent-shell-queue/with-paused-session-pauses-during-body ()
  "Session is paused while body executes, resumed afterward."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-session*" t)
      (let ((paused-during nil))
        (agent-shell-queue-with-paused-session (current-buffer)
          (setq paused-during
                (member (buffer-name) (agent-shell-queue-queue-session-paused
                                       agent-shell-queue--queue))))
        ;; After macro: session should be resumed (removed from paused list)
        (should paused-during)
        (should (not (member (buffer-name)
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-resumes-on-error ()
  "Session is resumed even when body signals an error."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-err-session*" t)
      (let ((buf-name (buffer-name)))
        (ignore-errors
          (agent-shell-queue-with-paused-session (current-buffer)
            (error "deliberate test error")))
        (should (not (member buf-name
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-accepts-string ()
  "Macro works when BUF is a buffer name string."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-str-session*" t)
      (let ((buf-name (buffer-name))
            (paused-during nil))
        (agent-shell-queue-with-paused-session buf-name
          (setq paused-during
                (member buf-name (agent-shell-queue-queue-session-paused
                                  agent-shell-queue--queue))))
        (should paused-during)
        (should (not (member buf-name
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

(ert-deftest agent-shell-queue/with-paused-session-returns-body-value ()
  "Macro returns the value of its body."
  (agent-shell-queue-test/isolate
    (with-temp-buffer
      (rename-buffer "*test-retval-session*" t)
      (let ((result (agent-shell-queue-with-paused-session (current-buffer)
                      (+ 1 2))))
        (should (= 3 result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-collect-items

(ert-deftest agent-shell-queue/fork-collect-items-nil-from-id ()
  "With nil from-id, collects all active/blocked/draft items but not done."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" blocked.skip nil)
            ("q3" "p3" done nil)
            ("q4" "p4" draft nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" nil)))
      (should (= 3 (length collected)))
      (should (member "q1" (seq-map #'agent-shell-queue-item-id collected)))
      (should (member "q2" (seq-map #'agent-shell-queue-item-id collected)))
      (should (member "q4" (seq-map #'agent-shell-queue-item-id collected)))
      (should (not (member "q3" (seq-map #'agent-shell-queue-item-id collected)))))))

(ert-deftest agent-shell-queue/fork-collect-items-from-id ()
  "With from-id, collects active items at and after that position."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" active nil)
            ("q3" "p3" active nil)
            ("q4" "p4" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "q2")))
      (should (= 3 (length collected)))
      (should (seq-every-p (lambda (it)
                             (member (agent-shell-queue-item-id it) '("q2" "q3" "q4")))
                           collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-from-id-skips-done ()
  "Skips done and running items even when they appear in range."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*"
            ("q1" "p1" active nil)
            ("q2" "p2" running nil)
            ("q3" "p3" active nil)
            ("q4" "p4" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "q2")))
      ;; q2 is running → not eligible; q3, q4 are active
      (should (= 2 (length collected)))
      (should (seq-every-p (lambda (it)
                             (member (agent-shell-queue-item-id it) '("q3" "q4")))
                           collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-unknown-from-id ()
  "Unknown from-id returns empty list."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*s*" ("q1" "p1" active nil))))
    (let ((collected (agent-shell-queue--fork-collect-items "*s*" "no-such-id")))
      (should (null collected)))))

(ert-deftest agent-shell-queue/fork-collect-items-empty-bucket ()
  "Returns nil for an empty bucket."
  (agent-shell-queue-test/isolate
    (let ((collected (agent-shell-queue--fork-collect-items "*nonexistent*" nil)))
      (should (null collected)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-fork-session

(defmacro agent-shell-queue-test/with-fork-stubs (new-buf-name &rest body)
  "Execute BODY with session-creation stubbed to return a buffer named NEW-BUF-NAME.
Also stubs subscription management and `sit-for' to avoid side effects."
  (declare (indent 1))
  `(agent-shell-queue-test/isolate-no-sub
    (cl-letf (((symbol-function 'agent-shell-queue--fork-create-session)
               (lambda (_src _mode _dir) (get-buffer-create ,new-buf-name)))
              ((symbol-function 'sit-for) #'ignore))
      ,@body)))

(ert-deftest agent-shell-queue/fork-session-moves-items-to-new-buffer ()
  "Items at/after from-id are moved to the new session buffer."
  (agent-shell-queue-test/with-fork-stubs "*new-session*"
    (with-temp-buffer
      (rename-buffer "*src-session*" t)
      (let ((src-buf (current-buffer)))
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("*src-session*"
                ("q1" "p1" active nil)
                ("q2" "p2" active nil)
                ("q3" "p3" active nil))))
        (agent-shell-queue-fork-session src-buf "q2")
        ;; q1 remains in source
        (let ((src-items (cdr (assoc "*src-session*"
                                     (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 1 (length src-items)))
          (should (equal "q1" (agent-shell-queue-item-id (car src-items)))))
        ;; q2 and q3 moved to new session
        (let ((new-items (cdr (assoc "*new-session*"
                                     (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 2 (length new-items)))
          (should (member "q2" (seq-map #'agent-shell-queue-item-id new-items)))
          (should (member "q3" (seq-map #'agent-shell-queue-item-id new-items))))
        (kill-buffer "*new-session*")))))

(ert-deftest agent-shell-queue/fork-session-nil-from-id-moves-all ()
  "With nil from-id, all active items are moved to the new session."
  (agent-shell-queue-test/with-fork-stubs "*new-all*"
    (with-temp-buffer
      (rename-buffer "*src-all*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-all*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (agent-shell-queue-fork-session (current-buffer) nil)
      (let ((src-items (cdr (assoc "*src-all*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (null src-items)))
      (let ((new-items (cdr (assoc "*new-all*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length new-items))))
      (kill-buffer "*new-all*"))))

(ert-deftest agent-shell-queue/fork-session-capture-pending-marks-status ()
  "With :capture-pending, items get pending-fork status and stay in source."
  (agent-shell-queue-test/with-fork-stubs "*fork-pending-new*"
    (with-temp-buffer
      (rename-buffer "*src-pending*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-pending*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil)
              ("q3" "p3" active nil))))
      (agent-shell-queue-fork-session (current-buffer) "q2"
                                      '(:capture-pending t))
      ;; q1 untouched; q2 and q3 become pending-fork
      (let ((items (cdr (assoc "*src-pending*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (eq 'active (agent-shell-queue-item-status
                             (cl-find "q1" items :key #'agent-shell-queue-item-id
                                      :test #'equal))))
        (should (eq 'pending-fork (agent-shell-queue-item-status
                                   (cl-find "q2" items :key #'agent-shell-queue-item-id
                                            :test #'equal))))
        (should (eq 'pending-fork (agent-shell-queue-item-status
                                   (cl-find "q3" items :key #'agent-shell-queue-item-id
                                            :test #'equal)))))
      ;; source session remains paused after capture-pending
      (should (member "*src-pending*"
                      (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
      (kill-buffer "*fork-pending-new*"))))

(ert-deftest agent-shell-queue/fork-session-errors-when-no-items ()
  "Signals user-error when there are no eligible items to fork."
  (agent-shell-queue-test/with-fork-stubs "*no-items-new*"
    (with-temp-buffer
      (rename-buffer "*src-empty*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*src-empty*" ("q1" "p1" done nil))))
      (should-error (agent-shell-queue-fork-session (current-buffer) nil)
                    :type 'user-error)
      (when (get-buffer "*no-items-new*") (kill-buffer "*no-items-new*")))))

(ert-deftest agent-shell-queue/fork-session-resumes-source-on-error ()
  "Source session is resumed (removed from paused) if session creation fails."
  (agent-shell-queue-test/isolate-no-sub
    (cl-letf (((symbol-function 'agent-shell-queue--fork-create-session)
               (lambda (_src _mode _dir) nil))  ; returns nil → triggers user-error
              ((symbol-function 'sit-for) #'ignore))
      (with-temp-buffer
        (rename-buffer "*src-resume*" t)
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (agent-shell-queue-test/populate
               '("*src-resume*" ("q1" "p1" active nil))))
        (ignore-errors
          (agent-shell-queue-fork-session (current-buffer) nil))
        ;; unwind-protect removes from paused list after error
        (should (not (member "*src-resume*"
                             (agent-shell-queue-queue-session-paused
                              agent-shell-queue--queue))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-release-pending-fork

(ert-deftest agent-shell-queue/release-pending-fork-activates-items ()
  "pending-fork items become active after release."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*release-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*release-session*"
              ("q1" "p1" pending-fork nil)
              ("q2" "p2" pending-fork nil)
              ("q3" "p3" active nil))))
      ;; Pre-pause the session so resume has something to clear
      (cl-pushnew "*release-session*"
                  (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  :test #'equal)
      (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore)
                ((symbol-function 'agent-shell-queue-session-resume) #'ignore))
        (agent-shell-queue-release-pending-fork (current-buffer)))
      (let ((items (cdr (assoc "*release-session*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (seq-every-p (lambda (it)
                               (eq 'active (agent-shell-queue-item-status it)))
                       items))))))

(ert-deftest agent-shell-queue/release-pending-fork-no-op-when-none ()
  "Releasing when no pending-fork items sends no-resume and reports 0."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*release-none*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*release-none*" ("q1" "p1" active nil))))
      (let (resumed)
        (cl-letf (((symbol-function 'agent-shell-queue-session-resume)
                   (lambda (_) (setq resumed t)))
                  ((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
          (agent-shell-queue-release-pending-fork (current-buffer)))
        ;; resume should not be called since nothing was pending-fork
        (should (not resumed))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-insert-at

(ert-deftest agent-shell-queue/fork-insert-at-beginning ()
  "Inserting at idx 0 places item before all others."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "new" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins*" new-item 0)
      (let ((items (cdr (assoc "*ins*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (car items))))))))

(ert-deftest agent-shell-queue/fork-insert-at-middle ()
  "Inserting at idx 1 places item between first and second."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins-mid*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "mid" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins-mid*" new-item 1)
      (let ((items (cdr (assoc "*ins-mid*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (nth 1 items))))
        (should (equal "q2" (agent-shell-queue-item-id (nth 2 items))))))))

(ert-deftest agent-shell-queue/fork-insert-at-nil-appends ()
  "Inserting with nil idx appends to end."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("*ins-end*" ("q1" "p1" active nil) ("q2" "p2" active nil))))
    (let ((new-item (agent-shell-queue--make-item "end" nil 'emacs)))
      (agent-shell-queue--fork-insert-at "*ins-end*" new-item nil)
      (let ((items (cdr (assoc "*ins-end*"
                               (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 3 (length items)))
        (should (equal (agent-shell-queue-item-id new-item)
                       (agent-shell-queue-item-id (nth 2 items))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-insert-fork-before / -after

(ert-deftest agent-shell-queue/insert-fork-before-inserts-emacs-item ()
  "insert-fork-before adds an emacs-kind item before the target."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*ibefore-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*ibefore-session*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-before
                        (current-buffer) "q2" nil)))
        (should (eq 'emacs (agent-shell-queue-item-kind fork-item)))
        (let ((items (cdr (assoc "*ibefore-session*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 3 (length items)))
          (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
          ;; fork item lands at position 1 (before q2)
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items))))
          (should (equal "q2" (agent-shell-queue-item-id (nth 2 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-after-inserts-emacs-item ()
  "insert-fork-after adds an emacs-kind item after the target."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*iafter-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*iafter-session*"
              ("q1" "p1" active nil)
              ("q2" "p2" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-after
                        (current-buffer) "q1" nil)))
        (should (eq 'emacs (agent-shell-queue-item-kind fork-item)))
        (let ((items (cdr (assoc "*iafter-session*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 3 (length items)))
          (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
          ;; fork item at position 1 (after q1)
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items))))
          (should (equal "q2" (agent-shell-queue-item-id (nth 2 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-before-nil-appends ()
  "insert-fork-before with nil item-id appends to end."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*ib-nil*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*ib-nil*" ("q1" "p1" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-before (current-buffer) nil nil)))
        (let ((items (cdr (assoc "*ib-nil*"
                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
          (should (= 2 (length items)))
          (should (equal (agent-shell-queue-item-id fork-item)
                         (agent-shell-queue-item-id (nth 1 items)))))))))

(ert-deftest agent-shell-queue/insert-fork-form-contains-buf-name ()
  "The emacs form in the inserted item references the source buffer name."
  (agent-shell-queue-test/isolate-no-sub
    (with-temp-buffer
      (rename-buffer "*iform-session*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*iform-session*" ("q1" "p1" active nil))))
      (let ((fork-item (agent-shell-queue-insert-fork-after
                        (current-buffer) "q1" '(:fork-mode fork))))
        (should (string-match-p "iform-session"
                                (agent-shell-queue-item-args fork-item)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-session-from-running-emacs

(ert-deftest agent-shell-queue/fork-from-running-emacs-forks-after-running ()
  "Finds the running item and forks starting from the item after it."
  (agent-shell-queue-test/with-fork-stubs "*from-running-new*"
    (with-temp-buffer
      (rename-buffer "*from-running-src*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*from-running-src*"
              ("q1" "p1" done nil)
              ("q2" "p2" running nil)   ; currently running emacs item
              ("q3" "p3" active nil)
              ("q4" "p4" active nil))))
      (agent-shell-queue--fork-session-from-running-emacs "*from-running-src*" nil)
      ;; q3 and q4 should be moved to new session
      (let ((new-items (cdr (assoc "*from-running-new*"
                                   (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length new-items)))
        (should (member "q3" (seq-map #'agent-shell-queue-item-id new-items)))
        (should (member "q4" (seq-map #'agent-shell-queue-item-id new-items))))
      (kill-buffer "*from-running-new*"))))

(ert-deftest agent-shell-queue/fork-from-running-emacs-no-items-after ()
  "Returns nil and does nothing when no items follow the running one."
  (agent-shell-queue-test/with-fork-stubs "*from-running-empty-new*"
    (with-temp-buffer
      (rename-buffer "*from-running-empty*" t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("*from-running-empty*"
              ("q1" "p1" running nil))))  ; running, nothing after it
      ;; Should not error; fork-session will error with user-error which we ignore
      (ignore-errors
        (agent-shell-queue--fork-session-from-running-emacs "*from-running-empty*" nil))
      (when (get-buffer "*from-running-empty-new*")
        (kill-buffer "*from-running-empty-new*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pending-fork status — display

(ert-deftest agent-shell-queue/status-string-pending-fork ()
  "Items with pending-fork status display as \"pending-fork\"."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-pf" "p" 'pending-fork nil)))
      (should (equal "pending-fork" (agent-shell-queue--status-string item))))))

(ert-deftest agent-shell-queue/item-display-pending-fork-face ()
  "pending-fork items get the pending-fork face."
  (agent-shell-queue-test/isolate
    (let* ((item (agent-shell-queue-test/make-item "q-pf2" "p" 'pending-fork nil))
           (display (agent-shell-queue--item-display item nil)))
      (should (equal "pending-fork" (car display)))
      (should (eq 'agent-shell-queue-pending-fork-face (cdr display))))))

(ert-deftest agent-shell-queue/pending-fork-not-dispatched ()
  "Items with pending-fork status are not eligible for dispatch."
  (agent-shell-queue-test/isolate
    (let ((items (list (agent-shell-queue-test/make-item "q-pf3" "p" 'pending-fork nil)
                       (agent-shell-queue-test/make-item "q-act" "p" 'active nil))))
      (let ((next (agent-shell-queue--next-dispatchable-item items)))
        (should (equal "q-act" (agent-shell-queue-item-id next)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-create-worktree

(ert-deftest agent-shell-queue/fork-create-worktree-no-git-repo ()
  "Returns nil when not in a git repo."
  (agent-shell-queue-test/isolate
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "")))  ; empty repo root
      (let ((result (agent-shell-queue--fork-create-worktree nil nil nil)))
        (should (null result))))))

(ert-deftest agent-shell-queue/fork-create-worktree-failure ()
  "Returns nil when git worktree add fails."
  (agent-shell-queue-test/isolate
    ;; Use a path that doesn't exist so file-exists-p returns nil naturally.
    ;; We cannot stub file-exists-p (C primitive) in Emacs 29 native compilation.
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "/some/repo"))
              ((symbol-function 'call-process)
               (lambda (&rest _) 128)))  ; non-zero exit
      (let ((result (agent-shell-queue--fork-create-worktree
                     nil "branch" "/tmp/asdf-no-such-ert-worktree-path-failure")))
        (should (null result))))))

(ert-deftest agent-shell-queue/fork-create-worktree-success ()
  "Returns the worktree path when git worktree add succeeds."
  (agent-shell-queue-test/isolate
    ;; Use a path that doesn't exist so file-exists-p returns nil naturally.
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "/some/repo"))
              ((symbol-function 'call-process)
               (lambda (&rest _) 0)))  ; zero exit = success
      (let ((result (agent-shell-queue--fork-create-worktree
                     nil "my-branch" "/tmp/asdf-no-such-ert-worktree-path-success")))
        (should (equal "/tmp/asdf-no-such-ert-worktree-path-success" result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--fork-elisp-form

(ert-deftest agent-shell-queue/fork-elisp-form-contains-buf-name ()
  "The generated form string contains the buffer name."
  (let ((form (agent-shell-queue--fork-elisp-form "*my-shell*" nil)))
    (should (string-match-p "my-shell" form))))

(ert-deftest agent-shell-queue/fork-elisp-form-contains-opts ()
  "The generated form string contains the opts plist."
  (let ((form (agent-shell-queue--fork-elisp-form "*shell*" '(:fork-mode fork))))
    (should (string-match-p "fork-mode" form))))

(ert-deftest agent-shell-queue/fork-elisp-form-is-readable ()
  "The generated form string is valid Emacs Lisp."
  (let* ((form (agent-shell-queue--fork-elisp-form "*readable-shell*" '(:capture-pending t)))
         (parsed (condition-case nil (read form) (error nil))))
    (should (listp parsed))
    (should (eq 'agent-shell-queue--fork-session-from-running-emacs (car parsed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-queue/menu-no-key-prefix-conflicts ()
  "No key in agent-shell-queue-dispatch is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-dispatch))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-queue/menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-queue-dispatch."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-dispatch))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(ert-deftest agent-shell-queue/item-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-queue-item-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-item-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-queue/item-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-queue-item-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-queue-item-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; capture buffer header (agent-shell-queue--open-capture)

(defmacro agent-shell-queue-test/with-capture (target-buf &rest body)
  "Run BODY with pop-to-buffer and capture-mode stubbed out.
Binds `capture-buf' to the buffer returned by `--open-capture'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
             ((symbol-function 'agent-shell-queue-capture-mode) #'ignore))
     (let ((capture-buf (agent-shell-queue--open-capture ,target-buf)))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p capture-buf)
           (kill-buffer capture-buf))))))

(ert-deftest agent-shell-queue/open-capture-header-shows-bucket-state-depth ()
  "Regression: header shows bucket/state/depth — not 'Inserting after item <id>'.
Applies to all capture paths, not just insert-after."
  (let ((target (generate-new-buffer "*test-capture-target*")))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (let ((item (agent-shell-queue-test/make-item "q01" "p" 'active)))
           (setf (agent-shell-queue-store-items agent-shell-queue--store)
                 (list (list (buffer-name target) item)))
           (cl-letf (((symbol-function 'agent-shell-queue--activity-state)
                      (lambda () "idle")))
             (agent-shell-queue-test/with-capture target
               (let ((header (with-current-buffer capture-buf header-line-format)))
                 (should (stringp header))
                 (should-not (string-match-p "Inserting after item" header))
                 (should (string-match-p (regexp-quote (buffer-name target)) header))
                 (should (string-match-p "idle" header))
                 (should (string-match-p "depth: 1" header)))))))
      (kill-buffer target))))

(ert-deftest agent-shell-queue/open-capture-header-shows-unassigned ()
  "Unassigned capture (nil target) shows the unassigned key in the header."
  (agent-shell-queue-test/isolate
   (cl-letf (((symbol-function 'agent-shell-queue--activity-state) (lambda () "idle")))
     (agent-shell-queue-test/with-capture nil
       (let ((header (with-current-buffer capture-buf header-line-format)))
         (should (stringp header))
         (should (string-match-p (regexp-quote agent-shell-queue--unassigned-key)
                                 header)))))))

(ert-deftest agent-shell-queue/open-capture-header-reflects-depth ()
  "Header depth reflects the actual item count in the target bucket."
  (let ((target (generate-new-buffer "*test-capture-depth*")))
    (unwind-protect
        (agent-shell-queue-test/isolate
         (let ((items (list (agent-shell-queue-test/make-item "q01" "p1" 'active)
                            (agent-shell-queue-test/make-item "q02" "p2" 'active)
                            (agent-shell-queue-test/make-item "q03" "p3" 'deferred))))
           (setf (agent-shell-queue-store-items agent-shell-queue--store)
                 (list (cons (buffer-name target) items)))
           (cl-letf (((symbol-function 'agent-shell-queue--activity-state)
                      (lambda () "idle")))
             (agent-shell-queue-test/with-capture target
               (let ((header (with-current-buffer capture-buf header-line-format)))
                 (should (string-match-p "depth: 3" header)))))))
      (kill-buffer target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue archive enable/path separation

(ert-deftest agent-shell-queue/archive-file-disabled-when-not-enabled ()
  "`agent-shell-queue--archive-file' returns nil when archive-enabled is nil."
  (let ((agent-shell-queue-archive-enabled nil))
    (should-not (agent-shell-queue--archive-file))))

(ert-deftest agent-shell-queue/archive-file-calls-function-when-enabled ()
  "`agent-shell-queue--archive-file' calls the file function when enabled."
  (let ((agent-shell-queue-archive-enabled t)
        (agent-shell-queue-archive-file-function (lambda () "/tmp/test-archive.jsonl")))
    (should (equal "/tmp/test-archive.jsonl" (agent-shell-queue--archive-file)))))

(ert-deftest agent-shell-queue/default-archive-file-returns-string ()
  "The default archive file function returns a non-empty string path."
  (let ((result (agent-shell-queue--default-archive-file)))
    (should (stringp result))
    (should (string-match-p "\\.jsonl\\'" result))))

(ert-deftest agent-shell-queue/write-archive-no-op-when-disabled ()
  "`--write-archive' does nothing when archiving is disabled."
  (let ((agent-shell-queue-archive-enabled nil)
        (wrote nil))
    (cl-letf (((symbol-function 'write-region)
               (lambda (&rest _) (setq wrote t))))
      (let ((item (agent-shell-queue-test/make-item "q01" "p" 'done)))
        (agent-shell-queue--write-archive "*buf*" item))
      (should-not wrote))))

(ert-deftest agent-shell-queue/write-archive-writes-when-enabled ()
  "`--write-archive' writes a record when archiving is enabled."
  (let* ((agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () "/tmp/test-archive.jsonl"))
         (wrote nil))
    (cl-letf (((symbol-function 'make-directory) #'ignore)
              ((symbol-function 'lock-file) #'ignore)
              ((symbol-function 'unlock-file) #'ignore)
              ((symbol-function 'write-region)
               (lambda (&rest _) (setq wrote t))))
      (let ((item (agent-shell-queue-test/make-item "q01" "prompt" 'done)))
        (agent-shell-queue--write-archive "*buf*" item))
      (should wrote))))

(ert-deftest agent-shell-queue/buffer-archive-errors-when-disabled ()
  "`agent-shell-queue-buffer-archive' signals user-error when disabled."
  (let ((agent-shell-queue-archive-enabled nil))
    (should-error (agent-shell-queue-buffer-archive) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--active-item-count

(ert-deftest agent-shell-queue/active-item-count-excludes-done ()
  "Depth count must not include items with status `done'."
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'active)
                     (agent-shell-queue-test/make-item "q02" "p" 'done)
                     (agent-shell-queue-test/make-item "q03" "p" 'deferred)
                     (agent-shell-queue-test/make-item "q04" "p" 'done))))
    (should (= 2 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-all-done ()
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'done)
                     (agent-shell-queue-test/make-item "q02" "p" 'done))))
    (should (= 0 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-none-done ()
  (let ((items (list (agent-shell-queue-test/make-item "q01" "p" 'active)
                     (agent-shell-queue-test/make-item "q02" "p" 'running))))
    (should (= 2 (agent-shell-queue--active-item-count items)))))

(ert-deftest agent-shell-queue/active-item-count-empty ()
  (should (= 0 (agent-shell-queue--active-item-count nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outcome field

(ert-deftest agent-shell-queue/outcome-nil-for-fresh-item ()
  "A newly created item has no outcome."
  (let ((item (agent-shell-queue-test/make-item "q01" "p" 'active)))
    (should (null (agent-shell-queue-item-outcome item)))))

(ert-deftest agent-shell-queue/outcome-success-after-mark-running-done ()
  "outcome is set to `success' when --mark-running-done completes an item."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-done "buf1")
    (should (eq 'success
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-interrupted-after-mark-running-incomplete ()
  "outcome is set to `interrupted' when --mark-running-incomplete fires."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (agent-shell-queue--mark-running-incomplete "buf1")
    (should (eq 'interrupted
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-manual-after-mark-done ()
  "outcome is set to `manual' when agent-shell-queue-mark-done is called."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
    (agent-shell-queue-mark-done "q-1")
    (should (eq 'manual
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-canceled-after-buffer-abort ()
  "outcome is set to `canceled' when buffer-abort is called."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "p" running nil))))
    (cl-letf (((symbol-function 'agent-shell-interrupt) #'ignore))
      (let ((item (cadar (agent-shell-queue-store-items agent-shell-queue--store))))
        (setf (agent-shell-queue-item-status item) 'aborted)
        (setf (agent-shell-queue-item-outcome item) 'canceled)))
    (should (eq 'canceled
                (agent-shell-queue-item-outcome
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/outcome-plist-round-trip ()
  "outcome survives a plist serialise/deserialise round-trip for all values."
  (dolist (val '(success canceled interrupted manual))
    (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'done))
           (_ (setf (agent-shell-queue-item-outcome item) val))
           (restored (agent-shell-queue-item-from-plist
                      (agent-shell-queue-item-to-plist item))))
      (should (eq val (agent-shell-queue-item-outcome restored))))))

(ert-deftest agent-shell-queue/outcome-plist-round-trip-nil ()
  "A nil outcome survives a plist round-trip as nil."
  (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'active))
         (restored (agent-shell-queue-item-from-plist
                    (agent-shell-queue-item-to-plist item))))
    (should (null (agent-shell-queue-item-outcome restored)))))

(ert-deftest agent-shell-queue/outcome-json-round-trip ()
  "outcome survives a JSON serialise/deserialise round-trip."
  (dolist (val '(success canceled interrupted manual))
    (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'done))
           (_ (setf (agent-shell-queue-item-outcome item) val))
           (json-obj (agent-shell-queue--item-to-json item))
           (restored (agent-shell-queue--item-from-json json-obj)))
      (should (eq val (agent-shell-queue-item-outcome restored))))))

(ert-deftest agent-shell-queue/outcome-json-round-trip-nil ()
  "A nil outcome round-trips through JSON as nil (stored as :null)."
  (let* ((item (agent-shell-queue-test/make-item "q01" "p" 'active))
         (json-obj (agent-shell-queue--item-to-json item))
         (restored (agent-shell-queue--item-from-json json-obj)))
    (should (null (agent-shell-queue-item-outcome restored)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write-archive — outcome field

(ert-deftest agent-shell-queue/write-archive-outcome-nil-stored-as-null ()
  "A nil outcome is serialised as JSON null in the archive record."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-arch-outcome"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (agent-shell-queue-instance-name "ti"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'lock-file) #'ignore)
                    ((symbol-function 'unlock-file) #'ignore))
            (agent-shell-queue--write-archive
             "buf" (agent-shell-queue-test/make-item "q01" "p" 'done)))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist
                                            :null-object nil)))
            (should-not (plist-get parsed :outcome))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest agent-shell-queue/write-archive-outcome-symbol-stored-as-string ()
  "A symbol outcome is serialised as its name string in the archive record."
  (skip-unless (fboundp 'json-serialize))
  (let* ((tmp (make-temp-file "asq-arch-outcome-sym"))
         (agent-shell-queue-archive-enabled t)
         (agent-shell-queue-archive-file-function (lambda () tmp))
         (agent-shell-queue-instance-name "ti"))
    (unwind-protect
        (let ((item (agent-shell-queue-test/make-item "q01" "p" 'done)))
          (setf (agent-shell-queue-item-outcome item) 'success)
          (cl-letf (((symbol-function 'lock-file) #'ignore)
                    ((symbol-function 'unlock-file) #'ignore))
            (agent-shell-queue--write-archive "buf" item))
          (let* ((content (with-temp-buffer
                            (insert-file-contents tmp)
                            (buffer-string)))
                 (parsed (json-parse-string (string-trim content)
                                            :object-type 'plist)))
            (should (equal "success" (plist-get parsed :outcome)))))
      (ignore-errors (delete-file tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-archive-done-n

(ert-deftest agent-shell-queue/archive-done-n-errors-when-disabled ()
  "`archive-done-n' signals user-error when archiving is disabled."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-archive-enabled nil))
      (should-error (agent-shell-queue-archive-done-n 5) :type 'user-error))))

(ert-deftest agent-shell-queue/archive-done-n-errors-when-no-done-items ()
  "`archive-done-n' signals user-error when no done items exist."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (should-error (agent-shell-queue-archive-done-n 5) :type 'user-error)))))

(ert-deftest agent-shell-queue/archive-done-n-archives-oldest-items ()
  "`archive-done-n' archives the N oldest done items by created time."
  (agent-shell-queue-test/isolate-no-sub
    (let ((archived nil)
          (agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (let ((item1 (agent-shell-queue-test/make-item "q-1" "oldest" 'done))
            (item2 (agent-shell-queue-test/make-item "q-2" "middle" 'done))
            (item3 (agent-shell-queue-test/make-item "q-3" "newest" 'done)))
        (setf (agent-shell-queue-item-created item1) 1000.0)
        (setf (agent-shell-queue-item-created item2) 2000.0)
        (setf (agent-shell-queue-item-created item3) 3000.0)
        (setf (agent-shell-queue-store-items agent-shell-queue--store)
              (list (cons "buf1" (list item1 item2 item3)))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive)
                 (lambda (_buf item) (push (agent-shell-queue-item-id item) archived))))
        (agent-shell-queue-archive-done-n 2))
      (should (equal '("q-1" "q-2") (nreverse archived)))
      (let ((remaining (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
        (should (= 1 (length remaining)))
        (should (equal "q-3" (agent-shell-queue-item-id (car remaining))))))))

(ert-deftest agent-shell-queue/archive-done-n-leaves-active-items ()
  "`archive-done-n' does not touch active items."
  (agent-shell-queue-test/isolate-no-sub
    (let ((archived-count 0)
          (agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("buf1" ("q-1" "done-item" done nil) ("q-2" "active-item" active nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive)
                 (lambda (&rest _) (cl-incf archived-count))))
        (agent-shell-queue-archive-done-n 5))
      (should (= 1 archived-count))
      (let ((remaining (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
        (should (= 1 (length remaining)))
        (should (eq 'active (agent-shell-queue-item-status (car remaining))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-archive-done-all

(ert-deftest agent-shell-queue/archive-done-all-errors-when-disabled ()
  "`archive-done-all' signals user-error when archiving is disabled."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-archive-enabled nil))
      (should-error (agent-shell-queue-archive-done-all) :type 'user-error))))

(ert-deftest agent-shell-queue/archive-done-all-errors-when-no-done-items ()
  "`archive-done-all' signals user-error when no done items exist."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate '("buf1" ("q-1" "p" active nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (should-error (agent-shell-queue-archive-done-all) :type 'user-error)))))

(ert-deftest agent-shell-queue/archive-done-all-archives-all-done-items ()
  "`archive-done-all' archives every done item across all buckets."
  (agent-shell-queue-test/isolate-no-sub
    (let ((archived nil)
          (agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("buf1" ("q-1" "p1" done nil) ("q-2" "p2" done nil) ("q-3" "p3" active nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive)
                 (lambda (_buf item) (push (agent-shell-queue-item-id item) archived))))
        (agent-shell-queue-archive-done-all))
      (should (= 2 (length archived)))
      (let ((remaining (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
        (should (= 1 (length remaining)))
        (should (eq 'active (agent-shell-queue-item-status (car remaining))))))))

(ert-deftest agent-shell-queue/archive-done-all-leaves-active-items ()
  "`archive-done-all' never removes active, deferred, or running items."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-archive-enabled t)
          (agent-shell-queue-archive-file-function (lambda () "/tmp/x")))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (agent-shell-queue-test/populate
             '("buf1" ("q-1" "a" active nil) ("q-2" "d" deferred nil))))
      (cl-letf (((symbol-function 'agent-shell-queue--write-archive) #'ignore))
        (should-error (agent-shell-queue-archive-done-all) :type 'user-error))
      (should (= 2 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-toggle-archive

(ert-deftest agent-shell-queue/toggle-archive-enables-when-disabled ()
  "`toggle-archive' sets `agent-shell-queue-archive-enabled' to t when nil."
  (let ((agent-shell-queue-archive-enabled nil))
    (agent-shell-queue-toggle-archive)
    (should agent-shell-queue-archive-enabled)))

(ert-deftest agent-shell-queue/toggle-archive-disables-when-enabled ()
  "`toggle-archive' sets `agent-shell-queue-archive-enabled' to nil when t."
  (let ((agent-shell-queue-archive-enabled t))
    (agent-shell-queue-toggle-archive)
    (should-not agent-shell-queue-archive-enabled)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--serialize-single-item

(ert-deftest agent-shell-queue/serialize-single-item-plist ()
  "plist serialization wraps item and target in a :buffer/:item plist."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-item--make
                  :id "test-id" :args "hello" :status 'active
                  :kind 'prompt :created 1000.0)))
      (let ((out (agent-shell-queue--serialize-single-item item "buf1" 'plist)))
        (should (stringp out))
        (let ((parsed (read out)))
          (should (equal (plist-get parsed :buffer) "buf1"))
          (should (equal (plist-get (plist-get parsed :item) :args) "hello"))
          (should (equal (plist-get (plist-get parsed :item) :id) "test-id")))))))

(ert-deftest agent-shell-queue/serialize-single-item-json ()
  "JSON serialization includes buffer and item fields."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-item--make
                  :id "test-id" :args "hello" :status 'active
                  :kind 'prompt :created 1000.0)))
      (let ((out (agent-shell-queue--serialize-single-item item "buf1" 'json)))
        (should (stringp out))
        (let ((parsed (json-parse-string out :object-type 'plist :null-object nil)))
          (should (equal (plist-get parsed :buffer) "buf1"))
          (should (equal (plist-get (plist-get parsed :item) :args) "hello")))))))

(ert-deftest agent-shell-queue/serialize-single-item-unknown-format-errors ()
  "`serialize-single-item' signals user-error for unrecognized format."
  (agent-shell-queue-test/isolate-no-sub
    (let ((item (agent-shell-queue-item--make
                 :id "x" :args "p" :status 'active
                 :kind 'prompt :created 1000.0)))
      (should-error
       (agent-shell-queue--serialize-single-item item "buf" 'nosuchformat)
       :type 'user-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--inspect-format-display

(ert-deftest agent-shell-queue/inspect-format-display-marks-on-disk ()
  "The on-disk format entry carries the [on-disk] annotation."
  (agent-shell-queue-test/isolate-no-sub
    (let ((agent-shell-queue-serialization-format 'json))
      (setf (agent-shell-queue-store-format agent-shell-queue--store) 'json)
      (let ((choices (agent-shell-queue--inspect-format-display)))
        (should (equal (cdr (assoc "json" choices)) "[on-disk]"))
        (should (equal (cdr (assoc "plist" choices)) ""))))))

(ert-deftest agent-shell-queue/inspect-format-display-excludes-yaml-when-unavailable ()
  "yaml is excluded from choices when yaml-encode is not available."
  (agent-shell-queue-test/isolate-no-sub
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (if (eq sym 'yaml-encode) nil (fboundp sym)))))
      (let ((choices (agent-shell-queue--inspect-format-display)))
        (should-not (assoc "yaml" choices))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Detached status

(ert-deftest agent-shell-queue/item-detached-p-dead-buffer ()
  "Returns non-nil when the named buffer does not exist."
  (should (agent-shell-queue--item-detached-p "this-buffer-does-not-exist-xyz")))

(ert-deftest agent-shell-queue/item-detached-p-nil ()
  "Returns nil for a nil bucket name."
  (should-not (agent-shell-queue--item-detached-p nil)))

(ert-deftest agent-shell-queue/item-detached-p-unassigned ()
  "Returns nil for the unassigned bucket key."
  (should-not (agent-shell-queue--item-detached-p agent-shell-queue--unassigned-key)))

(ert-deftest agent-shell-queue/item-detached-p-live-buffer ()
  "Returns nil when the named buffer is live."
  (let ((buf (get-buffer-create " *asq-detached-test*")))
    (unwind-protect
        (should-not (agent-shell-queue--item-detached-p (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/status-string-detached-active ()
  "An active item in a dead bucket shows as 'detached'."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (should (equal "detached"
                     (agent-shell-queue--status-string item "no-such-buffer-xyz"))))))

(ert-deftest agent-shell-queue/status-string-detached-deferred-still-held ()
  "A blocked.skip item in a dead bucket shows as 'blocked.skip', not 'detached'."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'blocked.skip nil)))
      (should (equal "blocked.skip"
                     (agent-shell-queue--status-string item "no-such-buffer-xyz"))))))

(ert-deftest agent-shell-queue/status-string-detached-blocked-takes-priority ()
  "When a bucket is both session-paused and dead, blocked.runner wins."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "p" 'active nil)))
      (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue) '("dead-paused-buf"))
      (should (equal "blocked.runner"
                     (agent-shell-queue--status-string item "dead-paused-buf"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Item-view action table

(ert-deftest agent-shell-queue/item-view-action-table-non-empty ()
  "The action table has entries with required keys."
  (should (> (length agent-shell-queue--item-view-action-table) 0))
  (dolist (entry agent-shell-queue--item-view-action-table)
    (should (plist-get entry :key))
    (should (plist-get entry :label))
    (should (plist-get entry :cmd))))

(ert-deftest agent-shell-queue/item-view-action-table-keys-unique ()
  "No two entries share the same key binding."
  (let ((keys (seq-map (lambda (a) (plist-get a :key))
                       agent-shell-queue--item-view-action-table)))
    (should (= (length keys) (length (seq-uniq keys))))))

(ert-deftest agent-shell-queue/item-view-action-table-groups-valid ()
  "Every :group value is either nil or a member of the groups list."
  (dolist (entry agent-shell-queue--item-view-action-table)
    (let ((group (plist-get entry :group)))
      (when group
        (should (member group agent-shell-queue--item-view-action-groups))))))

(ert-deftest agent-shell-queue/item-view-build-map-has-actions ()
  "The built keymap contains all keys from the action table."
  (let ((m (agent-shell-queue--item-view-build-map)))
    (dolist (entry agent-shell-queue--item-view-action-table)
      (should (lookup-key m (kbd (plist-get entry :key)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--has-running-item-p

(ert-deftest agent-shell-queue/has-running-item-p-true ()
  "Returns non-nil when a running item exists for the buffer."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (should (agent-shell-queue--has-running-item-p "buf1")))))

(ert-deftest agent-shell-queue/has-running-item-p-false ()
  "Returns nil when no running items exist for the buffer."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (should-not (agent-shell-queue--has-running-item-p "buf1"))))

(ert-deftest agent-shell-queue/has-running-item-p-wrong-buf ()
  "Returns nil when the running item belongs to a different buffer."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (should-not (agent-shell-queue--has-running-item-p "buf2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--copy-item-to-end

(ert-deftest agent-shell-queue/copy-item-to-end-appends ()
  "A copy is appended after the existing items with a fresh ID."
  (agent-shell-queue-test/isolate
    (let* ((orig (agent-shell-queue-test/make-item "q-1" "hello" 'running nil))
           (extra (agent-shell-queue-test/make-item "q-2" "world" 'active nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" orig extra)))
      (let ((new-id (agent-shell-queue--copy-item-to-end "buf1" orig)))
        (let* ((items (cdr (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store))))
               (copy (car (last items))))
          (should (= 3 (length items)))
          (should (equal new-id (agent-shell-queue-item-id copy)))
          (should (equal "hello" (agent-shell-queue-item-args copy)))
          (should (eq 'active (agent-shell-queue-item-status copy)))
          (should-not (equal "q-1" new-id)))))))

(ert-deftest agent-shell-queue/copy-item-to-end-creates-bucket ()
  "Creates a new bucket when the buffer has no existing queue."
  (agent-shell-queue-test/isolate
    (let ((orig (agent-shell-queue-test/make-item "q-1" "hi" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
      (agent-shell-queue--copy-item-to-end "fresh-buf" orig)
      (let ((bucket (assoc "fresh-buf" (agent-shell-queue-store-items agent-shell-queue--store))))
        (should bucket)
        (should (= 1 (length (cdr bucket))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--send-now — smart dispatch

(ert-deftest agent-shell-queue/send-now-dispatches-when-no-running ()
  "Calls send-item when no running task exists for the buffer."
  (agent-shell-queue-test/isolate
    (let ((sent nil)
          (buf (get-buffer-create " *asq-sn-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "hello" 'active nil))))
            (cl-letf (((symbol-function 'agent-shell-queue-send-item)
                       (lambda (id) (setq sent id)))
                      ((symbol-function 'buffer-live-p) (lambda (_) t)))
              (agent-shell-queue--send-now "q-1")
              (should (equal sent "q-1"))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/send-now-running-item-enqueues-copy ()
  "Appends a copy when the item itself is running and queue is busy."
  (agent-shell-queue-test/isolate
    (let* ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil))
           (copied nil))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (cl-letf (((symbol-function 'agent-shell-queue--copy-item-to-end)
                 (lambda (buf-name _item) (setq copied buf-name) "new-id")))
        (agent-shell-queue--send-now "q-1")
        (should (equal copied "buf1"))))))

(ert-deftest agent-shell-queue/send-now-active-item-errors-when-running ()
  "Signals user-error when trying to dispatch an active item while queue runs."
  (agent-shell-queue-test/isolate
    (let* ((running (agent-shell-queue-test/make-item "q-run" "go" 'running nil))
           (active (agent-shell-queue-test/make-item "q-act" "wait" 'active nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" running active)))
      (should-error (agent-shell-queue--send-now "q-act") :type 'user-error))))

(ert-deftest agent-shell-queue/send-now-deferred-activates-when-running ()
  "Unblocks a blocked.skip item (sets active) when queue is busy."
  (agent-shell-queue-test/isolate
    (let* ((running (agent-shell-queue-test/make-item "q-run" "go" 'running nil))
           (blocked (agent-shell-queue-test/make-item "q-def" "later" 'blocked.skip nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" running blocked)))
      (cl-letf (((symbol-function 'agent-shell-queue-send-item) #'ignore))
        (agent-shell-queue--send-now "q-def")
        (should (eq 'active (agent-shell-queue-item-status blocked)))))))

(ert-deftest agent-shell-queue/send-now-done-item-reenqueues-when-running ()
  "Calls reenqueue when a done item is dispatched while queue is busy and user confirms."
  (agent-shell-queue-test/isolate
    (let* ((running (agent-shell-queue-test/make-item "q-run" "go" 'running nil))
           (done (agent-shell-queue-test/make-item "q-done" "finished" 'done nil))
           (reenqueued nil))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" running done)))
      (cl-letf (((symbol-function 'agent-shell-queue-reenqueue)
                 (lambda (id) (setq reenqueued id)))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
        (agent-shell-queue--send-now "q-done")
        (should (equal reenqueued "q-done"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-untrack-running

(ert-deftest agent-shell-queue/untrack-running-removes-item ()
  "Removes the running item from the store without aborting."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue-untrack-running "q-1")
      (should-not (agent-shell-queue--item-by-id "q-1")))))

(ert-deftest agent-shell-queue/untrack-running-errors-if-not-running ()
  "Signals user-error when item is not running."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (should-error (agent-shell-queue-untrack-running "q-1") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-enqueue-running-copy

(ert-deftest agent-shell-queue/enqueue-running-copy-appends ()
  "Appends an active copy of the running item to the queue."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue-enqueue-running-copy "q-1")
      (let ((items (cdr (assoc "buf1" (agent-shell-queue-store-items agent-shell-queue--store)))))
        (should (= 2 (length items)))
        (should (eq 'running (agent-shell-queue-item-status (nth 0 items))))
        (should (eq 'active (agent-shell-queue-item-status (nth 1 items))))
        (should (equal "hello" (agent-shell-queue-item-args (nth 1 items))))
        (should-not (equal "q-1" (agent-shell-queue-item-id (nth 1 items))))))))

(ert-deftest agent-shell-queue/enqueue-running-copy-errors-if-not-running ()
  "Signals user-error when item is not running."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf1" ("q-1" "hello" active nil))))
    (should-error (agent-shell-queue-enqueue-running-copy "q-1") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--on-queue-buffer-kill

(ert-deftest agent-shell-queue/on-queue-buffer-kill-aborts-running ()
  "Running items are marked aborted when the queue buffer is killed."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'running nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue--on-queue-buffer-kill)
      (should (eq 'aborted (agent-shell-queue-item-status item)))
      (should (eq 'canceled (agent-shell-queue-item-outcome item))))))

(ert-deftest agent-shell-queue/on-queue-buffer-kill-defers-active ()
  "Active items become blocked.skip when the queue buffer is killed."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "hello" 'active nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (agent-shell-queue--on-queue-buffer-kill)
      (should (eq 'blocked.skip (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue/on-queue-buffer-kill-leaves-done-intact ()
  "Done and blocked items are untouched when the queue buffer is killed."
  (agent-shell-queue-test/isolate
    (let ((done (agent-shell-queue-test/make-item "q-1" "d" 'done nil))
          (skipped (agent-shell-queue-test/make-item "q-2" "p" 'blocked.skip nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" done skipped)))
      (agent-shell-queue--on-queue-buffer-kill)
      (should (eq 'done (agent-shell-queue-item-status done)))
      (should (eq 'blocked.skip (agent-shell-queue-item-status skipped))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-recover-stuck-shell

(ert-deftest agent-shell-queue/recover-marks-running-item-aborted ()
  "recover-stuck-shell marks any running item as aborted with outcome interrupted."
  (agent-shell-queue-test/isolate
    (let ((item (agent-shell-queue-test/make-item "q-1" "stuck" 'running nil))
          (buf (get-buffer-create " *asq-recover-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) item)))
            (cl-letf (((symbol-function 'agent-shell-interrupt) #'ignore)
                      ((symbol-function 'agent-shell-queue--poll-for-idle-and-resume) #'ignore))
              (agent-shell-queue-recover-stuck-shell buf))
            (should (eq 'aborted (agent-shell-queue-item-status item)))
            (should (eq 'interrupted (agent-shell-queue-item-outcome item)))
            (should (agent-shell-queue-item-completed item)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/recover-leaves-non-running-items-unchanged ()
  "recover-stuck-shell does not touch active or done items."
  (agent-shell-queue-test/isolate
    (let ((active (agent-shell-queue-test/make-item "q-1" "next" 'active nil))
          (done (agent-shell-queue-test/make-item "q-2" "old" 'done nil))
          (buf (get-buffer-create " *asq-recover-unchanged-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) active done)))
            (cl-letf (((symbol-function 'agent-shell-interrupt) #'ignore)
                      ((symbol-function 'agent-shell-queue--poll-for-idle-and-resume) #'ignore))
              (agent-shell-queue-recover-stuck-shell buf))
            (should (eq 'active (agent-shell-queue-item-status active)))
            (should (eq 'done (agent-shell-queue-item-status done))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/recover-calls-agent-shell-interrupt ()
  "recover-stuck-shell calls agent-shell-interrupt in the target buffer."
  (agent-shell-queue-test/isolate
    (let ((interrupted-in nil)
          (buf (get-buffer-create " *asq-recover-interrupt-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf)
                              (agent-shell-queue-test/make-item "q-1" "p" 'running nil))))
            (cl-letf (((symbol-function 'agent-shell-interrupt)
                       (lambda () (setq interrupted-in (current-buffer))))
                      ((symbol-function 'agent-shell-queue--poll-for-idle-and-resume) #'ignore))
              (agent-shell-queue-recover-stuck-shell buf))
            (should (eq buf interrupted-in)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/poll-resumes-when-shell-idle ()
  "poll-for-idle-and-resume calls session-resume immediately when shell is not busy."
  (agent-shell-queue-test/isolate
    (let ((resumed nil)
          (buf (get-buffer-create " *asq-poll-idle-test*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-queue-session-resume)
                       (lambda (b) (setq resumed b))))
              (agent-shell-queue--poll-for-idle-and-resume buf 0))
            (should (eq buf resumed)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/poll-retries-when-shell-busy ()
  "poll-for-idle-and-resume schedules a retry timer when shell is still busy."
  (agent-shell-queue-test/isolate
    (let ((timer-scheduled nil)
          (buf (get-buffer-create " *asq-poll-retry-test*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t))
                      ((symbol-function 'run-with-timer)
                       (lambda (_delay _repeat fn &rest args)
                         (setq timer-scheduled (cons fn args)))))
              (agent-shell-queue--poll-for-idle-and-resume buf 0))
            (should timer-scheduled)
            (should (eq (car timer-scheduled) #'agent-shell-queue--poll-for-idle-and-resume))
            (should (eq (cadr timer-scheduled) buf))
            (should (= (caddr timer-scheduled) 1)))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/poll-gives-up-after-max-attempts ()
  "poll-for-idle-and-resume stops retrying after attempt 20."
  (agent-shell-queue-test/isolate
    (let ((resumed nil)
          (timer-scheduled nil)
          (buf (get-buffer-create " *asq-poll-timeout-test*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t))
                      ((symbol-function 'agent-shell-queue-session-resume)
                       (lambda (_b) (setq resumed t)))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) (setq timer-scheduled t))))
              (agent-shell-queue--poll-for-idle-and-resume buf 21))
            (should-not resumed)
            (should-not timer-scheduled))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/poll-abandons-when-buffer-dead ()
  "poll-for-idle-and-resume does not crash or resume when buffer is killed."
  (agent-shell-queue-test/isolate
    (let ((resumed nil)
          (buf (get-buffer-create " *asq-poll-dead-test*")))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'agent-shell-queue-session-resume)
                 (lambda (_b) (setq resumed t))))
        (agent-shell-queue--poll-for-idle-and-resume buf 0))
      (should-not resumed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interjection feature

(ert-deftest agent-shell-queue/interjection-item-struct-fields ()
  "Queue item struct has interjection-prompt and interjection-result fields."
  (let ((item (agent-shell-queue-item--make)))
    (should (null (agent-shell-queue-item-interjection-prompt item)))
    (should (null (agent-shell-queue-item-interjection-result item)))
    (setf (agent-shell-queue-item-interjection-prompt item) "hello")
    (setf (agent-shell-queue-item-interjection-result item) "world")
    (should (equal "hello" (agent-shell-queue-item-interjection-prompt item)))
    (should (equal "world" (agent-shell-queue-item-interjection-result item)))))

(ert-deftest agent-shell-queue/interjection-queue-struct-pending ()
  "Queue struct has interjection-pending field defaulting to nil."
  (let ((q (agent-shell-queue-queue--make)))
    (should (null (agent-shell-queue-queue-interjection-pending q)))
    (setf (agent-shell-queue-queue-interjection-pending q) t)
    (should (agent-shell-queue-queue-interjection-pending q))))

(ert-deftest agent-shell-queue/interjection-fields-not-serialized ()
  "Interjection fields are not included in item plist serialization."
  (let ((item (agent-shell-queue-item--make
               :id "q1" :args "prompt" :status 'done
               :interjection-prompt "user text"
               :interjection-result "agent reply")))
    (let ((plist (agent-shell-queue-item-to-plist item)))
      (should-not (plist-get plist :interjection-prompt))
      (should-not (plist-get plist :interjection-result)))))

(ert-deftest agent-shell-queue/item-done-hook-fires-on-llm-completion ()
  "item-done-hook is invoked when --mark-running-done marks a running item done."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-test/make-item "q01" "test" 'running))
           (fired nil)
           (agent-shell-queue-item-done-hook
            (list (lambda (bn it) (setq fired (list bn it))))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (cl-letf (((symbol-function 'agent-shell-queue--capture-response) #'ignore)
                ((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore))
        (agent-shell-queue--mark-running-done "buf1"))
      (should fired)
      (should (equal "buf1" (car fired)))
      (should (eq item (cadr fired))))))

(ert-deftest agent-shell-queue/item-done-hook-fires-on-manual-mark-done ()
  "item-done-hook is invoked when agent-shell-queue-mark-done is called."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-test/make-item "q01" "test" 'active))
           (fired nil)
           (agent-shell-queue-item-done-hook
            (list (lambda (bn it) (setq fired (list bn it))))))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (cl-letf (((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                ((symbol-function 'agent-shell-queue--assert-not-running) #'ignore)
                ((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
        (agent-shell-queue-mark-done "q01"))
      (should fired)
      (should (equal "buf1" (car fired))))))

(ert-deftest agent-shell-queue/item-done-hook-fires-for-pause-on-session-resume ()
  "item-done-hook fires for running pause/compact items when session-resume is called."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-item--make
                  :id "q01" :args "[PAUSE]" :status 'running :kind 'pause :created 1000.0))
           (fired nil)
           (agent-shell-queue-item-done-hook
            (list (lambda (bn it) (setq fired (list bn it)))))
           (buf (get-buffer-create " *asq-resume-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) item)))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  (list (buffer-name buf)))
            (cl-letf (((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                      ((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
              (agent-shell-queue-session-resume buf))
            (should fired)
            (should (equal (buffer-name buf) (car fired))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/interjecting-state-handled-by-mark-running-done ()
  "An interjecting item is completed by --mark-running-done on turn-complete."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-test/make-item "q01" "original" 'interjecting))
           (hook-fired nil)
           (agent-shell-queue-item-done-hook
            (list (lambda (bn it) (setq hook-fired (list bn it))))))
      (setf (agent-shell-queue-item-interjection-prompt item) "user question")
      (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) t)
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (cl-letf (((symbol-function 'agent-shell-queue--capture-response)
                 (lambda (_id _buf)
                   (setf (agent-shell-queue-item-response item) "agent answer")))
                ((symbol-function 'agent-shell-queue--append-done-log) #'ignore)
                ((symbol-function 'agent-shell-queue--alert-if-empty) #'ignore)
                ((symbol-function 'agent-shell-queue--session-unpause-name) #'ignore))
        (agent-shell-queue--mark-running-done "buf1"))
      (should (eq 'done (agent-shell-queue-item-status item)))
      (should (equal "agent answer" (agent-shell-queue-item-interjection-result item)))
      (should (null (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue)))
      (should hook-fired))))

(ert-deftest agent-shell-queue/interjection-readable-prompt-strips-header ()
  "agent-shell-queue--interjection-readable-prompt returns text after separator."
  (let* ((sep (make-string 60 ?─))
         (raw (concat "Interjecting: q01 — first line\nOriginal:\n  prompt\n"
                      sep "\n\nmy interjection text")))
    (should (equal "my interjection text"
                   (agent-shell-queue--interjection-readable-prompt raw)))))

(ert-deftest agent-shell-queue/interject-errors-without-running-item ()
  "agent-shell-queue-interject signals user-error when no running item exists."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (list (list "buf1" (agent-shell-queue-test/make-item "q01" "test" 'active))))
    (should-error (agent-shell-queue-interject) :type 'user-error)))

(ert-deftest agent-shell-queue/interject-errors-when-already-pending ()
  "agent-shell-queue-interject signals user-error when interjection-pending is set."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (list (list "buf1" (agent-shell-queue-test/make-item "q01" "test" 'running))))
    (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) t)
    (should-error (agent-shell-queue-interject) :type 'user-error)))

(ert-deftest agent-shell-queue/interject-sets-state-and-pending-flag ()
  "agent-shell-queue-interject transitions item to interjecting and sets pending flag."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((item (agent-shell-queue-test/make-item "q01" "test" 'running))
           (buf (get-buffer-create " *asq-interject-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq major-mode 'agent-shell-mode))
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) item)))
            (cl-letf (((symbol-function 'agent-shell-interrupt) #'ignore)
                      ((symbol-function 'agent-shell-queue--open-interjection-buffer) #'ignore))
              (agent-shell-queue-interject))
            (should (eq 'interjecting (agent-shell-queue-item-status item)))
            (should (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue)))
        (kill-buffer buf)))))

;;; Re-enqueue tests

(ert-deftest agent-shell-queue/reenqueue-item-struct-fields ()
  "Item struct has reenqueued-from and reenqueued-as fields."
  (let ((item (agent-shell-queue-item--make :id "q01" :args "test" :status 'active)))
    (should (null (agent-shell-queue-item-reenqueued-from item)))
    (should (null (agent-shell-queue-item-reenqueued-as item)))
    (setf (agent-shell-queue-item-reenqueued-from item) "q00")
    (setf (agent-shell-queue-item-reenqueued-as item) '("q02" "q03"))
    (should (equal "q00" (agent-shell-queue-item-reenqueued-from item)))
    (should (equal '("q02" "q03") (agent-shell-queue-item-reenqueued-as item)))))

(ert-deftest agent-shell-queue/reenqueue-sets-cross-references ()
  "agent-shell-queue-reenqueue sets reenqueued-from on new item and reenqueued-as on original."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((old-item (agent-shell-queue-test/make-item "qold" "repeat this" 'done))
           (buf (get-buffer-create " *asq-reenqueue-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'agent-shell-mode))
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) old-item)))
            (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
                      ((symbol-function 'alert) #'ignore))
              (agent-shell-queue-reenqueue "qold"))
            ;; reenqueued-as on the original should contain the new ID
            (let ((as-ids (agent-shell-queue-item-reenqueued-as old-item)))
              (should (= 1 (length as-ids)))
              (let* ((new-id (car as-ids))
                     (new-pair (agent-shell-queue--item-by-id new-id))
                     (new-item (cdr new-pair)))
                (should new-item)
                ;; new item points back to original
                (should (equal "qold" (agent-shell-queue-item-reenqueued-from new-item)))
                ;; new item is active, not done
                (should (eq 'active (agent-shell-queue-item-status new-item)))
                ;; original response is untouched
                (should (null (agent-shell-queue-item-response old-item))))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-does-not-overwrite-response ()
  "agent-shell-queue-reenqueue does not modify the original item's response."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((old-item (agent-shell-queue-test/make-item "qold2" "task" 'done))
           (buf (get-buffer-create " *asq-reenqueue-response-test*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-item-response old-item) "original response text")
            (with-current-buffer buf (setq major-mode 'agent-shell-mode))
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) old-item)))
            (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
                      ((symbol-function 'alert) #'ignore))
              (agent-shell-queue-reenqueue "qold2"))
            (should (equal "original response text" (agent-shell-queue-item-response old-item))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/reenqueue-multiple-times-accumulates-ids ()
  "Calling reenqueue twice on the same item accumulates both new IDs in reenqueued-as."
  (agent-shell-queue-test/isolate-no-sub
    (let* ((old-item (agent-shell-queue-test/make-item "qbase" "base task" 'done))
           (buf (get-buffer-create " *asq-reenqueue-multi-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'agent-shell-mode))
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf) old-item)))
            (cl-letf (((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
                      ((symbol-function 'alert) #'ignore))
              (agent-shell-queue-reenqueue "qbase")
              (agent-shell-queue-reenqueue "qbase"))
            (should (= 2 (length (agent-shell-queue-item-reenqueued-as old-item)))))
        (kill-buffer buf)))))

(ert-deftest agent-shell-queue/get-item-by-id-returns-pair ()
  "agent-shell-queue-get-item-by-id returns (BUF-NAME . ITEM) for a known ID."
  (agent-shell-queue-test/isolate-no-sub
    (let ((item (agent-shell-queue-test/make-item "q42" "find me" 'active)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf1" item)))
      (let ((result (agent-shell-queue-get-item-by-id "q42")))
        (should result)
        (should (equal "buf1" (car result)))
        (should (equal item (cdr result)))))))

(ert-deftest agent-shell-queue/get-item-by-id-returns-nil-for-unknown ()
  "agent-shell-queue-get-item-by-id returns nil for an unknown ID."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store) nil)
    (should (null (agent-shell-queue-get-item-by-id "q99")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Struct migration: stale agent-shell-queue-queue structs from savehist
;;
;; When a field is added to `agent-shell-queue-queue', structs persisted by
;; savehist under the previous layout have fewer vector slots.  Accessing a
;; new slot on an old vector signals `args-out-of-range'.
;; `agent-shell-queue--migrate-queue-struct' must detect and rebuild such
;; values before any accessor is called.

(defun agent-shell-queue-test/make-stale-queue-struct (&optional session-paused)
  "Return a `agent-shell-queue-queue' record with one fewer field than current
\(store, session-paused, editing-ids — missing the trailing interjection-pending
field).  Uses `record' so `agent-shell-queue-queue-p' recognises it, matching
what savehist restores when it reads a `#s(...)' printed by an older Emacs
session.  SESSION-PAUSED is placed at its slot."
  (record (type-of (agent-shell-queue-queue--make))
          'agent-shell-queue--store
          session-paused
          nil))

(ert-deftest agent-shell-queue/migrate-queue-struct-noop-when-current ()
  "Migration is a no-op when the struct already has the current layout."
  (let* ((fresh (agent-shell-queue-queue--make))
         (agent-shell-queue--queue fresh))
    (agent-shell-queue--migrate-queue-struct)
    (should (eq fresh agent-shell-queue--queue))))

(ert-deftest agent-shell-queue/migrate-queue-struct-rebuilds-stale ()
  "A stale struct (fewer fields than current) is replaced with a valid one."
  (let ((agent-shell-queue--queue (agent-shell-queue-test/make-stale-queue-struct)))
    (agent-shell-queue--migrate-queue-struct)
    (should (agent-shell-queue-queue-p agent-shell-queue--queue))
    (should (= (length agent-shell-queue--queue)
               (length (agent-shell-queue-queue--make))))))

(ert-deftest agent-shell-queue/migrate-queue-struct-preserves-session-paused ()
  "Migration preserves `session-paused' from a stale struct when the slot exists."
  (let ((agent-shell-queue--queue
         (agent-shell-queue-test/make-stale-queue-struct '("*foo*" "*bar*"))))
    (agent-shell-queue--migrate-queue-struct)
    (should (equal '("*foo*" "*bar*")
                   (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))))

(ert-deftest agent-shell-queue/predicates-safe-on-stale-struct ()
  "All queue predicates return nil (not signal) when the queue struct is stale."
  (let ((agent-shell-queue--queue (agent-shell-queue-test/make-stale-queue-struct))
        (agent-shell-queue--store
         (agent-shell-queue--make-store :items nil :format 'plist :file nil)))
    (should-not (agent-shell-queue-interject-available-p))
    (should-not (agent-shell-queue-session-paused-p))))

(ert-deftest agent-shell-queue/isolate-macro-creates-current-layout-struct ()
  "The test isolation macro must create a struct matching the current field count.
Fails immediately when a field is added to `agent-shell-queue-queue' but not to
the `:interjection-pending' (or subsequent) keyword in `agent-shell-queue-test/isolate'."
  (agent-shell-queue-test/isolate
    (should (= (length agent-shell-queue--queue)
               (length (agent-shell-queue-queue--make))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--next-dispatchable-item

(ert-deftest agent-shell-queue/next-dispatchable-returns-first-active ()
  "Returns the first active item in the list."
  (agent-shell-queue-test/isolate
    (let ((items (list (agent-shell-queue-test/make-item "q-1" "p" 'active nil)
                       (agent-shell-queue-test/make-item "q-2" "p" 'active nil))))
      (should (equal "q-1"
                     (agent-shell-queue-item-id
                      (agent-shell-queue--next-dispatchable-item items)))))))

(ert-deftest agent-shell-queue/next-dispatchable-skips-non-active ()
  "Running, done, and blocked items are not returned."
  (agent-shell-queue-test/isolate
    (let ((items (list (agent-shell-queue-test/make-item "q-1" "p" 'running nil)
                       (agent-shell-queue-test/make-item "q-2" "p" 'done nil)
                       (agent-shell-queue-test/make-item "q-3" "p" 'blocked.skip nil)
                       (agent-shell-queue-test/make-item "q-4" "p" 'active nil))))
      (should (equal "q-4"
                     (agent-shell-queue-item-id
                      (agent-shell-queue--next-dispatchable-item items)))))))

(ert-deftest agent-shell-queue/next-dispatchable-skips-editing-id ()
  "Active items whose ID is in editing-ids are not returned."
  (agent-shell-queue-test/isolate
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) '("q-1"))
    (let ((items (list (agent-shell-queue-test/make-item "q-1" "p" 'active nil)
                       (agent-shell-queue-test/make-item "q-2" "p" 'active nil))))
      (should (equal "q-2"
                     (agent-shell-queue-item-id
                      (agent-shell-queue--next-dispatchable-item items)))))))

(ert-deftest agent-shell-queue/next-dispatchable-empty-returns-nil ()
  (agent-shell-queue-test/isolate
    (should-not (agent-shell-queue--next-dispatchable-item nil))))

(ert-deftest agent-shell-queue/next-dispatchable-all-blocked-returns-nil ()
  (agent-shell-queue-test/isolate
    (let ((items (list (agent-shell-queue-test/make-item "q-1" "p" 'blocked.skip nil)
                       (agent-shell-queue-test/make-item "q-2" "p" 'done nil))))
      (should-not (agent-shell-queue--next-dispatchable-item items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--insert-item-after

(ert-deftest agent-shell-queue/insert-item-after-middle ()
  "New item appears immediately after the reference item."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-1" "a" active nil) ("q-2" "b" active nil) ("q-3" "c" active nil))))
    (let ((new-item (agent-shell-queue-test/make-item "q-new" "new" 'active nil)))
      (agent-shell-queue--insert-item-after "buf" new-item "q-1")
      (should (equal '("q-1" "q-new" "q-2" "q-3")
                     (seq-map #'agent-shell-queue-item-id
                               (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))))

(ert-deftest agent-shell-queue/insert-item-after-last ()
  "New item appears at the end when ref-id is the last item."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf" ("q-1" "a" active nil) ("q-2" "b" active nil))))
    (let ((new-item (agent-shell-queue-test/make-item "q-new" "new" 'active nil)))
      (agent-shell-queue--insert-item-after "buf" new-item "q-2")
      (should (equal '("q-1" "q-2" "q-new")
                     (seq-map #'agent-shell-queue-item-id
                               (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))))

(ert-deftest agent-shell-queue/insert-item-after-unknown-ref-appends ()
  "When ref-id is not found the new item is appended to the bucket."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-1" "a" active nil))))
    (let ((new-item (agent-shell-queue-test/make-item "q-new" "new" 'active nil)))
      (agent-shell-queue--insert-item-after "buf" new-item "q-999")
      (should (equal '("q-1" "q-new")
                     (seq-map #'agent-shell-queue-item-id
                               (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))))

(ert-deftest agent-shell-queue/insert-item-after-missing-bucket-is-noop ()
  "When the bucket does not exist nothing is added."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-1" "a" active nil))))
    (let ((new-item (agent-shell-queue-test/make-item "q-new" "new" 'active nil)))
      (agent-shell-queue--insert-item-after "other-buf" new-item "q-1")
      (should (= 1 (length (cdar (agent-shell-queue-store-items agent-shell-queue--store))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-unblock

(ert-deftest agent-shell-queue/unblock-blocked-skip ()
  "blocked.skip item is set to active."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-1" "p" blocked.skip nil))))
    (agent-shell-queue-unblock "q-1")
    (should (eq 'active
                (agent-shell-queue-item-status
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/unblock-blocked-dep ()
  "blocked.dep item is set to active."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-1" "p" blocked.dep nil))))
    (agent-shell-queue-unblock "q-1")
    (should (eq 'active
                (agent-shell-queue-item-status
                 (cadar (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/unblock-blocked-task-cascades-dep-items ()
  "Unblocking a blocked.task sets it and subsequent blocked.dep items to active,
stopping before the next blocked.task."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf"
             ("q-1" "first" blocked.task nil)
             ("q-2" "dep1"  blocked.dep  nil)
             ("q-3" "dep2"  blocked.dep  nil)
             ("q-4" "done"  done         nil))))
    (agent-shell-queue-unblock "q-1")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (eq 'active (agent-shell-queue-item-status (nth 0 items))))
      (should (eq 'active (agent-shell-queue-item-status (nth 1 items))))
      (should (eq 'active (agent-shell-queue-item-status (nth 2 items))))
      (should (eq 'done   (agent-shell-queue-item-status (nth 3 items)))))))

(ert-deftest agent-shell-queue/unblock-blocked-task-stops-at-next-blocked-task ()
  "Cascade stops at the next blocked.task; items after it are unchanged."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate
           '("buf"
             ("q-1" "first"  blocked.task nil)
             ("q-2" "dep"    blocked.dep  nil)
             ("q-3" "second" blocked.task nil)
             ("q-4" "after"  blocked.dep  nil))))
    (agent-shell-queue-unblock "q-1")
    (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
      (should (eq 'active      (agent-shell-queue-item-status (nth 0 items))))
      (should (eq 'active      (agent-shell-queue-item-status (nth 1 items))))
      (should (eq 'blocked.task (agent-shell-queue-item-status (nth 2 items))))
      (should (eq 'blocked.dep  (agent-shell-queue-item-status (nth 3 items)))))))

(ert-deftest agent-shell-queue/unblock-active-item-signals-error ()
  "Calling unblock on an active (non-blocked) item signals user-error."
  (agent-shell-queue-test/isolate-no-sub
    (setf (agent-shell-queue-store-items agent-shell-queue--store)
          (agent-shell-queue-test/populate '("buf" ("q-1" "p" active nil))))
    (should-error (agent-shell-queue-unblock "q-1") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue--insert-resume-task

(ert-deftest agent-shell-queue/insert-resume-task-creates-blocked-task ()
  "A blocked.task resume item is inserted immediately after the aborted item."
  (agent-shell-queue-test/isolate-no-sub
    (let ((aborted (agent-shell-queue-test/make-item "q-1" "original work" 'aborted nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf" aborted)))
      (agent-shell-queue--insert-resume-task "buf" aborted)
      (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
        (should (= 2 (length items)))
        (let ((resume (nth 1 items)))
          (should (eq 'blocked.task (agent-shell-queue-item-status resume)))
          (should (string-match-p "original work" (agent-shell-queue-item-args resume))))))))

(ert-deftest agent-shell-queue/insert-resume-task-cascades-subsequent-active-to-blocked-dep ()
  "Active items after the resume task are cascaded to blocked.dep."
  (agent-shell-queue-test/isolate-no-sub
    (let ((aborted (agent-shell-queue-test/make-item "q-1" "work" 'aborted nil))
          (next1   (agent-shell-queue-test/make-item "q-2" "next1" 'active nil))
          (next2   (agent-shell-queue-test/make-item "q-3" "next2" 'active nil)))
      (setf (agent-shell-queue-store-items agent-shell-queue--store)
            (list (list "buf" aborted next1 next2)))
      (agent-shell-queue--insert-resume-task "buf" aborted)
      (let ((items (cdar (agent-shell-queue-store-items agent-shell-queue--store))))
        ;; q-1 aborted, resume item inserted, then q-2 and q-3 cascaded
        (should (eq 'aborted     (agent-shell-queue-item-status (nth 0 items))))
        (should (eq 'blocked.task (agent-shell-queue-item-status (nth 1 items))))
        (should (eq 'blocked.dep  (agent-shell-queue-item-status (nth 2 items))))
        (should (eq 'blocked.dep  (agent-shell-queue-item-status (nth 3 items))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-raw-edit: freeze/thaw only newly-paused sessions

(ert-deftest agent-shell-queue/raw-edit-pauses-only-not-already-paused-sessions ()
  "raw-edit pauses every bucket not already in session-paused, and marks
their active items blocked.runner — a buffer already individually paused is
left untouched (still just once in the list)."
  (agent-shell-queue-test/isolate
    (let ((buf1 (get-buffer-create " *asq-raw-edit-buf1*"))
          (buf2 (get-buffer-create " *asq-raw-edit-buf2*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf1) (agent-shell-queue-test/make-item "q-1" "a" 'active nil))
                        (list (buffer-name buf2) (agent-shell-queue-test/make-item "q-2" "b" 'active nil))))
            ;; buf1 is already individually paused before raw edit starts.
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  (list (buffer-name buf1)))
            (cl-letf (((symbol-function 'yaml-encode) (lambda (&rest _) ""))
                      ((symbol-function 'pop-to-buffer) #'ignore))
              (agent-shell-queue-raw-edit))
            (unwind-protect
                (progn
                  (should (member (buffer-name buf1) (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
                  (should (member (buffer-name buf2) (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
                  (should (= 1 (seq-count (lambda (n) (equal n (buffer-name buf1)))
                                          (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
                  (with-current-buffer "*agent-shell-queue-raw-edit*"
                    (should (equal (list (buffer-name buf2))
                                   agent-shell-queue--raw-edit-newly-paused))))
              (when (get-buffer "*agent-shell-queue-raw-edit*")
                (kill-buffer "*agent-shell-queue-raw-edit*"))))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest agent-shell-queue/raw-edit-cancel-restores-exact-prior-pause-state ()
  "raw-edit-cancel unpauses exactly the sessions raw-edit newly paused,
leaving sessions that were already individually paused beforehand untouched."
  (agent-shell-queue-test/isolate
    (let ((buf1 (get-buffer-create " *asq-raw-edit-cancel-buf1*"))
          (buf2 (get-buffer-create " *asq-raw-edit-cancel-buf2*")))
      (unwind-protect
          (progn
            (setf (agent-shell-queue-store-items agent-shell-queue--store)
                  (list (list (buffer-name buf1) (agent-shell-queue-test/make-item "q-1" "a" 'active nil))
                        (list (buffer-name buf2) (agent-shell-queue-test/make-item "q-2" "b" 'active nil))))
            (setf (agent-shell-queue-queue-session-paused agent-shell-queue--queue)
                  (list (buffer-name buf1)))
            (cl-letf (((symbol-function 'yaml-encode) (lambda (&rest _) ""))
                      ((symbol-function 'pop-to-buffer) #'ignore))
              (agent-shell-queue-raw-edit))
            (with-current-buffer "*agent-shell-queue-raw-edit*"
              (cl-letf (((symbol-function 'quit-window) #'ignore))
                (agent-shell-queue-raw-edit-cancel)))
            ;; buf1 (paused before raw edit) remains paused.
            (should (member (buffer-name buf1) (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
            ;; buf2 (only paused because of raw edit) is unpaused again.
            (should-not (member (buffer-name buf2) (agent-shell-queue-queue-session-paused agent-shell-queue--queue))))
        (kill-buffer buf1)
        (kill-buffer buf2)
        (when (get-buffer "*agent-shell-queue-raw-edit*")
          (kill-buffer "*agent-shell-queue-raw-edit*"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest agent-shell-queue/get-background-prefix-resolves-properly ()
  "Test background prefix resolution logic using various configurations."
  (let ((buf (get-buffer-create " *asq-test-bg-buf*"))
        (agent-shell-queue-background-prefix '((omp . "/bg-omp ") (t . "/bg-default "))))
    (unwind-protect
        (progn
          ;; 1. Check with omp config matching
          (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . omp)))))
            (should (equal "/bg-omp " (agent-shell-queue--get-background-prefix buf))))
          ;; 2. Check with fallback matching (t key)
          (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . claude-code)))))
            (should (equal "/bg-default " (agent-shell-queue--get-background-prefix buf))))
          ;; 3. Check with string config override
          (let ((agent-shell-queue-background-prefix "/custom-bg "))
            (should (equal "/custom-bg " (agent-shell-queue--get-background-prefix buf))))
          ;; 4. Check with function override
          (let ((agent-shell-queue-background-prefix (lambda (ident) (if (eq ident 'omp) "/f-omp " "/f-def "))))
            (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . omp)))))
              (should (equal "/f-omp " (agent-shell-queue--get-background-prefix buf))))
            (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . gemini-cli)))))
              (should (equal "/f-def " (agent-shell-queue--get-background-prefix buf))))))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue/get-clear-command-resolves-properly ()
  "Test clear command resolution logic using various configurations."
  (let ((buf (get-buffer-create " *asq-test-clear-buf*"))
        (agent-shell-queue-clear-command '((omp . "/fresh-omp") (t . "/clear-default"))))
    (unwind-protect
        (progn
          ;; 1. Check with omp config matching
          (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . omp)))))
            (should (equal "/fresh-omp" (agent-shell-queue--get-clear-command buf))))
          ;; 2. Check with fallback matching (t key)
          (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . claude-code)))))
            (should (equal "/clear-default" (agent-shell-queue--get-clear-command buf))))
          ;; 3. Check with string config override
          (let ((agent-shell-queue-clear-command "/custom-clear"))
            (should (equal "/custom-clear" (agent-shell-queue--get-clear-command buf))))
          ;; 4. Check with function override
          (let ((agent-shell-queue-clear-command (lambda (ident) (if (eq ident 'omp) "/f-fresh" "/f-clear"))))
            (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . omp)))))
              (should (equal "/f-fresh" (agent-shell-queue--get-clear-command buf))))
            (cl-letf (((symbol-function 'agent-shell-get-config) (lambda (_buf) '((:identifier . gemini-cli)))))
              (should (equal "/f-clear" (agent-shell-queue--get-clear-command buf))))))
      (kill-buffer buf))))
(ert-deftest agent-shell-queue/pause-delay-and-alerts ()
  "Test pause delays, start alerts, and pre-end alerts."
  (agent-shell-queue-test/isolate-no-sub
   (let ((alerts nil)
         (agent-shell-queue-default-pause-delay 30)
         (agent-shell-queue-alert-on-pause-start t)
         (agent-shell-queue-alert-before-pause-end 10)
         (agent-shell-queue--pause-timers nil))
     (cl-letf (((symbol-function 'alert)
                (lambda (msg &rest args)
                  (push (cons msg args) alerts))))
       ;; Start pause delay for 30s
       (agent-shell-queue--start-pause-delay "buf1" 30 "Task pause" nil)
       ;; 1. Check start alert was fired
       (should (= 1 (length alerts)))
       (should (string-match-p "Task pause started (30 s)" (caar alerts)))
       (should (assoc "buf1" agent-shell-queue--pause-timers))
       (agent-shell-queue--cancel-pause-timer "buf1")
       (should-not (assoc "buf1" agent-shell-queue--pause-timers))))))

(ert-deftest agent-shell-queue/item-delays-persistence ()
  "Test delay-before and delay-after fields in item struct and persistence."
  (agent-shell-queue-test/isolate-no-sub
   (let ((item (agent-shell-queue-item--make
                :id "d1"
                :args "test delays"
                :status 'active
                :delay-before 5
                :delay-after 10)))
     (should (= 5 (agent-shell-queue-item-delay-before item)))
     (should (= 10 (agent-shell-queue-item-delay-after item)))

     ;; JSON round-trip
     (let* ((json-obj (agent-shell-queue--item-to-json item))
            (item-from-j (agent-shell-queue--item-from-json json-obj)))
       (should (= 5 (agent-shell-queue-item-delay-before item-from-j)))
       (should (= 10 (agent-shell-queue-item-delay-after item-from-j))))

     ;; YAML round-trip
     (let* ((yaml-obj (agent-shell-queue--item-to-yaml item))
            (item-from-y (agent-shell-queue--item-from-yaml yaml-obj)))
       (should (= 5 (agent-shell-queue-item-delay-before item-from-y)))
       (should (= 10 (agent-shell-queue-item-delay-after item-from-y)))))))

(ert-deftest agent-shell-queue/timed-pause-item ()
  "Test inserting and dispatching a timed pause item."
  (agent-shell-queue-test/isolate-no-sub
   (let ((buf (get-buffer-create " *asq-test-pause-buf*"))
         (alerts nil)
         (agent-shell-queue-alert-on-pause-start t))
     (unwind-protect
         (cl-letf (((symbol-function 'alert)
                    (lambda (msg &rest args)
                      (push (cons msg args) alerts))))
           (agent-shell-queue-insert-pause buf nil 15)
           (let* ((items (cdr (assoc " *asq-test-pause-buf*" (agent-shell-queue-store-items agent-shell-queue--store))))
                  (pause-item (car items)))
             (should (eq 'pause (agent-shell-queue-item-kind pause-item)))
             (should (= 15 (agent-shell-queue-item-delay-after pause-item)))
             ;; Dispatch pause item
             (agent-shell-queue--dispatch-pause-compact pause-item " *asq-test-pause-buf*")
             ;; Start alert should fire
             (should (string-match-p "Pause item started (15 s)" (caar alerts)))
             (agent-shell-queue--cancel-pause-timer " *asq-test-pause-buf*")))
       (kill-buffer buf)))))
(ert-deftest agent-shell-queue/alert-delegates-and-intercepts-errors ()
  "Test `agent-shell-queue--alert` delegates to `alert` and intercepts errors."
  (let (captured-msg captured-args)
    (cl-letf (((symbol-function 'alert)
               (lambda (msg &rest args)
                 (setq captured-msg msg
                       captured-args args))))
      (agent-shell-queue--alert "Test message" :title "Test title" :severity 'low)
      (should (equal "Test message" captured-msg))
      (should (equal '(:title "Test title" :severity low) captured-args))))
  ;; Test error interception
  (cl-letf (((symbol-function 'alert)
             (lambda (&rest _)
               (error "Simulated notification backend failure"))))
    ;; Must not signal error
    (should-not (condition-case err
                    (progn
                      (agent-shell-queue--alert "Failing alert")
                      nil)
                  (error t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Queues & Halted-on-Abort Tests

(ert-deftest agent-shell-queue/dir-queue-bucket-helpers ()
  "Test bucket helpers for dir:<path> directory queues."
  (let ((dir "/tmp/test-project/"))
    (should (agent-shell-queue--dir-bucket-p "dir:/tmp/test-project/"))
    (should-not (agent-shell-queue--dir-bucket-p "*agent-shell*"))
    (should (equal "/tmp/test-project/" (agent-shell-queue--dir-from-bucket "dir:/tmp/test-project/")))
    (should (equal (concat "dir:" (file-name-as-directory (expand-file-name "/tmp/test-project")))
                   (agent-shell-queue--bucket-for-dir "/tmp/test-project")))))

(ert-deftest agent-shell-queue/dir-queue-add-directory ()
  "Test adding an item to a directory queue."
  (agent-shell-queue-test/isolate-no-sub
   (let* ((item (agent-shell-queue-add-directory "Do work in dir" "/tmp/my-proj")))
     (should item)
     (should (equal (agent-shell-queue--canonicalize-dir "/tmp/my-proj")
                    (agent-shell-queue-item-directory item)))
     (let ((bucket (agent-shell-queue--bucket-for-dir "/tmp/my-proj")))
       (should (assoc bucket (agent-shell-queue-store-items agent-shell-queue--store)))))))

(ert-deftest agent-shell-queue/dir-queue-pick-shell-and-naming ()
  "Test picking shell for directory queue creates a shell named with item-id."
  (agent-shell-queue-test/isolate-no-sub
   (let ((created-buf nil))
     (cl-letf (((symbol-function 'agent-shell-new-shell)
                (lambda ()
                  (setq created-buf (get-buffer-create "*agent-shell: test-dir*"))
                  (with-current-buffer created-buf
                    (setq default-directory "/tmp/test-dir/"))
                  created-buf)))
       (let ((buf (agent-shell-queue--pick-shell-for-directory "/tmp/test-dir/" "q1234")))
         (should buf)
         (should (equal "*agent-shell: test-dir*-q1234" (buffer-name buf)))
         (kill-buffer buf))))))

(ert-deftest agent-shell-queue/halted-on-abort-state ()
  "Test marking, checking, and clearing halted-on-abort status."
  (agent-shell-queue-test/isolate-no-sub
   (agent-shell-queue--mark-halted-on-abort "buf-a")
   (should (agent-shell-queue--halted-on-abort-p "buf-a"))
   (agent-shell-queue--clear-halted-on-abort "buf-a")
   (should-not (agent-shell-queue--halted-on-abort-p "buf-a"))))

(ert-deftest agent-shell-queue/on-interrupt-halts-dispatch ()
  "Test that on-interrupt sets halted-on-abort and blocks auto-dispatch."
  (agent-shell-queue-test/isolate-no-sub
   (let ((buf (get-buffer-create "*asq-interrupt-buf*")))
     (unwind-protect
         (progn
          (with-current-buffer buf
            (setq major-mode 'agent-shell-mode)
            (setq default-directory "/tmp/asq-int/"))
           (setf (agent-shell-queue-store-items agent-shell-queue--store)
                 (list (list (buffer-name buf)
                             (agent-shell-queue-test/make-item "q-int" "prompt" 'active nil))))
           (with-current-buffer buf
             (agent-shell-queue--on-interrupt))
           (should (agent-shell-queue--halted-on-abort-p (buffer-name buf)))
           ;; dispatch-if-ready must refuse to send while halted
           (cl-letf (((symbol-function 'agent-shell-queue-send-item)
                      (lambda (_) (error "Should not dispatch while halted"))))
             (should-not (agent-shell-queue--dispatch-if-ready buf))))
       (kill-buffer buf)))))

(ert-deftest agent-shell-queue/recovery-criteria-verification ()
  "Test the 3 recovery criteria (uninterrupted, no question, not in plan mode)."
  (agent-shell-queue-test/isolate-no-sub
   (let ((buf (get-buffer-create "*asq-recovery-buf*")))
     (unwind-protect
         (progn
          (with-current-buffer buf
            (setq major-mode 'agent-shell-mode))
           ;; Question detection
           (should (agent-shell-queue--response-has-question-p "What next?"))
           (should (agent-shell-queue--response-has-question-p "Shall I continue?\n"))
           (should-not (agent-shell-queue--response-has-question-p "Done with task."))
           ;; Clean item vs aborted item
           (let ((clean-item (agent-shell-queue-test/make-item "q1" "p" 'done nil))
                 (aborted-item (agent-shell-queue-test/make-item "q2" "p" 'done nil)))
             (setf (agent-shell-queue-item-outcome aborted-item) 'aborted)
             (should (agent-shell-queue--verify-recovery buf clean-item "Done successfully."))
             (should-not (agent-shell-queue--verify-recovery buf aborted-item "Done successfully."))
             (should-not (agent-shell-queue--verify-recovery buf clean-item "Should I proceed?"))))
       (kill-buffer buf)))))

(ert-deftest agent-shell-queue/recovery-clears-halted-on-abort ()
  "Test turn complete clears halted-on-abort when recovery criteria pass."
  (agent-shell-queue-test/isolate-no-sub
   (let ((buf (get-buffer-create "*asq-rec-complete-buf*")))
     (unwind-protect
         (progn
          (with-current-buffer buf
            (setq major-mode 'agent-shell-mode))
           (agent-shell-queue--mark-halted-on-abort (buffer-name buf))
           (should (agent-shell-queue--halted-on-abort-p (buffer-name buf)))
           (let ((item (agent-shell-queue-test/make-item "q-rec" "p" 'running nil)))
             (setf (agent-shell-queue-item-response item) "All steps completed successfully.")
             (setf (agent-shell-queue-store-items agent-shell-queue--store)
                   (list (list (buffer-name buf) item)))
             (cl-letf (((symbol-function 'agent-shell-queue--send-next-for-buffer) #'ignore))
               (agent-shell-queue--on-turn-complete buf (buffer-name buf) nil))
             (should-not (agent-shell-queue--halted-on-abort-p (buffer-name buf)))))
       (kill-buffer buf)))))

(ert-deftest agent-shell-queue/persistence-halted-sessions-and-dir-buckets ()
  "Test queue struct migration preserves halted-sessions."
  (agent-shell-queue-test/isolate-no-sub
   (setf (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue) '("dir:/tmp/test/" "buf-halted"))
   (should (member "dir:/tmp/test/" (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue)))
   (should (member "buf-halted" (agent-shell-queue-queue-halted-sessions agent-shell-queue--queue)))))

(ert-deftest agent-shell-queue/recovery-plan-mode-blocks-recovery ()
  "Recovery check fails when session is in a blocked mode (e.g. plan or dontAsk)."
  (agent-shell-queue-test/isolate-no-sub
   (let ((buf (get-buffer-create "*asq-mode-block-buf*")))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (setq major-mode 'agent-shell-mode)
             (setq-local agent-shell--state '((:session (:mode-id . "plan")))))
           (let ((clean-item (agent-shell-queue-test/make-item "q-plan" "prompt" 'done nil)))
             (should-not (agent-shell-queue--verify-recovery buf clean-item "All done.")))
           (with-current-buffer buf
             (setq-local agent-shell--state '((:session (:mode-id . "dontAsk")))))
           (let ((clean-item (agent-shell-queue-test/make-item "q-dontask" "prompt" 'done nil)))
             (should-not (agent-shell-queue--verify-recovery buf clean-item "All done.")))
           (with-current-buffer buf
             (setq-local agent-shell--state '((:session (:mode-id . "default")))))
           (let ((clean-item (agent-shell-queue-test/make-item "q-default" "prompt" 'done nil)))
             (should (agent-shell-queue--verify-recovery buf clean-item "All done."))))
       (kill-buffer buf)))))

(ert-deftest agent-shell-queue/dir-queue-execution-spawns-new-shell ()
  "Dispatching a directory queue item spawns a new shell buffer and marks item running."
  (agent-shell-queue-test/isolate-no-sub
   (let* ((spawned-buf nil)
          (dispatched-text nil)
          (item (agent-shell-queue-add-directory "Dir work prompt" "/tmp/my-dir-queue/")))
     (cl-letf (((symbol-function 'agent-shell-new-shell)
                (lambda ()
                  (setq spawned-buf (get-buffer-create "*agent-shell: my-dir-queue*"))
                  (with-current-buffer spawned-buf
                    (setq default-directory "/tmp/my-dir-queue/"))
                  spawned-buf))
               ((symbol-function 'agent-shell-insert)
                (lambda (&rest args)
                  (setq dispatched-text (plist-get args :text)))))
       (unwind-protect
           (progn
             (agent-shell-queue-send-item (agent-shell-queue-item-id item))
             (should (eq (agent-shell-queue-item-status item) 'running))
             (should (equal "Dir work prompt" dispatched-text))
             (should spawned-buf)
             (should (string-match-p (regexp-quote (agent-shell-queue-item-id item))
                                     (buffer-name spawned-buf))))
         (when (and spawned-buf (buffer-live-p spawned-buf))
           (kill-buffer spawned-buf)))))))

;;; test-agent-shell-queue.el ends here
