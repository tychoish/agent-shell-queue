;;; test-agent-shell-quota.el --- ERT tests for agent-shell-quota -*- lexical-binding: t -*-

;;; Commentary:
;; Offline ERT tests for `agent-shell-quota' using recorded `codexbar'
;; JSON fixtures and mocked async process lifecycle.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-shell-quota)

;;; Recorded Fixtures

(defconst test-agent-shell-quota--codex-json
  "[{\"provider\":\"codex\",\"source\":\"oauth\",\"rateWindowLabels\":{\"primary\":\"Session\",\"secondary\":\"Weekly\"},\"pace\":{\"primary\":{\"summary\":\"5% in reserve | Lasts until reset\"},\"secondary\":{\"summary\":\"17% in deficit | Runs out in 2d 1h\"}},\"usage\":{\"loginMethod\":\"plus\",\"primary\":{\"usedPercent\":60,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:31Z\"},\"secondary\":{\"usedPercent\":30,\"windowMinutes\":10080,\"resetsAt\":\"2026-12-03T03:44:13Z\"},\"tertiary\":null,\"providerCost\":{\"used\":0,\"limit\":0,\"currencyCode\":\"Credits\",\"period\":\"Extra usage\"}}}]"
  "Recorded `codexbar --provider codex --json' fixture.")

(defconst test-agent-shell-quota--claude-enterprise-json
  "[{\"provider\":\"claude\",\"source\":\"oauth\",\"usage\":{\"primary\":null,\"secondary\":null,\"tertiary\":null,\"loginMethod\":\"Claude Enterprise\",\"providerCost\":{\"currencyCode\":\"USD\",\"used\":490.49,\"limit\":1400,\"period\":\"Spend limit\"}}}]"
  "Recorded `codexbar --provider claude --json' fixture for Enterprise spend.")

(defconst test-agent-shell-quota--antigravity-json
  "[{\"provider\":\"antigravity\",\"source\":\"cli\",\"rateWindowLabels\":{\"primary\":\"Gemini Models\",\"secondary\":\"Claude and GPT\"},\"usage\":{\"primary\":{\"usedPercent\":45.1,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:36Z\"},\"secondary\":{\"usedPercent\":0,\"windowMinutes\":300,\"resetsAt\":\"1970-01-01T00:00:00Z\"},\"extraRateWindows\":[{\"id\":\"gemini-5h\",\"title\":\"Gemini 5-hour\",\"window\":{\"usedPercent\":45.1,\"windowMinutes\":300,\"resetsAt\":\"2026-11-27T02:29:36Z\"}},{\"id\":\"gemini-weekly\",\"title\":\"Gemini weekly\",\"window\":{\"usedPercent\":32.1,\"windowMinutes\":10080,\"resetsAt\":\"2026-12-03T16:29:36Z\"}},{\"id\":\"3p-5h\",\"title\":\"Claude/GPT 5-hour\",\"window\":{\"usedPercent\":0,\"windowMinutes\":300,\"resetsAt\":\"1970-01-01T00:00:00Z\"}},{\"id\":\"3p-weekly\",\"title\":\"Claude/GPT weekly\",\"window\":{\"usedPercent\":0,\"windowMinutes\":10080,\"resetsAt\":\"1970-01-01T00:00:00Z\"}}]}}]"
  "Recorded `codexbar --provider antigravity --json' fixture.")

;;; Parsing Tests

(ert-deftest test-agent-shell-quota/parse-codex ()
  "Codex fixture parses primary/secondary windows, labels, and pace notes."
  (let* ((entry (agent-shell-quota--parse-output
                 "codex" test-agent-shell-quota--codex-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (equal (agent-shell-quota-entry-plan entry) "plus"))
    (should (equal (agent-shell-quota-entry-source entry) "oauth"))
    (should (= (length windows) 2))
    (let ((w1 (nth 0 windows))
          (w2 (nth 1 windows)))
      (should (equal (agent-shell-quota-window-label w1) "Session"))
      (should (= (agent-shell-quota-window-used-percent w1) 60.0))
      (should (numberp (agent-shell-quota-window-resets-at w1)))
      (should (equal (agent-shell-quota-window-note w1)
                     "5% in reserve | Lasts until reset"))
      (should (equal (agent-shell-quota-window-label w2) "Weekly"))
      (should (= (agent-shell-quota-window-used-percent w2) 30.0))
      (should (equal (agent-shell-quota-window-note w2)
                     "17% in deficit | Runs out in 2d 1h")))))

(ert-deftest test-agent-shell-quota/parse-claude-enterprise ()
  "Claude Enterprise fixture emits a Spend limit window from providerCost."
  (let* ((entry (agent-shell-quota--parse-output
                 "claude" test-agent-shell-quota--claude-enterprise-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (equal (agent-shell-quota-entry-plan entry) "Claude Enterprise"))
    (should (= (length windows) 1))
    (let ((w (car windows)))
      (should (equal (agent-shell-quota-window-label w) "Spend limit"))
      (should (< (abs (- (agent-shell-quota-window-used-percent w) 35.035)) 0.01))
      (should (null (agent-shell-quota-window-resets-at w)))
      (should (equal (agent-shell-quota-window-note w)
                     "$490.49 / $1400 (Claude Enterprise)")))))

(ert-deftest test-agent-shell-quota/parse-antigravity-extra-windows ()
  "Antigravity prefers extraRateWindows and converts epoch-0 resetsAt to nil."
  (let* ((entry (agent-shell-quota--parse-output
                 "antigravity" test-agent-shell-quota--antigravity-json 1000.0))
         (windows (agent-shell-quota-entry-windows entry)))
    (should (eq (agent-shell-quota-entry-status entry) 'ok))
    (should (= (length windows) 4))
    (should (equal (agent-shell-quota-window-label (nth 0 windows)) "Gemini 5-hour"))
    (should (numberp (agent-shell-quota-window-resets-at (nth 0 windows))))
    (should (equal (agent-shell-quota-window-label (nth 2 windows)) "Claude/GPT 5-hour"))
    (should (null (agent-shell-quota-window-resets-at (nth 2 windows))))
    (should (equal (substring-no-properties
                    (agent-shell-quota--format-reset
                     (agent-shell-quota-window-resets-at (nth 2 windows)) 1000.0))
                   "---"))))

;;; Formatting and Threshold Tests

(ert-deftest test-agent-shell-quota/faces-and-bars ()
  "Threshold faces match ok (<60), warn (60..84), and critical (>=85)."
  (should (eq (agent-shell-quota--usage-face 30) 'agent-shell-quota-ok-face))
  (should (eq (agent-shell-quota--usage-face 60) 'agent-shell-quota-warn-face))
  (should (eq (agent-shell-quota--usage-face 85) 'agent-shell-quota-critical-face))
  (let ((bar (agent-shell-quota--format-bar 50)))
    (should (equal (substring-no-properties bar) "[█████░░░░░]  50%"))
    (should (eq (get-text-property 0 'face bar) 'agent-shell-quota-ok-face))))

(ert-deftest test-agent-shell-quota/format-reset-countdowns ()
  "Countdown formatting handles past, minutes, hours, and days."
  (let ((now 100000.0))
    (should (equal (substring-no-properties
                    (agent-shell-quota--format-reset (- now 10) now))
                   "reset"))
    (should (equal (agent-shell-quota--format-reset (+ now 600) now) "in 10m"))
    (should (equal (agent-shell-quota--format-reset (+ now 5400) now) "in 1h 30m"))
    (should (equal (agent-shell-quota--format-reset (+ now 93600) now) "in 1d 2h"))))

;;; Incremental Async, Deduplication, Error, and Timeout Tests

(ert-deftest test-agent-shell-quota/incremental-async-and-dedup ()
  "Parallel refreshes update rows incrementally and deduplicate in-flight providers."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota--inflight (make-hash-table :test #'equal))
        (agent-shell-quota-providers '("claude" "codex"))
        (spawned nil)
        (sentinels (make-hash-table :test #'equal))
        (outbufs (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codexbar"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (let* ((cmd (plist-get plist :command))
                        (prov (caddr cmd))
                        (buf (plist-get plist :buffer))
                        (sentinel (plist-get plist :sentinel))
                        (fake-proc (list :provider prov :live t :exit 0)))
                   (push prov spawned)
                   (setf (map-elt sentinels prov) (cons fake-proc sentinel))
                   (setf (map-elt outbufs prov) buf)
                   fake-proc)))
              ((symbol-function 'process-live-p) (lambda (p) (plist-get p :live)))
              ((symbol-function 'process-exit-status) (lambda (p) (plist-get p :exit)))
              ((symbol-function 'process-get) (lambda (p k) (plist-get p k)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      ;; First refresh launches both providers
      (agent-shell-quota-refresh)
      (should (= (length spawned) 2))
      ;; Second refresh while in-flight spawns nothing new
      (agent-shell-quota-refresh)
      (should (= (length spawned) 2))
      ;; Both rows currently show loading
      (let ((rows (agent-shell-quota--build-rows)))
        (should (= (length rows) 2))
        (should (equal (substring-no-properties (aref (cadr (nth 0 rows)) 1)) "loading...")))
      ;; Complete claude first -> incremental update
      (with-current-buffer (map-elt outbufs "claude")
        (insert test-agent-shell-quota--claude-enterprise-json))
      (let ((cell (map-elt sentinels "claude")))
        (plist-put (car cell) :live nil)
        (funcall (cdr cell) (car cell) "finished\n"))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "claude"))
                  'ok))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "codex"))
                  'loading))
      (should-not (buffer-live-p (map-elt outbufs "claude")))
      ;; Complete codex with error -> sibling claude intact
      (with-current-buffer (map-elt outbufs "codex")
        (insert "auth expired\n"))
      (let ((cell (map-elt sentinels "codex")))
        (plist-put (car cell) :live nil)
        (plist-put (car cell) :exit 1)
        (funcall (cdr cell) (car cell) "exited abnormally\n"))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "claude"))
                  'ok))
      (should (eq (agent-shell-quota-entry-status
                   (map-elt agent-shell-quota--cache "codex"))
                  'error))
      (should (equal (agent-shell-quota-entry-error-message
                      (map-elt agent-shell-quota--cache "codex"))
                     "auth expired")))))

(ert-deftest test-agent-shell-quota/timeout-and-bad-json ()
  "Timed-out process and malformed JSON both record structured error entries."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota--inflight (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'process-exit-status) (lambda (p) (plist-get p :exit)))
              ((symbol-function 'process-get) (lambda (p k) (plist-get p k))))
      ;; Timeout case
      (let ((buf (generate-new-buffer " *test-timeout*")))
        (agent-shell-quota--handle-exit
         "antigravity" '(:exit 9 agent-shell-quota-timed-out t) buf)
        (should-not (buffer-live-p buf))
        (let ((entry (map-elt agent-shell-quota--cache "antigravity")))
          (should (eq (agent-shell-quota-entry-status entry) 'error))
          (should (string-match-p "Timed out" (agent-shell-quota-entry-error-message entry)))))
      ;; Bad JSON case
      (let ((buf (generate-new-buffer " *test-badjson*")))
        (with-current-buffer buf (insert "not-json"))
        (agent-shell-quota--handle-exit "codex" '(:exit 0) buf)
        (should-not (buffer-live-p buf))
        (let ((entry (map-elt agent-shell-quota--cache "codex")))
          (should (eq (agent-shell-quota-entry-status entry) 'error))
          (should (string-match-p "Parse error" (agent-shell-quota-entry-error-message entry))))))))

;;; HUD Modeline Segment Tests

(ert-deftest test-agent-shell-quota/hud-segment ()
  "HUD segment returns nil when empty and compact peak summary when populated."
  (let ((agent-shell-quota--cache (make-hash-table :test #'equal))
        (agent-shell-quota-providers '("antigravity" "codex" "claude")))
    (should (null (agent-shell-quota--hud-segment)))
    (setf (map-elt agent-shell-quota--cache "antigravity")
          (agent-shell-quota--parse-output
           "antigravity" test-agent-shell-quota--antigravity-json 1000.0))
    (setf (map-elt agent-shell-quota--cache "codex")
          (agent-shell-quota--parse-output
           "codex" test-agent-shell-quota--codex-json 1000.0))
    (setf (map-elt agent-shell-quota--cache "claude")
          (agent-shell-quota--parse-output
           "claude" test-agent-shell-quota--claude-enterprise-json 1000.0))
    (let ((seg (agent-shell-quota--hud-segment)))
      (should (equal (substring-no-properties seg) "Agy:45% Cdx:60% Cld:35%")))))

(provide 'test-agent-shell-quota)
;;; test-agent-shell-quota.el ends here
