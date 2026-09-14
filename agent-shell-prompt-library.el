;;; agent-shell-prompt-library.el --- Built-in agent-shell-prompt workflows -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Standard `register-agent-shell-prompt` registrations: CI failure
;; remediation, PR review patching, coverage expansion, refactor
;; cleanup, and git commit authoring.  Each pre-op is deterministic Elisp
;; gathering exact context via `gh` or `git` (using Magit when available)
;; rather than letting the agent hallucinate it.  Large artifacts (logs,
;; diffs > 5 lines, review dumps) are saved to disk or summarized via
;; diffstat rather than inlining excessive context directly into prompt turns.

;;; Code:

(require 'seq)
(require 'agent-shell-prompt)

(declare-function magit-git-output "magit-git" (&rest args))
(declare-function magit-get-current-branch "magit-git" ())
(declare-function magit-dash-gh--repo-info "magit-dash" ())
(declare-function magit-dash--repo-at-point "magit-dash" ())
(declare-function magit-dash-repo-name "magit-dash" (repo))
(declare-function annotated-completing-read "annotated-completing-read")
(declare-function vc-git-branches "vc-git" ())

(defun agent-shell-prompt-library--shell (&rest args)
  "Run ARGS as a shell command in `default-directory` and return its output.
Trailing newline is trimmed.  Errors are captured inline in the result
rather than signaled, so a pre-op can surface tool failures to the agent
instead of aborting the workflow."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'call-process (car args) nil t nil (cdr args))))))

(defun agent-shell-prompt-library--git-output (&rest args)
  "Run git with ARGS in `default-directory` and return trimmed output.
Uses `magit-git-output` when available, falling back to
`agent-shell-prompt-library--shell`."
  (if (fboundp 'magit-git-output)
      (string-trim (or (apply #'magit-git-output args) ""))
    (apply #'agent-shell-prompt-library--shell "git" args)))

(defun agent-shell-prompt-library--diff-summary (args &optional max-lines)
  "Return git diff for ARGS, truncating to `--stat` if lines exceed MAX-LINES.
MAX-LINES defaults to 5.  When diff has <= MAX-LINES lines, returns full diff.
When diff exceeds MAX-LINES lines, returns `git diff --stat` output with a note."
  (let* ((limit (or max-lines 5))
         (full-diff (apply #'agent-shell-prompt-library--git-output (append '("diff") args)))
         (trimmed (string-trim full-diff)))
    (if (string-empty-p trimmed)
        "(no changes)"
      (let ((lines (split-string trimmed "\n" t)))
        (if (<= (length lines) limit)
            trimmed
          (let* ((stat (apply #'agent-shell-prompt-library--git-output (append '("diff" "--stat") args)))
                 (trimmed-stat (string-trim (or stat ""))))
            (if (not (string-empty-p trimmed-stat))
                (format "%s\n(Diff exceeds %d lines — run `git diff%s` to view full diff)"
                        trimmed-stat limit
                        (if args (concat " " (mapconcat #'identity args " ")) ""))
              (format "%s\n...\n(Truncated — run `git diff` to view full diff)"
                      (mapconcat #'identity (seq-take lines limit) "\n")))))))))

(defun agent-shell-prompt-library--gather (ctx pairs)
  "Populate CTX with the output of each shell command in PAIRS.
PAIRS is a list of (CTX-KEY COMMAND ARG...) entries; each COMMAND is run
via `agent-shell-prompt-library--shell` and stored under CTX-KEY."
  (dolist (pair pairs ctx)
    (plist-put ctx (car pair) (apply #'agent-shell-prompt-library--shell (cdr pair)))))

(defun agent-shell-prompt-library--iso-to-seconds (iso-str)
  "Convert ISO-STR timestamp string to float seconds."
  (when (and (stringp iso-str) (not (string-empty-p iso-str)))
    (ignore-errors
      (float-time (encode-time (parse-time-string iso-str))))))

(defun agent-shell-prompt-library--format-duration (start-iso end-iso)
  "Format duration between START-ISO and END-ISO string."
  (let ((s (agent-shell-prompt-library--iso-to-seconds start-iso))
        (e (agent-shell-prompt-library--iso-to-seconds end-iso)))
    (if (and s e)
        (let ((diff (max 0 (floor (- e s)))))
          (cond ((< diff 60) (format "%ds" diff))
                ((< diff 3600) (format "%dm %ds" (/ diff 60) (% diff 60)))
                (t (format "%dh %dm" (/ diff 3600) (% (% diff 3600) 60)))))
      "n/a")))

(defun agent-shell-prompt-library--format-time-ago (iso-time)
  "Format ISO-TIME string as relative time ago."
  (let ((t-sec (agent-shell-prompt-library--iso-to-seconds iso-time)))
    (if t-sec
        (let ((diff (max 0 (floor (- (float-time) t-sec)))))
          (cond ((< diff 60) "just now")
                ((< diff 3600) (format "%dm ago" (/ diff 60)))
                ((< diff 86400) (format "%dh ago" (/ diff 3600)))
                (t (format "%dd ago" (/ diff 86400)))))
      "n/a")))

(defun agent-shell-prompt-library--resolve-repo-slug (repo)
  "Resolve REPO name or path to an OWNER/NAME GitHub repository slug string."
  (if (and (stringp repo) (string-match-p "/" repo))
      repo
    (or (ignore-errors
          (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
            (unless (or (string-empty-p slug) (string-match-p "^error" slug))
              slug)))
        (ignore-errors
          (and (fboundp 'magit-dash-gh--repo-info)
               (when-let* ((info (magit-dash-gh--repo-info))
                           (o (plist-get info :owner))
                           (r (plist-get info :repo)))
                 (format "%s/%s" o r))))
        repo)))

(defun agent-shell-prompt-library--fetch-runs (repo &optional limit)
  "Fetch recent GitHub Actions run records for REPO as a list of alists.
Optional LIMIT sets maximum runs to fetch (defaults to 20)."
  (when-let* ((slug (agent-shell-prompt-library--resolve-repo-slug repo))
              ((executable-find "gh" t)))
    (let* ((lim (number-to-string (or limit 20)))
           (json-str (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "gh" nil t nil "run" "list"
                                       "--repo" slug
                                       "--limit" lim
                                       "--json" "databaseId,displayTitle,status,conclusion,headBranch,headSha,createdAt,updatedAt,startedAt,url"))))
           (parsed (ignore-errors (json-parse-string json-str :object-type 'alist :array-type 'list))))
      (when (listp parsed) parsed))))

(defun agent-shell-prompt-library--current-branch ()
  "Return current git branch name or `main`."
  (or (ignore-errors
        (and (fboundp 'magit-get-current-branch)
             (magit-get-current-branch)))
      (ignore-errors
        (car (vc-git-branches)))
      (let ((b (ignore-errors (string-trim (shell-command-to-string "git branch --show-current")))))
        (unless (or (null b) (string-empty-p b)) b))
      "main"))

(defun agent-shell-prompt-library--resolve-ci-run (repo &optional target-branch)
  "Return a run-id for REPO and TARGET-BRANCH.
If the latest run on TARGET-BRANCH is failing, return its run-id automatically.
Otherwise, prompt the user with an ACR picker showing recent runs."
  (let* ((branch (or target-branch (agent-shell-prompt-library--current-branch)))
         (runs (agent-shell-prompt-library--fetch-runs repo 20))
         (branch-runs (seq-filter (lambda (r) (equal (map-elt r 'headBranch) branch)) runs))
         (target-runs (or branch-runs runs))
         (latest (car target-runs))
         (latest-conclusion (and latest (or (map-elt latest 'conclusion) (map-elt latest 'status))))
         (latest-failing-p (and latest
                                (member latest-conclusion '("failure" "cancelled" "timed_out" "action_required")))))
    (if latest-failing-p
        (map-elt latest 'databaseId)
      (if (null target-runs)
          (user-error "No CI runs found for %s" repo)
        (let* ((items (mapcar
                       (lambda (r)
                         (let* ((id (map-elt r 'databaseId))
                                (title (map-elt r 'displayTitle))
                                (sha (map-elt r 'headSha))
                                (short-sha (if (and (stringp sha) (>= (length sha) 7))
                                               (substring sha 0 7)
                                             (or sha "")))
                                (b (map-elt r 'headBranch))
                                (status (map-elt r 'status))
                                (conclusion (or (map-elt r 'conclusion) status))
                                (dur (agent-shell-prompt-library--format-duration
                                      (map-elt r 'startedAt) (map-elt r 'updatedAt)))
                                (ago (agent-shell-prompt-library--format-time-ago
                                      (or (map-elt r 'updatedAt) (map-elt r 'createdAt))))
                                (cand (format "#%s %s (%s) [%s]" id title short-sha b))
                                (ann (format "%s | %s | %s" conclusion dur ago)))
                           (list cand id ann)))
                       target-runs))
               (table (mapcar (lambda (item) (cons (nth 0 item) (nth 2 item))) items))
               (selected (if (fboundp 'annotated-completing-read)
                             (annotated-completing-read table
                                                        :prompt "Select CI Run: "
                                                        :require-match t
                                                        :history 'agent-shell-prompt-ci-run-history)
                           (completing-read "Select CI Run: " table nil t)))
               (match (assoc selected items)))
          (if match
              (nth 1 match)
            (user-error "No CI run selected")))))))

;; Local filesystem helpers for prompt artifacts

(defun agent-shell-prompt-library--project-root ()
  "Return the root directory of the current project or repository."
  (file-name-as-directory
   (expand-file-name
    (or (ignore-errors (vc-root-dir))
        (ignore-errors (locate-dominating-file default-directory ".git"))
        default-directory))))

(defun agent-shell-prompt-library--write-file (file-path content)
  "Write CONTENT string to FILE-PATH, creating parent directories as needed."
  (let ((dir (file-name-directory file-path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file file-path
    (insert (or content ""))))

(defun agent-shell-prompt-library--format-pr-comments-markdown (repo pr-num view-obj inline-comments raw-comments)
  "Format PR review comments into Markdown.
REPO is the repository slug string.  PR-NUM is the PR number string.
VIEW-OBJ is the parsed hash-table from `gh pr view --json ...`.
INLINE-COMMENTS is the parsed vector of hash-tables from `gh api ...`.
RAW-COMMENTS is fallback plain text from `gh pr view --comments`."
  (with-temp-buffer
    (let* ((title (if (hash-table-p view-obj) (or (gethash "title" view-obj) "") ""))
           (author-val (if (hash-table-p view-obj) (gethash "author" view-obj) nil))
           (author (cond ((hash-table-p author-val) (or (gethash "login" author-val) ""))
                         ((stringp author-val) author-val)
                         (t "")))
           (url (if (hash-table-p view-obj) (or (gethash "url" view-obj) "") ""))
           (reviews (if (hash-table-p view-obj) (gethash "reviews" view-obj) nil))
           (comments (if (hash-table-p view-obj) (gethash "comments" view-obj) nil))
           (inline inline-comments))
      (insert (format "# PR #%s Comments" pr-num))
      (unless (string-empty-p title)
        (insert (format ": %s" title)))
      (insert "\n\n")
      (when repo
        (insert (format "- **Repository**: %s\n" repo)))
      (unless (string-empty-p author)
        (insert (format "- **Author**: @%s\n" author)))
      (unless (string-empty-p url)
        (insert (format "- **URL**: %s\n" url)))
      (insert (format "- **Generated**: %s\n" (format-time-string "%Y-%m-%dT%T%z")))
      (insert (format "- **Reviews**: %d\n" (if (vectorp reviews) (length reviews) 0)))
      (insert (format "- **Top-level Comments**: %d\n" (if (vectorp comments) (length comments) 0)))
      (insert (format "- **Inline Comments**: %d\n\n" (if (vectorp inline) (length inline) 0)))

      ;; Reviews
      (when (and (vectorp reviews) (> (length reviews) 0))
        (insert (format "## Reviews (%d)\n\n" (length reviews)))
        (seq-doseq (r reviews)
          (let* ((u-obj (gethash "author" r))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (state (or (gethash "state" r) ""))
                 (submitted (or (gethash "submittedAt" r) ""))
                 (r-url (or (gethash "url" r) ""))
                 (body (or (gethash "body" r) "")))
            (unless (equal user "github-actions")
              (insert (format "### Review by @%s (%s)\n" (or user "unknown") state))
              (unless (string-empty-p submitted) (insert (format "- **Submitted**: %s\n" submitted)))
              (unless (string-empty-p r-url) (insert (format "- **URL**: %s\n" r-url)))
              (unless (string-empty-p body)
                (insert "\n**Body**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Comments
      (when (and (vectorp comments) (> (length comments) 0))
        (insert (format "## Top-level Comments (%d)\n\n" (length comments)))
        (seq-doseq (c comments)
          (let* ((u-obj (gethash "author" c))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (created (or (gethash "createdAt" c) ""))
                 (c-url (or (gethash "url" c) ""))
                 (body (or (gethash "body" c) "")))
            (unless (equal user "github-actions")
              (insert (format "### Comment by @%s\n" (or user "unknown")))
              (unless (string-empty-p created) (insert (format "- **At**: %s\n" created)))
              (unless (string-empty-p c-url) (insert (format "- **URL**: %s\n" c-url)))
              (unless (string-empty-p body)
                (insert "\n**Body**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Inline comments
      (when (and (vectorp inline) (> (length inline) 0))
        (insert (format "## Inline Review Comments (%d)\n\n" (length inline)))
        (seq-doseq (ic inline)
          (let* ((u-obj (gethash "user" ic))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (path (or (gethash "path" ic) ""))
                 (line (or (gethash "line" ic) (gethash "original_line" ic) 0))
                 (created (or (gethash "created_at" ic) ""))
                 (i-url (or (gethash "html_url" ic) (gethash "url" ic) ""))
                 (diff (or (gethash "diff_hunk" ic) ""))
                 (body (or (gethash "body" ic) "")))
            (unless (equal user "github-actions")
              (insert (format "### Inline Comment by @%s on `%s` (line %s)\n" (or user "unknown") path line))
              (insert (format "- **File**: `%s:%s`\n" path line))
              (unless (string-empty-p created) (insert (format "- **At**: %s\n" created)))
              (unless (string-empty-p i-url) (insert (format "- **URL**: %s\n" i-url)))
              (unless (string-empty-p diff)
                (insert "\n```diff\n")
                (insert diff)
                (insert "\n```\n"))
              (unless (string-empty-p body)
                (insert "\n**Comment**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Fallback when structured data is absent
      (when (and (or (null reviews) (= (length reviews) 0))
                 (or (null comments) (= (length comments) 0))
                 (or (null inline) (= (length inline) 0))
                 (and raw-comments (not (string-empty-p raw-comments))))
        (insert "## Comments\n\n")
        (insert raw-comments)
        (insert "\n"))

      (buffer-string))))

;; CI build failure remediation

(defun agent-shell-prompt-library--fix-ci-pre-op (ctx)
  "Fetch failing CI artifacts for :repo/:run-id in CTX and save them locally.
Artifacts (failed-step log, jobs metadata JSON, and triage index) are written
under <project-root>/.agent/fix-ci/ so the agent can inspect them as files."
  (let* ((args (plist-get ctx :args))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (magit-dash-repo-name r))))
                       (ignore-errors
                         (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
                           (unless (or (string-empty-p slug) (string-match-p "^error" slug))
                             slug)))
                       (user-error "No repository specified for fix-ci")))
         (repo-slug (agent-shell-prompt-library--resolve-repo-slug raw-repo))
         (run-id (or (plist-get args :run-id)
                     (agent-shell-prompt-library--resolve-ci-run repo-slug (plist-get args :branch))))
         (run-id-str (when run-id (format "%s" run-id)))
         (updated-args (plist-put (plist-put (copy-sequence args) :repo repo-slug) :run-id run-id))
         (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
    (if (and repo-slug run-id-str)
        (let* ((root (agent-shell-prompt-library--project-root))
               (ci-dir (expand-file-name ".agent/fix-ci" root))
               (log-file (expand-file-name (format "run-%s-logs.txt" run-id-str) ci-dir))
               (jobs-file (expand-file-name (format "run-%s-jobs.json" run-id-str) ci-dir))
               (index-file (expand-file-name "ci-triage-index.md" ci-dir))
               (alias-log-file (expand-file-name "ci-logs.txt" ci-dir))
               (alias-jobs-file (expand-file-name "ci-jobs.json" ci-dir))
               (rel-ci-dir (file-relative-name ci-dir root))
               (rel-log-file (file-relative-name log-file root))
               (rel-jobs-file (file-relative-name jobs-file root))
               (rel-index-file (file-relative-name index-file root))
               ;; Fetch run summary, failed logs, and job metadata
               (ci-summary (agent-shell-prompt-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug))
               (ci-log (agent-shell-prompt-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--log-failed"))
               (ci-jobs (agent-shell-prompt-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch"))
               (log-content (if (and (stringp ci-log) (not (string-empty-p ci-log)))
                                ci-log
                              (let ((full-log (agent-shell-prompt-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--log")))
                                (if (and (stringp full-log) (not (string-empty-p full-log)))
                                    full-log
                                  (format "No failed-step logs returned for run #%s.\n\nSummary:\n%s" run-id-str ci-summary)))))
               (jobs-content (if (and (stringp ci-jobs) (not (string-empty-p ci-jobs)))
                                 ci-jobs
                               "{}"))
               (index-content
                (format "# CI Triage Index\n\n- **Repository**: %s\n- **Run ID**: %s\n- **Generated**: %s\n- **Log File**: `%s`\n- **Jobs Metadata**: `%s`\n\n## Summary\n\n```\n%s\n```\n"
                        repo-slug run-id-str (format-time-string "%Y-%m-%dT%T%z") rel-log-file rel-jobs-file ci-summary)))
          ;; Write artifacts to local filesystem
          (agent-shell-prompt-library--write-file log-file log-content)
          (agent-shell-prompt-library--write-file jobs-file jobs-content)
          (agent-shell-prompt-library--write-file index-file index-content)
          (agent-shell-prompt-library--write-file alias-log-file log-content)
          (agent-shell-prompt-library--write-file alias-jobs-file jobs-content)
          ;; Populate context
          (setq updated-ctx (plist-put updated-ctx :ci-summary ci-summary))
          (setq updated-ctx (plist-put updated-ctx :ci-dir rel-ci-dir))
          (setq updated-ctx (plist-put updated-ctx :ci-log-file rel-log-file))
          (setq updated-ctx (plist-put updated-ctx :ci-jobs-file rel-jobs-file))
          (setq updated-ctx (plist-put updated-ctx :ci-index-file rel-index-file))
          (setq updated-ctx (plist-put updated-ctx :ci-log ci-log))
          updated-ctx)
      updated-ctx)))

(register-agent-shell-prompt fix-ci
  :doc "Download CI artifacts to local filesystem and prompt agent to fix build failure"
  :category "CI/CD"
  :args ((repo :prompt "Repository: " :optional t)
         (run-id :prompt "Run ID: " :type integer :optional t))
  :pre-op #'agent-shell-prompt-library--fix-ci-pre-op
  :template "Investigate and fix the CI failure in {{args.repo}} (run #{{args.run-id}}).

## CI Artifacts:
- Failed step log: `{{ci-log-file}}`
- Failing jobs metadata: `{{ci-jobs-file}}`
- CI triage index: `{{ci-index-file}}`

Do NOT read the entire log file into context. Inspect the logs as files using search or tail inspection (failures typically appear in the last 150-200 lines).

## Instructions:
1. Analyze failure in `{{ci-log-file}}` (search FAIL, errors, panics, or lint failures).
2. Write structured fix plan to `.agent/fix-ci/fix-plan.md` (root cause, action items, verification).
3. Present fix plan to user and ask confirmation before modifying source files.
4. Implement targeted fix and verify with narrow tests."
  :submit t
  :target :session-reuse)

;; PR review comment remediation

(defun agent-shell-prompt-library--pr-review-pre-op (ctx)
  "Fetch PR review comments for :pr-number in CTX and save them locally.
Markdown summary and JSON export are saved under
<project-root>/.agent/pr-comments/."
  (let* ((args (plist-get ctx :args))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (magit-dash-repo-name r))))
                       (ignore-errors
                         (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
                           (unless (or (string-empty-p slug) (string-match-p "^error" slug))
                             slug)))))
         (repo-slug (when raw-repo (agent-shell-prompt-library--resolve-repo-slug raw-repo)))
         (pr-arg (plist-get args :pr-number))
         (pr-number (or (and pr-arg (if (numberp pr-arg) pr-arg (string-to-number (format "%s" pr-arg))))
                        (ignore-errors
                          (let ((val (string-trim (shell-command-to-string "gh pr view --json number --jq .number"))))
                            (when (and val (not (string-empty-p val)) (string-match-p "^[0-9]+$" val))
                              (string-to-number val))))))
         (pr-str (if pr-number (format "%s" pr-number)
                   (user-error "No PR number specified or detected for pr-review-patch")))
         (root (agent-shell-prompt-library--project-root))
         (pr-dir (expand-file-name ".agent/pr-comments" root))
         (md-file (expand-file-name (format "pr-%s-comments.md" pr-str) pr-dir))
         (json-file (expand-file-name (format "pr-%s-comments.json" pr-str) pr-dir))
         (alias-md-file (expand-file-name "pr-comments.md" pr-dir))
         (alias-json-file (expand-file-name "pr-comments.json" pr-dir))
         (rel-pr-dir (file-relative-name pr-dir root))
         (rel-md-file (file-relative-name md-file root))
         (rel-json-file (file-relative-name json-file root))
         (repo-args (if repo-slug (list "--repo" repo-slug) nil))
         ;; Fetch structured review info
         (view-json-raw
          (apply #'agent-shell-prompt-library--shell
                 "gh" "pr" "view" pr-str "--json"
                 "number,title,author,url,reviews,comments"
                 repo-args))
         ;; Fetch inline review comments
         (api-json-raw
          (when repo-slug
            (agent-shell-prompt-library--shell
             "gh" "api" (format "repos/%s/pulls/%s/comments" repo-slug pr-str)
             "--paginate")))
         ;; Fetch raw formatted comments fallback
         (raw-comments
          (apply #'agent-shell-prompt-library--shell
                 "gh" "pr" "view" pr-str "--comments"
                 repo-args))
         ;; Parse JSON responses
         (view-obj (ignore-errors
                     (json-parse-string view-json-raw :object-type 'hash-table :array-type 'array)))
         (api-arr (ignore-errors
                    (when (and api-json-raw (not (string-empty-p api-json-raw)))
                      (json-parse-string api-json-raw :object-type 'hash-table :array-type 'array))))
         ;; Render Markdown
         (md-content (agent-shell-prompt-library--format-pr-comments-markdown
                      repo-slug pr-str view-obj api-arr raw-comments))
         ;; Build structured JSON
         (json-content
          (if (hash-table-p view-obj)
              (let ((table (make-hash-table :test #'equal)))
                (puthash "pr" (or pr-number (string-to-number pr-str)) table)
                (when repo-slug (puthash "repo" repo-slug table))
                (when-let* ((t-val (gethash "title" view-obj))) (puthash "title" t-val table))
                (when-let* ((u-val (gethash "url" view-obj))) (puthash "url" u-val table))
                (puthash "reviews" (or (gethash "reviews" view-obj) []) table)
                (puthash "comments" (or (gethash "comments" view-obj) []) table)
                (puthash "inline_comments" (or api-arr []) table)
                (or (ignore-errors (json-serialize table)) "{}"))
            (if (and view-json-raw (not (string-empty-p view-json-raw)))
                view-json-raw
              "{}")))
         ;; Summary string
         (pr-summary
          (if (hash-table-p view-obj)
              (let* ((title (or (gethash "title" view-obj) ""))
                     (author-obj (gethash "author" view-obj))
                     (author (if (hash-table-p author-obj) (gethash "login" author-obj) (format "%s" (or author-obj ""))))
                     (reviews (gethash "reviews" view-obj))
                     (comments (gethash "comments" view-obj))
                     (inline api-arr))
                (format "PR #%s: %s (by @%s) — %d review(s), %d comment(s), %d inline comment(s)"
                        pr-str title author
                        (if (vectorp reviews) (length reviews) 0)
                        (if (vectorp comments) (length comments) 0)
                        (if (vectorp inline) (length inline) 0)))
            (format "PR #%s in %s" pr-str (or repo-slug "repository")))))
    ;; Write artifacts to disk
    (agent-shell-prompt-library--write-file md-file md-content)
    (agent-shell-prompt-library--write-file json-file (or json-content "{}"))
    (agent-shell-prompt-library--write-file alias-md-file md-content)
    (agent-shell-prompt-library--write-file alias-json-file (or json-content "{}"))
    ;; Populate context
    (let* ((updated-args (plist-put (copy-sequence args) :pr-number (or pr-number (string-to-number pr-str))))
           (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
      (when repo-slug
        (setq updated-args (plist-put updated-args :repo repo-slug))
        (setq updated-ctx (plist-put updated-ctx :args updated-args)))
      (setq updated-ctx (plist-put updated-ctx :pr-summary pr-summary))
      (setq updated-ctx (plist-put updated-ctx :pr-dir rel-pr-dir))
      (setq updated-ctx (plist-put updated-ctx :pr-comments-file rel-md-file))
      (setq updated-ctx (plist-put updated-ctx :pr-comments-json-file rel-json-file))
      (setq updated-ctx (plist-put updated-ctx :pr-comments raw-comments))
      updated-ctx)))

(register-agent-shell-prompt pr-review-patch
  :doc "Fetch PR review comments to local filesystem and draft remediation patch"
  :category "Code Review"
  :args ((pr-number :prompt "PR number: " :type integer :optional t)
         (repo :prompt "Repository: " :optional t))
  :pre-op #'agent-shell-prompt-library--pr-review-pre-op
  :template "Address the review comments on PR #{{args.pr-number}} in {{args.repo}}.

## PR Summary:
{{pr-summary}}

## Review Comments:
- Markdown summary: `{{pr-comments-file}}`
- Structured JSON: `{{pr-comments-json-file}}`

Do NOT read all raw comment data into context at once. Review comments in `{{pr-comments-file}}` using search or file viewing.

## Instructions:
1. Categorize comments in `{{pr-comments-file}}` (change-required, question, nit, praise, discussion, resolved).
2. Write review plan to `.agent/pr-comments/review-plan.md` (proposed changes, reviewer replies, user questions).
3. Present summary counts and discussion items to user for confirmation.
4. Apply confirmed changes and verify tests pass."
  :submit t
  :target :session-reuse)

;; Test coverage expansion

(defun agent-shell-prompt-library--coverage-pre-op (ctx)
  "Diff :file in CTX against HEAD to scope untested edits."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (diff (agent-shell-prompt-library--diff-summary (list "HEAD" "--" file) 5)))
    (plist-put (copy-sequence ctx) :file-diff diff)))

(register-agent-shell-prompt expand-coverage
  :doc "Analyze uncovered lines and author missing unit tests"
  :category "Testing"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--coverage-pre-op
  :template "Review {{args.file}} for untested logic and author missing unit tests.

Diff:
{{file-diff}}"
  :submit t
  :target :session-reuse)

;; Refactor / dead-code cleanup

(defun agent-shell-prompt-library--refactor-pre-op (ctx)
  "Gather git log summary for :file in CTX to scope stale/legacy code."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (log (agent-shell-prompt-library--git-output "log" "--oneline" "-n" "5" "--" file)))
    (plist-put (copy-sequence ctx) :recent-history (if (string-empty-p log) "(no history)" log))))

(register-agent-shell-prompt refactor-module
  :doc "Clean up dead code and migrate legacy macro forms"
  :category "Refactoring"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--refactor-pre-op
  :template "Refactor {{args.file}}: remove dead code and migrate legacy forms to current conventions.

Recent history:
{{recent-history}}"
  :submit t
  :target :session-reuse)

;; Git commit authoring

(defun agent-shell-prompt-library--create-commit-pre-op (ctx)
  "Gather git status, diff against HEAD, and recent log history for CTX.
Uses Magit or Git directly to gather state."
  (let* ((args (plist-get ctx :args))
         (files (plist-get args :files))
         (has-files (and (stringp files) (not (string-empty-p files))))
         (file-args (when has-files (list "--" files)))
         (status (apply #'agent-shell-prompt-library--git-output
                        (append '("status" "--short") file-args)))
         (diff (agent-shell-prompt-library--diff-summary
                (append '("HEAD") file-args) 5))
         (log (agent-shell-prompt-library--git-output "log" "--oneline" "-n" "5"))
         (updated-ctx (copy-sequence ctx)))
    (setq updated-ctx (plist-put updated-ctx :git-status (if (string-empty-p status) "(clean)" status)))
    (setq updated-ctx (plist-put updated-ctx :git-diff diff))
    (setq updated-ctx (plist-put updated-ctx :recent-log (if (string-empty-p log) "(no history)" log)))
    updated-ctx))

(register-agent-shell-prompt create-commit
  :doc "Draft and create a git commit with concise message and attribution"
  :category "Git"
  :args ((files :prompt "Files to commit (optional): " :optional t)
         (instructions :prompt "Additional instructions (optional): " :optional t))
  :pre-op #'agent-shell-prompt-library--create-commit-pre-op
  :template "Create a git commit for the current changes:

## Working Tree Status:
{{git-status}}

## Current Diff:
{{git-diff}}

## Recent Commits (for style reference):
{{recent-log}}

{{args.instructions}}

## Guidelines:
- Subject: Imperative, concise sentence matching repository conventions.
- Body: 2-3 sentences explaining motivation/impact (omit if self-explanatory).
- Attribution: Include `Co-authored-by:` trailer identifying the AI assistant.
- Stage appropriate changes and commit."
  :submit t
  :target :session-reuse)

(provide 'agent-shell-prompt-library)

;;; agent-shell-prompt-library.el ends here
