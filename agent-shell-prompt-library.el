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

;; Standard `register-agent-shell-prompt' registrations: CI failure
;; remediation, PR review patching, coverage expansion, refactor
;; cleanup, and git commit authoring.  Each pre-op is deterministic Elisp
;; gathering exact context via `gh' or `git' (using Magit when available)
;; rather than letting the agent hallucinate it.

;;; Code:

(require 'agent-shell-prompt)

(declare-function magit-git-output "magit-git" (&rest args))
(declare-function magit-get-current-branch "magit-git" ())
(declare-function magit-dash-gh--repo-info "magit-dash" ())
(declare-function magit-dash--repo-at-point "magit-dash" ())
(declare-function magit-dash-repo-name "magit-dash" (repo))
(declare-function annotated-completing-read "annotated-completing-read")

(defun agent-shell-prompt-library--shell (&rest args)
  "Run ARGS as a shell command in `default-directory' and return its output.
Trailing newline is trimmed.  Errors are captured inline in the result
rather than signaled, so a pre-op can surface tool failures to the agent
instead of aborting the workflow."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'call-process (car args) nil t nil (cdr args))))))

(defun agent-shell-prompt-library--git-output (&rest args)
  "Run git with ARGS in `default-directory' and return trimmed output.
Uses `magit-git-output' when available, falling back to
`agent-shell-prompt-library--shell'."
  (if (fboundp 'magit-git-output)
      (string-trim (or (apply #'magit-git-output args) ""))
    (apply #'agent-shell-prompt-library--shell "git" args)))

(defun agent-shell-prompt-library--gather (ctx pairs)
  "Populate CTX with the output of each shell command in PAIRS.
PAIRS is a list of (CTX-KEY COMMAND ARG...) entries; each COMMAND is run
via `agent-shell-prompt-library--shell' and stored under CTX-KEY."
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
        (let ((diff (max 0 (floor (- e s)))))(cond ((< diff 60)
           (format "%ds" diff))
                ((< diff 3600) (format "%dm %ds" (/ diff 60) (% diff 60)))
                (t (format "%dh %dm" (/ diff 3600) (% (% diff 3600) 60)))))
      "n/a")))

(defun agent-shell-prompt-library--format-time-ago (iso-time)
  "Format ISO-TIME string as relative time ago."
  (let ((t-sec (agent-shell-prompt-library--iso-to-seconds iso-time)))
    (if t-sec
        (let ((diff (max 0 (floor (- (float-time) t-sec)))))(cond ((< diff 60)
           "just now")
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
  "Fetch recent GitHub Action runs for REPO as a list of alists."
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
  "Return current git branch name or `main'."
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
Otherwise, prompt the user with an ACR picker showing recent runs with duration and time ago."
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

(defun agent-shell-prompt-library--fix-ci-pre-op (ctx)
  "Fetch the failing CI run's summary and log for :repo/:run-id in CTX."
  (let* ((args (plist-get ctx :args))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (magit-dash-repo-name r))))
                       (user-error "No repository specified for fix-ci")))
         (repo-slug (agent-shell-prompt-library--resolve-repo-slug raw-repo))
         (run-id (or (plist-get args :run-id)
                     (agent-shell-prompt-library--resolve-ci-run repo-slug (plist-get args :branch))))
         (run-id-str (when run-id (format "%s" run-id)))
         (updated-args (plist-put (plist-put (copy-sequence args) :repo repo-slug) :run-id run-id))
         (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
    (if (and repo-slug run-id-str)
        (agent-shell-prompt-library--gather
         updated-ctx
         (list (list :ci-summary "gh" "run" "view" run-id-str "--repo" repo-slug)
               (list :ci-log "gh" "run" "view" run-id-str "--repo" repo-slug "--log-failed")))
      updated-ctx)))

(register-agent-shell-prompt fix-ci
  :doc "Download CI artifacts and prompt agent to fix build failure"
  :category "CI/CD"
  :args ((repo :prompt "Repository: " :optional t)
         (run-id :prompt "Run ID: " :type integer :optional t))
  :pre-op #'agent-shell-prompt-library--fix-ci-pre-op
  :template "Investigate and fix the CI failure in {{args.repo}} (run #{{args.run-id}}).\n\nSummary:\n{{ci-summary}}\n\nFailed step log:\n{{ci-log}}"
  :submit t
  :target :session-reuse)

;; PR review comment remediation

(defun agent-shell-prompt-library--pr-review-pre-op (ctx)
  "Fetch review comments for :pr-number in CTX."
  (let* ((args (plist-get ctx :args))
         (pr-number (format "%s" (plist-get args :pr-number))))
    (agent-shell-prompt-library--gather
     ctx (list (list :pr-comments "gh" "pr" "view" pr-number "--comments")))))

(register-agent-shell-prompt pr-review-patch
  :doc "Fetch PR review comments and draft remediation patch"
  :category "Code Review"
  :args ((pr-number :prompt "PR number: " :type integer))
  :pre-op #'agent-shell-prompt-library--pr-review-pre-op
  :template "Address the review comments on PR #{{args.pr-number}}.\n\nComments:\n{{pr-comments}}"
  :submit t
  :target :session-reuse)

;; Test coverage expansion

(defun agent-shell-prompt-library--coverage-pre-op (ctx)
  "Diff :file in CTX against HEAD to scope untested edits."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file)))
    (agent-shell-prompt-library--gather
     ctx (list (list :file-diff "git" "diff" "HEAD" "--" file)))))

(register-agent-shell-prompt expand-coverage
  :doc "Analyze uncovered lines and author missing unit tests"
  :category "Testing"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--coverage-pre-op
  :template "Review {{args.file}} for untested logic and add missing unit tests.\n\nUncommitted diff for context:\n{{file-diff}}"
  :submit t
  :target :session-reuse)

;; Refactor / dead-code cleanup

(defun agent-shell-prompt-library--refactor-pre-op (ctx)
  "Gather git log summary for :file in CTX to scope stale/legacy code."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file)))
    (agent-shell-prompt-library--gather
     ctx (list (list :recent-history "git" "log" "--oneline" "-n" "10" "--" file)))))

(register-agent-shell-prompt refactor-module
  :doc "Clean up dead code and migrate legacy macro forms"
  :category "Refactoring"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--refactor-pre-op
  :template "Refactor {{args.file}}: remove dead code and migrate legacy forms to current conventions.\n\nRecent history:\n{{recent-history}}"
  :submit t
  :target :session-reuse)

;; Git commit authoring

(defun agent-shell-prompt-library--create-commit-pre-op (ctx)
  "Gather git status, diff against HEAD, and recent log history for CTX using Magit/Git."
  (let* ((args (plist-get ctx :args))
         (files (plist-get args :files))
         (has-files (and (stringp files) (not (string-empty-p files))))
         (file-args (when has-files (list "--" files)))
         (status (apply #'agent-shell-prompt-library--git-output
                        (append '("status" "--short") file-args)))
         (diff (apply #'agent-shell-prompt-library--git-output
                      (append '("diff" "HEAD") file-args)))
         (log (agent-shell-prompt-library--git-output "log" "--oneline" "-n" "5"))
         (updated-ctx (copy-sequence ctx)))
    (setq updated-ctx (plist-put updated-ctx :git-status status))
    (setq updated-ctx (plist-put updated-ctx :git-diff diff))
    (setq updated-ctx (plist-put updated-ctx :recent-log log))
    updated-ctx))

(register-agent-shell-prompt create-commit
  :doc "Draft and create a git commit with concise message and attribution"
  :category "Git"
  :args ((files :prompt "Files to commit (optional): " :optional t)
         (instructions :prompt "Additional instructions (optional): " :optional t))
  :pre-op #'agent-shell-prompt-library--create-commit-pre-op
  :template "Review the working tree changes and create a git commit following these guidelines:

## Commit Message Style
- **Subject line**: One short, direct sentence stating what changed (imperative mood, concise, matching repository conventions).
- **Body** (when needed): Focus exclusively on design decisions and motivations — the *why* behind the patch, not a re-description of what the diff already shows.
- **Omit body when self-explanatory**: If the subject line is sufficient, omit the body entirely.
- **Keep body terse**: When included, keep it to 2-3 sentences (under 100 words), covering motivation and potential impact (what could break, behavior changes for users/callers).
- **Style consistency**: Match the formatting, prefixing, and casing conventions shown in recent commit history.

## Attribution & Author Identity
- The commit author must be the human directing the session (their configured git identity).
- Include a `Co-authored-by:` trailer identifying the AI assistant, separated from the body by a blank line.

## Working Tree Status:
{{git-status}}

## Current Diff:
{{git-diff}}

## Recent Commit History (for style reference):
{{recent-log}}

{{args.instructions}}

Stage the appropriate changes and create the commit."
  :submit t
  :target :session-reuse)

(provide 'agent-shell-prompt-library)

;;; agent-shell-prompt-library.el ends here
