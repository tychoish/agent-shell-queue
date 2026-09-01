;;; agent-shell-prompt-library.el --- Built-in agent-shell-prompt workflows -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (agent-shell-prompt "0.1"))

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

;; Standard `agent-shell-prompt-def' registrations: CI failure
;; remediation, PR review patching, coverage expansion, and refactor
;; cleanup.  Each pre-op is deterministic Elisp gathering exact context
;; via `gh' or `git' rather than letting the agent hallucinate it.

;;; Code:

(require 'agent-shell-prompt)

(defun agent-shell-prompt-library--shell (&rest args)
  "Run ARGS as a shell command in `default-directory' and return its output.
Trailing newline is trimmed.  Errors are captured inline in the result
rather than signaled, so a pre-op can surface tool failures to the agent
instead of aborting the workflow."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'call-process (car args) nil t nil (cdr args))))))

;; CI failure remediation

(defun agent-shell-prompt-library--fix-ci-pre-op (ctx)
  "Fetch the failing CI run's summary and log for :repo/:run-id in CTX."
  (let* ((args (plist-get ctx :args))
         (repo (plist-get args :repo))
         (run-id (plist-get args :run-id))
         (summary (agent-shell-prompt-library--shell
                   "gh" "run" "view" (format "%s" run-id) "--repo" repo))
         (log (agent-shell-prompt-library--shell
               "gh" "run" "view" (format "%s" run-id) "--repo" repo "--log-failed")))
    (plist-put ctx :ci-summary summary)
    (plist-put ctx :ci-log log)))

(agent-shell-prompt-def fix-ci
  :doc "Download CI artifacts and prompt agent to fix build failure"
  :category "CI/CD"
  :args ((repo :prompt "Repository: ")
         (run-id :prompt "Run ID: " :type integer))
  :pre-op #'agent-shell-prompt-library--fix-ci-pre-op
  :template "Investigate and fix the CI failure in {{args.repo}} (run #{{args.run-id}}).\n\nSummary:\n{{ci-summary}}\n\nFailed step log:\n{{ci-log}}"
  :submit t
  :target :session-reuse)

;; PR review comment remediation

(defun agent-shell-prompt-library--pr-review-pre-op (ctx)
  "Fetch review comments for :pr-number in CTX."
  (let* ((args (plist-get ctx :args))
         (pr-number (plist-get args :pr-number))
         (comments (agent-shell-prompt-library--shell
                    "gh" "pr" "view" (format "%s" pr-number) "--comments")))
    (plist-put ctx :pr-comments comments)))

(agent-shell-prompt-def pr-review-patch
  :doc "Fetch PR review comments and draft remediation patch"
  :category "Code Review"
  :args ((pr-number :prompt "PR number: " :type integer))
  :pre-op #'agent-shell-prompt-library--pr-review-pre-op
  :template "Address the review comments on PR #{{args.pr-number}}.\n\nComments:\n{{pr-comments}}"
  :submit t
  :target :session-reuse)

;; Test coverage expansion

(defun agent-shell-prompt-library--coverage-pre-op (ctx)
  "Compute a diff of :file in CTX against HEAD to scope untested changes."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (diff (agent-shell-prompt-library--shell "git" "diff" "HEAD" "--" file)))
    (plist-put ctx :file-diff diff)))

(agent-shell-prompt-def expand-coverage
  :doc "Analyze uncovered lines and author missing unit tests"
  :category "Testing"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--coverage-pre-op
  :template "Review {{args.file}} for untested logic and add missing unit tests.\n\nUncommitted diff for context:\n{{file-diff}}"
  :submit t
  :target :session-reuse)

;; Refactor / dead-code cleanup

(defun agent-shell-prompt-library--refactor-pre-op (ctx)
  "Gather git blame summary for :file in CTX to scope stale/legacy code."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (blame (agent-shell-prompt-library--shell "git" "log" "--oneline" "-n" "10" "--" file)))
    (plist-put ctx :recent-history blame)))

(agent-shell-prompt-def refactor-module
  :doc "Clean up dead code and migrate legacy macro forms"
  :category "Refactoring"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-prompt-library--refactor-pre-op
  :template "Refactor {{args.file}}: remove dead code and migrate legacy forms to current conventions.\n\nRecent history:\n{{recent-history}}"
  :submit t
  :target :session-reuse)

(provide 'agent-shell-prompt-library)

;;; agent-shell-prompt-library.el ends here
