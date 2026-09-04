;;; agent-shell-prompt-menu.el --- ACR picker for agent-shell-prompt -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell-prompt "0.1") (annotated-completing-read "0.1"))

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

;; The registry is elastic and often site/config-specific, so the
;; interactive surface is a single `annotated-completing-read' picker
;; (`agent-shell-prompt-select') rather than a bespoke transient menu
;; per prompt.  `agent-shell-prompt-dispatch-menu' wires exactly one
;; entry point into `agent-shell-menu-dispatch' that calls the picker.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'annotated-completing-read)
(require 'agent-shell-prompt)

(defun agent-shell-prompt--candidates (&optional category)
  "Return registered prompt specs, filtered to CATEGORY when non-nil."
  (let ((specs (agent-shell-prompt-list)))
    (if category
        (seq-filter (lambda (s) (equal (agent-shell-prompt-spec-category s) category)) specs)
      specs)))

;;;###autoload
(defun agent-shell-prompt-select (&optional category)
  "Pick a registered prompt via `annotated-completing-read' and dispatch it.
Candidates are annotated with their category and one-line doc.  With
CATEGORY non-nil, only prompts in that category are offered."
  (interactive)
  (let* ((specs (or (agent-shell-prompt--candidates category)
                    (user-error "No prompts registered")))
         (table (seq-map
                 (lambda (spec)
                   (cons (symbol-name (agent-shell-prompt-spec-id spec))
                         (cons (format "[%s] %s"
                                      (agent-shell-prompt-spec-category spec)
                                      (or (agent-shell-prompt-spec-doc spec) ""))
                               spec)))
                 specs))
         (selected (annotated-completing-read
                    table
                    :prompt "Select prompt: "
                    :category 'agent-shell-prompt
                    :require-match t
                    :history 'agent-shell-prompt-select)))
    (agent-shell-prompt-dispatch (agent-shell-prompt-spec-id selected))))

;;;###autoload
(transient-define-prefix agent-shell-prompt-dispatch-menu ()
  "Single entry point into the `agent-shell-prompt' ACR picker."
  ["Prompt Library"
   ("p" "Select prompt…" agent-shell-prompt-select)])

;; Wire a single entry into agent-shell-menu-dispatch's Actions column,
;; rather than duplicating per-prompt transient suffixes there — see
;; `agent-shell-prompt-select'.
(declare-function agent-shell-menu-dispatch "agent-shell-menu")
(with-eval-after-load 'agent-shell-menu
  (transient-append-suffix 'agent-shell-menu-dispatch "ac"
    '("ap" "Prompt library" agent-shell-prompt-select)))

(provide 'agent-shell-prompt-menu)

;;; agent-shell-prompt-menu.el ends here
