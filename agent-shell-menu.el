;;; agent-shell-menu.el --- ACR menus and transient prefixes for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell "0.1"))

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

;; ACR-based interactive menus for agent-shell sessions.  Provides transient
;; prefix menus `agent-shell-dispatch' and `agent-shell-session-menu' for
;; navigating and controlling agent sessions.  Covers permission resolution,
;; action selection, command selection, and collapse control.

;;; Code:

(require 'cl-lib)
(require 'annotated-completing-read)
(require 'transient)
(require 'agent-shell)
(require 'agent-shell-queue)

(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell--new-shell "agent-shell")
(declare-function agent-shell-ui--toggle-fragment-at-point "agent-shell-ui")

(declare-function agent-review "agent-review")
(declare-function agent-review-send-to-agent-shell "agent-review")

(defvar agent-shell-action-alist
  '(;; Shell interaction — only relevant when a session is reachable
    ("submit" . (shell-maker-submit . agent-shell-menu--in-session-p))
    ("interrupt" . (agent-shell-interrupt . agent-shell-menu--in-session-p))
    ("compose in viewport" . (agent-shell-prompt-compose . agent-shell-menu--in-session-p))
    ;; Navigation
    ("jump to end (prompt)" . (end-of-buffer . agent-shell-menu--in-session-p))
    ("goto last interaction" . (agent-shell-menu--goto-last-interaction . agent-shell-menu--in-session-p))
    ("next item" . (agent-shell-next-item . agent-shell-menu--in-session-p))
    ("previous item" . (agent-shell-previous-item . agent-shell-menu--in-session-p))
    ("other buffer (viewport)" . (agent-shell-other-buffer . agent-shell-menu--in-session-p))
    ;; Permissions — only when a permission is pending
    ("jump to permission row" . (agent-shell-jump-to-latest-permission-button-row . agent-shell--session-permission-pending-p))
    ("next permission button" . (agent-shell-next-permission-button . agent-shell--session-permission-pending-p))
    ("previous permission button" . (agent-shell-previous-permission-button . agent-shell--session-permission-pending-p))
    ;; Session switching — always available
    ("switch agent-shell" . agent-shell-switch-buffer)
    ;; Send content — needs a session target
    ("send region" . (agent-shell-send-region . agent-shell-menu--in-session-p))
    ("send file" . (agent-shell-send-file . agent-shell-menu--in-session-p))
    ("send file (pick)" . (agent-shell-menu-send-file . agent-shell-menu--in-session-p))
    ("send buffer" . (agent-shell-menu-send-buffer . agent-shell-menu--in-session-p))
    ("yank (DWIM)" . (agent-shell-yank-dwim . agent-shell-menu--in-session-p))
    ;; Session settings — need a session
    ("cycle session mode" . (agent-shell-cycle-session-mode . agent-shell-menu--in-session-p))
    ("set session mode" . (agent-shell-set-session-mode . agent-shell-menu--in-session-p))
    ("set session model" . (agent-shell-set-session-model . agent-shell-menu--in-session-p))
    ("copy session id" . (agent-shell-copy-session-id . agent-shell-menu--in-session-p))
    ("open transcript" . (agent-shell-open-transcript . agent-shell-menu--in-session-p))
    ("session info" . (agent-shell-session-info . agent-shell-menu--in-session-p))
    ("collapse menu" . (agent-shell-select-collapse . agent-shell-menu--in-session-p))
    ;; Queue actions
    ("interject" . (agent-shell-queue-interject . agent-shell-queue-interject-available-p))
    ("queue request" . agent-shell-queue-enqueue)
    ("queue capture" . agent-shell-queue-capture)
    ("capture unassigned" . agent-shell-queue-capture-unassigned)
    ("capture from region" . agent-shell-queue-capture-from-region)
    ("capture from clipboard" . agent-shell-queue-capture-from-clipboard)
    ("capture from context" . agent-shell-queue-capture-from-context)
    ("queue clear" . agent-shell-queue-enqueue-clear)
    ("queue review" . agent-shell-queue-buffer-open)
    ("set input mode" . (agent-shell-queue-toggle-input-mode . agent-shell-menu--in-shell-p))
    ("set input mode default" . agent-shell-queue-set-input-mode-default)
    ("reset all sessions to default" . agent-shell-queue-reset-all-input-modes)
    ("review changes" . (agent-review . agent-shell-menu--agent-review-available-p))
    ("send review issues to shell" . (agent-review-send-to-agent-shell . agent-shell-menu--agent-review-available-p)))
  "Alist mapping label strings to commands for `agent-shell-select-action'.
Each entry is either (LABEL . COMMAND) or (LABEL . (COMMAND . PREDICATE)).
When a PREDICATE is supplied it is called with no arguments; the entry is
omitted from the menu when the predicate returns nil.")

;;; Key binding

(defmacro agent-shell-mode-key (key fn)
  "Define `agent-shell-output-key-KEY' and bind it in `agent-shell-mode-map'.
In the output area, or while the shell is busy, calls FN interactively.
Self-inserts KEY only when at the idle prompt, unless queue-only mode is active
(in which case routes to `agent-shell-queue-ready-capture' instead).
Also binds FN directly in `agent-shell-viewport-view-mode-map'."
  (let* ((key-str (if (stringp key) key (symbol-name key)))
         (name (intern (concat "agent-shell-output-key-" key-str)))
         (char (pcase key-str
                 ("TAB" ?\t)
                 ((pred (lambda (s) (= 1 (length s)))) (aref key-str 0)))))
    `(progn
       (defun ,name ()
         ,(format "In output or busy: `%s'. Self-insert at idle prompt." fn)
         (interactive)
         (if (and (not (shell-maker-busy)) (shell-maker-point-at-last-prompt-p))
             ,(if char
                  `(if (bound-and-true-p agent-shell-queue-only-mode)
                       (agent-shell-queue-ready-capture)
                     (self-insert-command 1 ,char))
                '(ignore))
           (call-interactively #',fn)))
       (define-key agent-shell-mode-map (kbd ,key-str) #',name)
       (with-eval-after-load 'agent-shell-viewport
         (define-key agent-shell-viewport-view-mode-map (kbd ,key-str) #',fn)))))

;;; Buffer/session management

(defun agent-shell--buffer-annotation (buf)
  "Build an annotation string describing the agent-shell BUF."
  (with-current-buffer buf
    (let* ((status (agent-shell-status))
	   (state agent-shell--state)
	   (used (map-nested-elt state '(:usage :context-used)))
	   (size (map-nested-elt state '(:usage :context-size)))
	   (last (map-elt state :last-activity-time))
	   (cwd (abbreviate-file-name (or default-directory ""))))
      (mapconcat 'identity
		 (seq-remove #'null
		  (list (format "[%s]" status)
			cwd
			(when (and (numberp used) (numberp size) (> size 0))
			  (format "ctx %.0f%%" (* 100.0 (/ (float used) size))))
			(when last
			  (format "%s ago"
				  (agent-shell-queue--format-age (time-since last))))))
		 " · "))))

(defun agent-shell-extras--pick-buffer (prompt)
  "Select an agent-shell buffer via PROMPT using status/cwd/context annotations."
  (let ((bufs (or (agent-shell-buffers) (user-error "No live agent-shell buffers"))))
    (get-buffer
     (annotated-completing-read
      (thread-last
	bufs
	(seq-map (lambda (buf) (cons (buffer-name buf) (agent-shell--buffer-annotation buf)))))
      :prompt prompt
      :category 'agent-shell-buffer
      :require-match t
      :history 'agent-shell-extras--pick-buffer))))

;;;###autoload
(defun agent-shell-switch-buffer ()
  "Switch to an agent-shell buffer with status, cwd, context, and age annotations."
  (interactive)
  (switch-to-buffer (agent-shell-extras--pick-buffer "agent-shell =>")))

;;; Permission resolution

(defun agent-shell--permission-buttons ()
  "Return a list of (LABEL . POSITION) for each pending permission button.
LABEL is the visible button text trimmed of surrounding brackets/whitespace.
POSITION is buffer position of the button's start."
  (let (out)
    (save-excursion
      (goto-char (point-min))
      (let (match)
	(while (setq match (text-property-search-forward 'button 'permission t))
	  (let* ((beg (prop-match-beginning match))
		 (end (prop-match-end match))
		 (text (buffer-substring-no-properties beg end))
		 (label (string-trim text "[][ \t\n\r]+" "[][ \t\n\r]+")))
	    (push (cons label beg) out)))))
    (nreverse out)))

(defun agent-shell--permission-action-at (position)
  "Return the RET command bound on the permission button at POSITION."
  (when-let ((keymap (get-text-property position 'keymap)))
    (lookup-key keymap (kbd "RET"))))

(defun agent-shell--permission-button-action (pos)
  "Return an interactive command that activates the permission button at POS."
  (lambda ()
    (interactive)
    (when-let* ((cmd (agent-shell--permission-action-at pos))
                ((functionp cmd))
                ((commandp cmd)))
      (save-excursion
	(goto-char pos)
	(call-interactively cmd)))))

;;;###autoload
(defun agent-shell-resolve-permission ()
  "Resolve a pending permission prompt via `annotated-completing-read'."
  (interactive)

  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))

  (unless (agent-shell--permission-pending-p)
    (user-error "No pending permission request in this buffer"))

  (let* ((buttons (or (agent-shell--permission-buttons)
                      (user-error "No permission buttons found in this buffer")))
         (label (annotated-completing-read
		 buttons
                 :prompt "permission => "
                 :category 'agent-shell-permission
                 :require-match t
                 :history 'agent-shell-resolve-permission))
         (pos (cdr (assoc label buttons)))
         (cmd (or (and pos (agent-shell--permission-action-at pos))
                  (user-error "No action attached to permission button"))))

    (save-excursion
      (goto-char pos)
      (call-interactively cmd))))

;;; Transient permission group helpers

(defun agent-shell--session-shell-buffer ()
  "Return the agent-shell buffer for the current window context.
Works from both agent-shell buffers and viewport buffers."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((agent-shell-viewport--shell-buffer))))

(defun agent-shell--session-permission-pending-p ()
  "Return non-nil when a permission is pending in the relevant shell buffer."
  (when-let* ((shell (agent-shell--session-shell-buffer)))
    (agent-shell--permission-pending-p :shell-buffer shell)))

(defun agent-shell-menu--in-session-p ()
  "Return non-nil when the current context has an associated agent-shell session."
  (not (null (agent-shell--session-shell-buffer))))

(defun agent-shell-menu--in-shell-p ()
  "Return non-nil when currently in an agent-shell buffer (not a viewport)."
  (derived-mode-p 'agent-shell-mode))

(defun agent-shell--session-permission-button-action (shell-buf pos)
  "Return an interactive command that activates the permission button at POS in SHELL-BUF."
  (lambda ()
    (interactive)
    (with-current-buffer shell-buf
      (when-let* ((cmd (agent-shell--permission-action-at pos))
                  ((functionp cmd))
                  ((commandp cmd)))
        (save-excursion
          (goto-char pos)
          (call-interactively cmd))))))

(defun agent-shell--permission-suffixes-for (prefix)
  "Return transient suffixes for each pending permission button under PREFIX.
Keys are assigned as 1, 2, 3… in button order."
  (when-let* ((shell (agent-shell--session-shell-buffer))
              (buttons (with-current-buffer shell
                         (agent-shell--permission-buttons))))
    (seq-map-indexed
     (lambda (btn i)
       (transient-parse-suffix
        prefix
        (list (number-to-string (1+ i))
              (format "Permission: %s" (car btn))
              (agent-shell--session-permission-button-action shell (cdr btn)))))
     buttons)))

(defun agent-shell--permission-suffixes (_group)
  "Return transient suffixes for each pending permission button.
Keys are assigned as 1, 2, 3… in button order."
  (agent-shell--permission-suffixes-for 'agent-shell-dispatch))

;;; Action menu

(defun agent-shell-menu--action-entry-command (entry)
  "Return the command for ENTRY.
ENTRY cdr may be a plain COMMAND symbol, a function, or a (COMMAND . PREDICATE) cons."
  (let ((val (cdr entry)))
    (if (and (consp val) (not (functionp val))) (car val) val)))

(defun agent-shell-menu--action-entry-visible-p (entry)
  "Return non-nil if ENTRY should appear in the action menu.
Entries with no predicate are always visible; entries with a (CMD . PRED)
cdr are visible only when (funcall PRED) returns non-nil."
  (let ((val (cdr entry)))
    (if (and (consp val) (not (functionp val))) (funcall (cdr val)) t)))

;;;###autoload
(defun agent-shell-select-action ()
  "Pick a common agent-shell action and run it via `call-interactively'.
When a permission request is pending, permission responses are spliced into the menu."
  (interactive)
  (let* ((perm-entries (when (and (derived-mode-p 'agent-shell-mode)
				  (agent-shell--permission-pending-p))
			 (seq-map (lambda (b)
				    (cons (format "permission: %s" (car b))
					  (agent-shell--permission-button-action (cdr b))))
				  (agent-shell--permission-buttons))))
	 (cmd-entries (thread-last
			agent-shell-action-alist
			(seq-filter #'agent-shell-menu--action-entry-visible-p)
			(seq-filter (lambda (e) (commandp (agent-shell-menu--action-entry-command e))))
			(seq-map (lambda (e) (cons (car e) (agent-shell-menu--action-entry-command e))))))
	 (all-entries (append perm-entries cmd-entries))
	 (display-table (seq-map (lambda (entry)
				   (cons (car entry)
					 (or (car (split-string (or (documentation (cdr entry)) "") "\n")) "")))
				 all-entries))
	 (label (annotated-completing-read display-table
		 :prompt "agent-shell action =>"
		 :category 'agent-shell-action
		 :require-match t
		 :history 'agent-shell-select-action))
	 (cmd (cdr (assoc label all-entries))))
    (when (commandp cmd)
      (call-interactively cmd))))

;;; Project session switching

;;;###autoload
(defun agent-shell-menu-project-buffers ()
  "Return live agent-shell buffers sharing the current buffer's project directory."
  (let ((dir default-directory)
	(cb (current-buffer)))
    (thread-last
      (agent-shell-buffers)
      (seq-filter (lambda (buf) (not (eq buf cb))))
      (seq-filter (lambda (buf) (with-current-buffer buf
				  (or (equal default-directory dir)
				      (string-prefix-p default-directory dir))))))))
;;;###autoload
(defun agent-shell-switch-project-session ()
  "Switch to another agent-shell session in the same project directory."
  (interactive)
  (let ((bufs (or (agent-shell-menu-project-buffers)
                  (user-error "No other agent-shell sessions for this project"))))
    (switch-to-buffer
     (get-buffer
      (annotated-completing-read
       (seq-map (lambda (buf) (cons (buffer-name buf) (agent-shell--buffer-annotation buf)))
                bufs)
       :prompt "project session =>"
       :category 'agent-shell-buffer
       :require-match t
       :history 'agent-shell-switch-project-session)))))

;;; Send content to agent shell

;;;###autoload
(defun agent-shell-menu-send-file ()
  "Prompt for a file and send it to the current agent-shell session.
Uses `read-file-name' for file selection, integrating with Consult/Vertico."
  (interactive)
  (agent-shell-insert
   :text (agent-shell--get-files-context
          :files (list (expand-file-name (read-file-name "Send file: "))))))

;;;###autoload
(defun agent-shell-menu-send-buffer ()
  "Pick a buffer and send its contents to the current agent-shell session.
File-visiting buffers are sent as @file references; others as raw text."
  (interactive)
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (buf)
              (setf (map-elt table (buffer-name buf))
                    (with-current-buffer buf
                      (format "%-20s %s"
                              (symbol-name major-mode)
                              (or (buffer-file-name) "")))))
            (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b)))
                        (buffer-list)))
    (when-let* ((name (annotated-completing-read table
                                                 :prompt "Send buffer: "
                                                 :require-match t))
                (buf (get-buffer name)))
      (if-let* ((file (buffer-file-name buf)))
          (agent-shell-insert :text (agent-shell--get-files-context :files (list file)))
        (agent-shell-insert :text (with-current-buffer buf (buffer-string)))))))

;;; Transient menus

(defun agent-shell-menu-new-shell-in-dir (dir)
  "Start a new agent-shell session in DIR."
  (interactive "DNew shell in directory: ")
  (agent-shell--new-shell :location dir))

(defun agent-shell-menu--goto-last-interaction ()
  "Move to the last agent-shell interaction."
  (interactive)
  (agent-shell-goto-last-interaction))

(defun agent-shell-menu--agent-review-available-p ()
  "Return non-nil when `agent-review' is loaded."
  (featurep 'agent-review))

(defun agent-shell-menu--interjection-p ()
  "Return non-nil when in an active interjection buffer."
  (derived-mode-p 'agent-shell-queue-interjection-mode))

;;;###autoload
(transient-define-prefix agent-shell-dispatch ()
  "agent-shell operations — navigate, act, send, queue, and session management."
  [:description "Permissions"
   :class transient-column
   :if agent-shell--session-permission-pending-p
   :setup-children agent-shell--permission-suffixes]
  ;; Session management (always), act/write/settings/fork (session-conditional)
  [["Sessions"
    ("ss" "Switch session" agent-shell-switch-buffer)
    ("sb" "Find buffer" agent-shell-manager-find-buffer)
    ("sm" "Manager toggle" agent-shell-manager-toggle)]
   ["Create"
    ("sn" "New shell" agent-shell-new-shell)
    ("st" "New temp shell" agent-shell-new-temp-shell)
    ("sh" "Hydrate (resume)" agent-shell-resume-session)
    ("sd" "New in directory" agent-shell-menu-new-shell-in-dir)
    ("rr" "Review changes" agent-review
     :if agent-shell-menu--agent-review-available-p)
    ("rs" "Send issues to shell" agent-review-send-to-agent-shell
     :if agent-shell-menu--agent-review-available-p)]
   ["Actions" :if agent-shell-menu--in-session-p
    ("aa" "Action menu" agent-shell-select-action)
    ("ai" "Interrupt" agent-shell-interrupt)
    ("ar" "Resolve permission" agent-shell-resolve-permission)
    ("ac" "Command menu" agent-shell-select-command)
    ("ax" "Collapse menu" agent-shell-select-collapse)
    ("ij" "Interject" agent-shell-queue-interject
     :inapt-if-not agent-shell-queue-interject-available-p)
    ("is" "Send interjection" agent-shell-queue-interjection-send
     :if agent-shell-menu--interjection-p)
    ("ic" "Close/Abort interjection" agent-shell-queue-interjection-close
     :if agent-shell-menu--interjection-p)]
   ["Settings" :if agent-shell-menu--in-session-p
    ("mm" "Set mode" agent-shell-set-session-mode)
    ("mv" "Set model" agent-shell-set-session-model)
    ("mc" "Cycle mode" agent-shell-cycle-session-mode)
    ("mi" "Copy session ID" agent-shell-copy-session-id)
    ("mt" "Open transcript" agent-shell-open-transcript)]
   ["Fork" :if agent-shell-menu--in-session-p
    ("ff" "Fork session" agent-shell-fork)
    ("fo" "Other (project)" agent-shell-switch-project-session)
    ("fq" "Fork queue" agent-shell-queue-fork-session)
    ("fb" "Insert fork before" agent-shell-queue-insert-fork-before)
    ("fa" "Insert fork after" agent-shell-queue-insert-fork-after)
    ("fr" "Release pending fork" agent-shell-queue-release-pending-fork)]]
  ;; Queue row: global queue ops, intercept config, and capture
  [["Capture"
    ("cw" "Compose (write)" agent-shell-queue-capture)
    ("cu" "Unassigned" agent-shell-queue-capture-unassigned)
    ("cr" "From region" agent-shell-queue-capture-from-region)
    ("cy" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("cc" "From context" agent-shell-queue-capture-from-context)
    ("wf" "Send file" agent-shell-menu-send-file)
    ("wb" "Send buffer" agent-shell-menu-send-buffer)]
   ["Queue"
    ("qq" "Open queue" agent-shell-queue-buffer-open)
    ("qb" "Switch to queue" agent-shell-queue-buffer-switch)
    ("qe" "Enqueue" agent-shell-queue-enqueue)
    ("qd" "Edit task" agent-shell-queue-edit-task)
    ("qp" "Suspend all dispatch" agent-shell-queue-pause
     :inapt-if agent-shell-queue-paused-p)
    ("qr" "Resume all dispatch" agent-shell-queue-resume
     :inapt-if-not agent-shell-queue-paused-p)
    ("qu" "Resume all sessions" agent-shell-queue-unpause-all-sessions)]
  ;; Per-session queue controls
   ["Session Queue" :if agent-shell-menu--in-session-p
    ("qsp" "Suspend this session" agent-shell-queue-session-pause
     :inapt-if agent-shell-queue-session-paused-p)
    ("qsr" "Resume this session" agent-shell-queue-session-resume
     :inapt-if-not agent-shell-queue-session-paused-p)
    ("qim" agent-shell-queue-toggle-input-mode
     :description (lambda ()
                    (format "Input mode: [%s]" (agent-shell-queue-input-mode-value))))
    ("qix" "Reset all to default" agent-shell-queue-reset-all-input-modes)
    ("qid" agent-shell-queue-set-input-mode-default
     :description (lambda ()
                    (format "Default: [%s]" agent-shell-queue-input-mode-default))
     :if agent-shell-menu--in-shell-p)]
   ])

;;;###autoload
(defalias 'agent-shell-session-menu #'agent-shell-dispatch)

;;; Capture menu — bound to "e" in agent-shell-mode-map

;;;###autoload
(transient-define-prefix agent-shell-queue-capture-menu ()
  "Enqueue and capture operations for the current agent-shell session.
Bound to \"e\" in `agent-shell-mode-map'; self-inserts at the idle prompt."
  [["Capture"
    ("c"  "Compose prompt"      agent-shell-queue-capture)
    ("u"  "Unassigned"          agent-shell-queue-capture-unassigned)
    ("r"  "From region"         agent-shell-queue-capture-from-region)
    ("y"  "From clipboard"      agent-shell-queue-capture-from-clipboard)
    ("x"  "From context"        agent-shell-queue-capture-from-context)]
   ["Enqueue"
    ("ee" "Enqueue prompt"      agent-shell-queue-enqueue)
    ("el" "Emacs Lisp form"     agent-shell-queue-enqueue-emacs)
    ("ec" "Emacs command"       agent-shell-queue-enqueue-emacs-command)
    ("es" "Shell (eshell)"      agent-shell-queue-enqueue-shell-eshell)
    ("et" "Shell (eat)"         agent-shell-queue-enqueue-shell-eat)]
   ["Insert"
    ("p"  "Pause checkpoint"    agent-shell-queue-insert-pause)
    ("d"  "Context drop"        agent-shell-queue-insert-clear-context)
    ("k"  "Compact (manual)"    agent-shell-queue-insert-compact)
    ("w"  "Wait-until (timer)"  agent-shell-queue-insert-wait)]])

(agent-shell-mode-key "e" agent-shell-queue-capture-menu)

;;; Command menu

;;;###autoload
(defun agent-shell-select-command ()
  "Insert one of the agent's advertised `/' commands at the prompt."
  (interactive)
  (let* ((shell (or (cond
		     ((derived-mode-p 'agent-shell-mode) (current-buffer))
		     ((agent-shell-viewport--shell-buffer)))
		    (user-error "not in an agent-shell or viewport buffer")))
	 (commands (with-current-buffer shell
		     (map-elt agent-shell--state :available-commands))))
    (unless commands
      (user-error "no agent slash-commands advertised in %s" (buffer-name shell)))
    (agent-shell-insert :text (concat "/" (annotated-completing-read
					   (seq-map (lambda (c)
						     (cons (map-elt c 'name)
							        (replace-regexp-in-string "[\n\r]+" " "
							    (or (map-elt c 'description) ""))))
						   commands)
					   :prompt "agent /command => "
					   :category 'agent-shell-slash-command
					   :require-match t
					   :history 'agent-shell-select-command) " ")
			:shell-buffer shell
			:submit nil)))

;;; Collapse menu

(defun agent-shell--blocks-in-buffer ()
  "Return one entry per distinct fragment block in the buffer.
Each entry is `((:start . POS) (:state . STATE))'.  Plain-text entries
created via `agent-shell-ui-update-text' (no `:collapsed' key) are skipped."
  (let ((seen (make-hash-table :test 'equal))
	(pos (point-min))
	out)
    (while pos
      (when-let* ((state (get-text-property pos 'agent-shell-ui-state))
		  (id (map-elt state :qualified-id))
		  ((assq :collapsed state))
		  ((not (map-elt seen id))))
	(setf (map-elt seen id) t)
	(push (list (cons :start pos) (cons :state state)) out))
      (setq pos (next-single-property-change pos 'agent-shell-ui-state)))
    (nreverse out)))

(defun agent-shell--block-category (qualified-id)
  "Classify QUALIFIED-ID into a coarse block category string."
  (cond
   ((string-match-p "agent_thought_chunk\\'" qualified-id) "thinking")
   ((string-match-p "agent_message_chunk\\'" qualified-id) "agent message")
   ((string-match-p "user_message_chunk\\'" qualified-id)  "user message")
   ((string-suffix-p "-plan" qualified-id)                 "plan")
   ((string-prefix-p "bootstrapping-" qualified-id)        "session info")
   (t                                                       "tool call")))

(cl-defun agent-shell--set-collapse (target &key category)
  "Force `:collapsed' = TARGET on every toggleable block.
When CATEGORY is non-nil, only affect blocks matching that category."
  (save-mark-and-excursion
    (seq-do (lambda (block)
              (let* ((state (map-elt block :state))
                     (id (map-elt state :qualified-id))
                     (collapsed (map-elt state :collapsed)))
                (when (and (not (eq (and collapsed t) (and target t)))
                           (or (null category)
                               (equal category (agent-shell--block-category id))))
                  (goto-char (map-elt block :start))
                  (agent-shell-ui--toggle-fragment-at-point))))
            (agent-shell--blocks-in-buffer))))

;;;###autoload
(defun agent-shell-select-collapse ()
  "Pick a collapse action via `annotated-completing-read'.
Offers bulk expand/collapse, per-category toggles, and entries to flip the
three expand-by-default customization variables."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-mode)
	      (derived-mode-p 'agent-shell-viewport-view-mode))
    (user-error "Not in an agent-shell buffer"))
  (let* ((by-cat (make-hash-table :test #'equal))
	 (table (make-hash-table :test #'equal))
	 (toggles '(("~ thinking: expand-by-default"
		     . agent-shell-thought-process-expand-by-default)
		    ("~ tool call: expand-by-default"
		     . agent-shell-tool-use-expand-by-default)
		    ("~ user message: expand-by-default"
		     . agent-shell-user-message-expand-by-default))))
    (seq-do (lambda (b)
              (let* ((state (map-elt b :state))
                     (cat (agent-shell--block-category (map-elt state :qualified-id)))
                     (entry (or (map-elt by-cat cat) (cons 0 0))))
                (cl-incf (car entry))
                (when (map-elt state :collapsed) (cl-incf (cdr entry)))
                (setf (map-elt by-cat cat) entry)))
            (agent-shell--blocks-in-buffer))
    (setf (map-elt table "+ expand all") "show every collapseable block")
    (setf (map-elt table "+ collapse all") "hide every collapseable block")
    (setf (map-elt table "~ set all: collapse by default")
          (if (and (not (symbol-value 'agent-shell-thought-process-expand-by-default))
                   (not (symbol-value 'agent-shell-tool-use-expand-by-default))
                   (not (symbol-value 'agent-shell-user-message-expand-by-default)))
              "already collapsed by default"
            "set thinking, tool calls, and user messages to collapse by default"))
    (setf (map-elt table "~ set all: expand by default")
          (if (and (symbol-value 'agent-shell-thought-process-expand-by-default)
                   (symbol-value 'agent-shell-tool-use-expand-by-default)
                   (symbol-value 'agent-shell-user-message-expand-by-default))
              "already expanded by default"
            "set thinking, tool calls, and user messages to expand by default"))
    (seq-do (lambda (cat)
              (let* ((entry (map-elt by-cat cat))
                     (total (car entry))
                     (n-collapsed (cdr entry))
                     (state-str (cond ((zerop n-collapsed) "all expanded")
                                      ((= n-collapsed total) "all collapsed")
                                      (t (format "%d/%d collapsed" n-collapsed total)))))
                (setf (map-elt table cat) (format "%d block%s · %s"
                                                  total (if (= total 1) "" "s") state-str))))
            (sort (map-keys by-cat) #'string<))
    (seq-do (lambda (toggle)
              (setf (map-elt table (car toggle))
                    (if (symbol-value (cdr toggle)) "expanded by default" "collapsed by default")))
            toggles)
    (let ((choice (annotated-completing-read table
					     :prompt "agent-shell collapse: "
					     :category 'agent-shell-collapse
					     :require-match t
					     :history 'agent-shell-select-collapse)))
      (cond
       ((equal choice "+ expand all")   (agent-shell--set-collapse nil))
       ((equal choice "+ collapse all") (agent-shell--set-collapse t))
       ((equal choice "~ set all: collapse by default")
        (set 'agent-shell-thought-process-expand-by-default nil)
        (set 'agent-shell-tool-use-expand-by-default nil)
        (set 'agent-shell-user-message-expand-by-default nil)
        (message "All block types set to collapse by default"))
       ((equal choice "~ set all: expand by default")
        (set 'agent-shell-thought-process-expand-by-default t)
        (set 'agent-shell-tool-use-expand-by-default t)
        (set 'agent-shell-user-message-expand-by-default t)
        (message "All block types set to expand by default"))
       ((assoc choice toggles)
	(let ((var (cdr (assoc choice toggles))))
	  (set var (not (symbol-value var)))
	  (message "%s → %s" var (if (symbol-value var) "expanded" "collapsed"))))
       (t
	(let ((entry (map-elt by-cat choice)))
	  (agent-shell--set-collapse (< (cdr entry) (car entry))
				     :category choice)))))))

;;; Session info buffer

(declare-function agent-shell--state "agent-shell")
(declare-function shell-maker-process "shell-maker")

(defun agent-shell-menu--count-fragments (buf)
  "Return the number of distinct UI fragments (blocks) in BUF."
  (with-current-buffer buf
    (let ((seen (make-hash-table :test #'equal))
          (pos (point-min))
          (count 0))
      (while pos
        (when-let* ((state (get-text-property pos 'agent-shell-ui-state))
                    (id (map-elt state :qualified-id))
                    ((not (map-elt seen id))))
          (setf (map-elt seen id) t)
          (cl-incf count))
        (setq pos (next-single-property-change pos 'agent-shell-ui-state)))
      count)))

(defun agent-shell-menu--format-uptime (start-time)
  "Return a human-readable uptime string from START-TIME (a time value)."
  (if start-time
      (let* ((delta (float-time (time-since start-time)))
             (days  (floor (/ delta 86400)))
             (hours (floor (/ (mod delta 86400) 3600)))
             (mins  (floor (/ (mod delta 3600) 60)))
             (secs  (floor (mod delta 60))))
        (cond
         ((> days 0)  (format "%dd %dh %dm" days hours mins))
         ((> hours 0) (format "%dh %dm %ds" hours mins secs))
         ((> mins 0)  (format "%dm %ds" mins secs))
         (t           (format "%ds" secs))))
    "unknown"))

(defun agent-shell-menu--process-start-time (proc)
  "Return the start time of PROC as a time value, or nil."
  (when-let* ((attrs (ignore-errors (process-attributes (process-id proc))))
              (start (alist-get 'start attrs)))
    start))

(defvar agent-shell-info-map (make-sparse-keymap)
  "Keymap for agent-shell info help buffers.  Inherits `help-mode-map' once loaded.")

(with-eval-after-load 'help-mode
  (set-keymap-parent agent-shell-info-map help-mode-map))

;;;###autoload
(defun agent-shell-session-info ()
  "Display a read-only ephemeral buffer with live session diagnostics.
Shows fragment count, agent uptime, queue options, queue depth, and
the underlying shell process uptime for the current agent-shell buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let* ((shell-buf (current-buffer))
         (buf-name (buffer-name shell-buf))
         (state (ignore-errors (agent-shell--state)))
         (session (when state (map-elt state :session)))
         (last-activity (when state (map-elt state :last-activity-time)))
         (request-count (when state (or (map-elt state :request-count) 0)))
         (fragment-count (agent-shell-menu--count-fragments shell-buf))
         (proc (ignore-errors (shell-maker-process buf-name)))
         (proc-start (when proc (agent-shell-menu--process-start-time proc)))
         (model (when session (map-elt session :model-id)))
         (mode-id (when session (map-elt session :mode-id)))
         (input-mode (if (boundp 'agent-shell-queue-input-mode)
                         (buffer-local-value 'agent-shell-queue-input-mode shell-buf)
                       'default))
         (globally-paused (ignore-errors
                            (agent-shell-queue-queue-paused agent-shell-queue--queue)))
         (session-paused (ignore-errors
                           (member buf-name
                                   (agent-shell-queue-queue-session-paused
                                    agent-shell-queue--queue))))
         (queue-items (ignore-errors
                        (cdr (assoc buf-name
                                    (agent-shell-queue-store-items
                                     agent-shell-queue--store)))))
         (queue-depth (length queue-items))
         (active-items (seq-count (lambda (it)
                                    (memq (agent-shell-queue-item-status it)
                                          '(active running)))
                                  (or queue-items nil)))
         (info-buf-name (format "*agent-shell-info: %s*" buf-name)))
    (with-help-window info-buf-name
      (with-current-buffer standard-output
        (insert (propertize (format "Session: %s\n" buf-name) 'face 'bold))
        (insert (make-string (+ 9 (length buf-name)) ?─) "\n\n")
        (insert (propertize "Agent\n" 'face '(bold underline)))
        (insert (format "  Fragments (blocks)  %d\n" fragment-count))
        (insert (format "  Requests sent       %d\n" request-count))
        (insert (format "  Last activity       %s\n"
                        (if last-activity
                            (format "%s ago" (agent-shell-menu--format-uptime last-activity))
                          "none")))
        (when model
          (insert (format "  Model               %s\n" model)))
        (when mode-id
          (insert (format "  Mode                %s\n" mode-id)))
        (insert "\n")
        (insert (propertize "Process\n" 'face '(bold underline)))
        (insert (format "  Uptime              %s\n"
                        (if proc-start
                            (agent-shell-menu--format-uptime proc-start)
                          (if proc "running (start unknown)" "not running"))))
        (insert "\n")
        (insert (propertize "Queue\n" 'face '(bold underline)))
        (insert (format "  Depth               %d total, %d active\n"
                        queue-depth active-items))
        (insert (format "  Input mode          %s\n" input-mode))
        (insert (format "  Global pause        %s\n" (if globally-paused "paused" "running")))
        (insert (format "  Session suspended   %s\n" (if session-paused "yes" "no")))))
    (when-let* ((buf (get-buffer info-buf-name)))
      (with-current-buffer buf
        (let ((m (make-composed-keymap (make-sparse-keymap) agent-shell-info-map)))
          (define-key m (kbd "g") (lambda () (interactive)
                                    (with-current-buffer shell-buf
                                      (agent-shell-session-info))))
          (use-local-map m))))))

;;; Backup restore

;;;###autoload
(defun agent-shell-queue-restore-from-backup ()
  "Restore queue items from a versioned backup via annotated-completing-read.
Lists backup files in the safe-save directory newest-first.  Only items whose
item-id is not already present in the live queue are added, so the operation
is safe to run against an active queue.  Saves and refreshes after merging."
  (interactive)
  (let* ((dir (agent-shell-queue--safe-save-directory))
         (_ (unless (file-directory-p dir)
              (user-error "No backup directory: %s" dir)))
         (files (seq-filter
                 (lambda (f)
                   (string-match-p "\\`agent-shell-queue-archive-[0-9]+"
                                   (file-name-nondirectory f)))
                 (directory-files dir t nil t)))
         (_ (unless files (user-error "No backup files found in %s" dir)))
         (sorted (sort (copy-sequence files)
                       (lambda (a b) (file-newer-than-file-p a b))))
         (table (seq-map (lambda (f)
                           (cons (file-name-nondirectory f)
                                 (format-time-string
                                  "%Y-%m-%d %H:%M"
                                  (file-attribute-modification-time
                                   (file-attributes f)))))
                         sorted))
         (choice (annotated-completing-read table
                                            :prompt "restore backup => "
                                            :category 'agent-shell-queue-backup
                                            :require-match t
                                            :history 'agent-shell-queue-restore-from-backup))
         (chosen-file (expand-file-name choice dir))
         (ext (file-name-extension chosen-file t))
         (fmt (cond ((string= ext ".el") 'plist)
                    ((string= ext ".json") 'json)
                    ((string= ext ".yaml") 'yaml)
                    (t (user-error "Unknown backup format: %s" ext)))))
    (agent-shell-queue--ensure-loaded)
    (let* ((live-store (agent-shell-queue--current-store))
           (backup-items (agent-shell-queue-deserialize
                          (agent-shell-queue--make-store
                           :format fmt :file chosen-file :items nil)
                          (with-temp-buffer
                            (insert-file-contents chosen-file)
                            (buffer-string))))
           (current-ids (thread-last
                          (agent-shell-queue-store-items live-store)
                          (seq-mapcat #'cdr)
                          (seq-map #'agent-shell-queue-item-id)))
           (added 0))
      (seq-do (lambda (bucket)
                (let* ((buf-name (car bucket))
                       (new-items (seq-remove
                                   (lambda (item)
                                     (member (agent-shell-queue-item-id item) current-ids))
                                   (cdr bucket)))
                       (live-bucket (assoc buf-name
                                          (agent-shell-queue-store-items live-store))))
                  (when new-items
                    (setq added (+ added (length new-items)))
                    (if live-bucket
                        (setcdr live-bucket (append (cdr live-bucket) new-items))
                      (setf (agent-shell-queue-store-items live-store)
                            (append (agent-shell-queue-store-items live-store)
                                    (list (cons buf-name new-items))))))))
              backup-items)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer)
      (message "agent-shell-queue: restored %d item(s) from %s" added choice))))

;;; Wire menu key into queue mode map

(define-key agent-shell-queue-mode-map (kbd "M") #'agent-shell-session-menu)

(provide 'agent-shell-menu)

;;; agent-shell-menu.el ends here
