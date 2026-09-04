;;; agent-shell-menu.el --- ACR menus and transient prefixes for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell "0.1") (annotated-completing-read "0.1"))

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
;; prefix menu `agent-shell-menu-dispatch' for
;; navigating and controlling agent sessions.  Covers permission resolution,
;; action selection, command selection, and collapse control.

;;; Code:

(require 'cl-lib)
(require 'help-mode)
(require 'annotated-completing-read)
(require 'transient)
(require 'sprite-future nil t)
(require 'agent-shell)
(require 'agent-shell-queue)
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell-ui--toggle-fragment-at-point "agent-shell-ui")
(declare-function agent-shell--config-icon "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-switch-buffer "agent-shell")
(declare-function agent-review "agent-review")
(declare-function agent-review-send-to-agent-shell "agent-review")
(declare-function shell-maker-process "shell-maker")

;;; Session and Project Switching

(defun agent-shell-menu--buffer-annotation (buf)
  "Build an annotation string describing the agent-shell BUF.
Includes the agent icon (when `agent-shell-show-config-icons' is non-nil),
a color-coded status, the session title, cwd, context-usage percentage,
and time since last activity — a superset of what
`agent-shell--read-shell-buffer' shows for the upstream picker."
  (with-current-buffer buf
    (let* ((status (agent-shell-status))
	   (state agent-shell--state)
	   (icon (when agent-shell-show-config-icons
		   (agent-shell--config-icon :config (map-elt state :agent-config))))
	   (title (let ((raw (string-trim
			      (car (split-string
				    (or (map-nested-elt state '(:session :title)) "")
				    "\n")))))
		    (if (> (length raw) 50) (concat (substring raw 0 47) "...") raw)))
	   (used (map-nested-elt state '(:usage :context-used)))
	   (size (map-nested-elt state '(:usage :context-size)))
	   (last (map-elt state :last-activity-time))
	   (cwd (abbreviate-file-name (or default-directory ""))))
      (mapconcat 'identity
		 (seq-remove #'null
		  (list (when icon (concat icon " "))
			(propertize (format "[%s]" status)
				    'face (pcase status
					    ('busy 'warning)
					    ('blocked 'error)
					    (_ 'success)))
			(unless (string-empty-p title) title)
			cwd
			(when (and (numberp used) (numberp size) (> size 0))
			  (format "ctx %.0f%%" (* 100.0 (/ (float used) size))))
			(when last
			  (format "%s ago"
				  (agent-shell-queue--format-age (time-since last))))))
		 " · "))))

(defun agent-shell-menu--pick-buffer (prompt)
  "Select an agent-shell buffer via PROMPT using status/cwd/context annotations."
  (let ((bufs (or (agent-shell-buffers) (user-error "No live agent-shell buffers"))))
    (get-buffer
     (annotated-completing-read
      (thread-last
	bufs
	(seq-map (lambda (buf) (cons (buffer-name buf) (agent-shell-menu--buffer-annotation buf)))))
      :prompt prompt
      :category 'agent-shell-buffer
      :require-match t
      :history 'agent-shell-menu--pick-buffer))))

;;;###autoload
(defun agent-shell-menu--switch-buffer ()
  "Switch to an agent-shell buffer with annotations.
Annotations include icon, status, title, cwd, and age.
Installed as `:override' advice on `agent-shell-switch-buffer' so the
enhanced picker is used under the original, upstream-compatible name
instead of introducing a second public entry point.  Respects
`agent-shell-prefer-viewport-interaction' like the original."
  (interactive)
  (let ((shell-buffer (agent-shell-menu--pick-buffer "agent-shell =>")))
    (switch-to-buffer (or (when agent-shell-prefer-viewport-interaction
			     (agent-shell-viewport--buffer
			      :shell-buffer shell-buffer
			      :existing-only t))
			   shell-buffer))))

(advice-add 'agent-shell-switch-buffer :override #'agent-shell-menu--switch-buffer)

;;;###autoload
(defun agent-shell-menu-select-session-mode (&optional on-success)
  "Select a session mode via `annotated-completing-read'.
Annotates each candidate with its description (the same text shown at
the start of an agent-shell session) and marks the active mode as
\"[current]\", instead of requiring the user to already know the
candidate list.  Unlike `agent-shell-cycle-session-mode' this prompts
instead of blindly advancing to the next mode.

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (let ((available-modes (agent-shell--get-available-modes (agent-shell--state))))
    (unless available-modes
      (user-error "No session modes available"))
    (let* ((current-mode-id (agent-shell--current-mode-id (agent-shell--state)))
           (table (mapcar (lambda (mode)
                            (let ((desc (map-elt mode :description))
                                  (current-p (equal (map-elt mode :id) current-mode-id)))
                              (cons (map-elt mode :name)
                                    (cons (cond
                                           ((and desc current-p)
                                            (concat (propertize "[current]" 'face 'bold) " " desc))
                                           (desc desc)
                                           (current-p (propertize "[current]" 'face 'bold))
                                           (t nil))
                                          (map-elt mode :id)))))
                          available-modes))
           (default-mode-name (and current-mode-id
                                   (agent-shell--resolve-session-mode-name
                                    current-mode-id available-modes)))
           (selected-mode-id (annotated-completing-read
                              table
                              :prompt "Set session mode: "
                              :require-match t
                              :default default-mode-name)))
      (unless selected-mode-id
        (user-error "Unknown session mode"))
      (when (and current-mode-id (string= selected-mode-id current-mode-id))
        (error "Session mode already %s"
               (or (agent-shell--resolve-session-mode-name selected-mode-id available-modes)
                   selected-mode-id)))
      (agent-shell--config-option-set-mode-id
       :mode-id selected-mode-id
       :on-success on-success))))

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
(defun agent-shell-menu-switch-project-session ()
  "Switch to another agent-shell session in the same project directory."
  (interactive)
  (let ((bufs (or (agent-shell-menu-project-buffers)
                  (user-error "No other agent-shell sessions for this project"))))
    (switch-to-buffer
     (get-buffer
      (annotated-completing-read
       (seq-map (lambda (buf) (cons (buffer-name buf) (agent-shell-menu--buffer-annotation buf)))
                bufs)
       :prompt "project session =>"
       :category 'agent-shell-buffer
       :require-match t
       :history 'agent-shell-menu-switch-project-session)))))

;;; Actions and Commands

(defvar agent-shell-menu-action-alist
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
    ("jump to permission row" . (agent-shell-jump-to-latest-permission-button-row . agent-shell-menu--session-permission-pending-p))
    ("next permission button" . (agent-shell-next-permission-button . agent-shell-menu--session-permission-pending-p))
    ("previous permission button" . (agent-shell-previous-permission-button . agent-shell-menu--session-permission-pending-p))
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
    ("set session mode" . (agent-shell-menu-select-session-mode . agent-shell-menu--in-session-p))
    ("set session model" . (agent-shell-set-session-model . agent-shell-menu--in-session-p))
    ("copy session id" . (agent-shell-copy-session-id . agent-shell-menu--in-session-p))
    ("open transcript" . (agent-shell-open-transcript . agent-shell-menu--in-session-p))
    ("session info" . (agent-shell-menu-session-info . agent-shell-menu--in-session-p))
    ("collapse menu" . (agent-shell-menu-select-collapse . agent-shell-menu--in-session-p))
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
  "Alist mapping label strings to commands for `agent-shell-menu-select-action'.
Each entry is either (LABEL . COMMAND) or (LABEL . (COMMAND . PREDICATE)).
When a PREDICATE is supplied it is called with no arguments; the entry is
omitted from the menu when the predicate returns nil.")

(defun agent-shell-menu--action-entry-command (entry)
  "Return the command for ENTRY.
ENTRY cdr may be a plain COMMAND symbol, a function, or a
(COMMAND . PREDICATE) cons."
  (let ((val (cdr entry)))
    (if (and (consp val) (not (functionp val))) (car val) val)))

(defun agent-shell-menu--action-entry-visible-p (entry)
  "Return non-nil if ENTRY should appear in the action menu.
Entries with no predicate are always visible; entries with a (CMD . PRED)
cdr are visible only when (funcall PRED) returns non-nil."
  (let ((val (cdr entry)))
    (if (and (consp val) (not (functionp val))) (funcall (cdr val)) t)))

;;;###autoload
(defun agent-shell-menu-select-action ()
  "Pick a common agent-shell action and run it via `call-interactively'.
When a permission request is pending, permission responses are spliced
into the menu."
  (interactive)
  (let* ((perm-entries (when (and (derived-mode-p 'agent-shell-mode)
				  (agent-shell--permission-pending-p))
			 (seq-map (lambda (b)
				    (cons (format "permission: %s" (car b))
					  (agent-shell-menu--permission-button-action (cdr b))))
				  (agent-shell-menu--permission-buttons))))
	 (cmd-entries (thread-last
			agent-shell-menu-action-alist
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
		 :history 'agent-shell-menu-select-action))
	 (cmd (cdr (assoc label all-entries))))
    (when (commandp cmd)
      (call-interactively cmd))))

(defun agent-shell-menu--permission-buttons ()
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

(defun agent-shell-menu--permission-action-at (position)
  "Return the RET command bound on the permission button at POSITION."
  (when-let* ((map (get-text-property position 'keymap)))
    (lookup-key map (kbd "RET"))))

(defun agent-shell-menu--permission-button-action (pos)
  "Return an interactive command that activates the permission button at POS."
  (lambda ()
    (interactive)
    (when-let* ((cmd (agent-shell-menu--permission-action-at pos))
                ((functionp cmd))
                ((commandp cmd)))
      (save-excursion
	(goto-char pos)
	(call-interactively cmd)))))

;;;###autoload
(defun agent-shell-menu-resolve-permission ()
  "Resolve a pending permission prompt via `annotated-completing-read'."
  (interactive)

  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))

  (unless (agent-shell--permission-pending-p)
    (user-error "No pending permission request in this buffer"))

  (let* ((buttons (or (agent-shell-menu--permission-buttons)
                      (user-error "No permission buttons found in this buffer")))
         (label (annotated-completing-read
		 buttons
                 :prompt "permission => "
                 :category 'agent-shell-permission
                 :require-match t
                 :history 'agent-shell-menu-resolve-permission))
         (pos (cdr (assoc label buttons)))
         (cmd (or (and pos (agent-shell-menu--permission-action-at pos))
                  (user-error "No action attached to permission button"))))

    (save-excursion
      (goto-char pos)
      (call-interactively cmd))))

;;;###autoload
(defun agent-shell-menu-select-command ()
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
					   :history 'agent-shell-menu-select-command) " ")
			:shell-buffer shell
			:submit nil)))

;;; Content Insertion

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

;;; Output and Collapse Control

(defun agent-shell-menu--blocks-in-buffer ()
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

(defun agent-shell-menu--block-category (qualified-id)
  "Classify QUALIFIED-ID into a coarse block category string."
  (cond
   ((string-match-p "agent_thought_chunk\\'" qualified-id) "thinking")
   ((string-match-p "agent_message_chunk\\'" qualified-id) "agent message")
   ((string-match-p "user_message_chunk\\'" qualified-id)  "user message")
   ((string-suffix-p "-plan" qualified-id)                 "plan")
   ((string-prefix-p "bootstrapping-" qualified-id)        "session info")
   (t                                                       "tool call")))

(cl-defun agent-shell-menu--set-collapse (target &key category)
  "Force `:collapsed' = TARGET on every toggleable block.
When CATEGORY is non-nil, only affect blocks matching that category."
  (save-mark-and-excursion
    (seq-do (lambda (block)
              (let* ((state (map-elt block :state))
                     (id (map-elt state :qualified-id))
                     (collapsed (map-elt state :collapsed)))
                (when (and (not (eq (and collapsed t) (and target t)))
                           (or (null category)
                               (equal category (agent-shell-menu--block-category id))))
                  (goto-char (map-elt block :start))
                  (agent-shell-ui--toggle-fragment-at-point)
                  ;; If this is a group and we are collapsing/expanding, propagate one level deep
                  (when (eq (map-elt state :kind) 'group)
                    (seq-do (lambda (child)
                              (when-let* ((child-state (get-text-property (map-elt child :start) 'agent-shell-ui-state))
                                          (child-collapsed (map-elt child-state :collapsed))
                                          ((not (eq child-collapsed (and target t)))))
                                (goto-char (map-elt child :start))
                                (agent-shell-ui--toggle-fragment-at-point)))
                            (agent-shell-ui--group-children :group-qualified-id id))))))
            (agent-shell-menu--blocks-in-buffer))))

;;;###autoload
(defun agent-shell-menu-select-collapse ()
  "Pick a collapse action via `annotated-completing-read'.
Offers bulk expand/collapse, per-category toggles, entries to flip the
three expand-by-default customization variables globally, and a
buffer-local variant that only changes the default for this session."
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
		     . agent-shell-user-message-expand-by-default)
		    ("~ activity group: expand-by-default"
		     . agent-shell-activity-group-expand-by-default))))
    (seq-do (lambda (b)
              (let* ((state (map-elt b :state))
                     (cat (agent-shell-menu--block-category (map-elt state :qualified-id)))
                     (entry (or (map-elt by-cat cat) (cons 0 0))))
                (cl-incf (car entry))
                (when (map-elt state :collapsed) (cl-incf (cdr entry)))
                (setf (map-elt by-cat cat) entry)))
            (agent-shell-menu--blocks-in-buffer))
    (setf (map-elt table "+ expand all") "select to expand every collapseable block")
    (setf (map-elt table "+ collapse all") "select to collapse every collapseable block")
    (setf (map-elt table "~ set all: collapse by default")
          (if (and (not (symbol-value 'agent-shell-thought-process-expand-by-default))
                   (not (symbol-value 'agent-shell-tool-use-expand-by-default))
                   (not (symbol-value 'agent-shell-user-message-expand-by-default))
                   (not (symbol-value 'agent-shell-activity-group-expand-by-default)))
              "currently all collapsed by default → no change"
            "currently mixed/expanded → select to collapse all block types by default"))
    (setf (map-elt table "~ set all: expand by default")
          (if (and (symbol-value 'agent-shell-thought-process-expand-by-default)
                   (symbol-value 'agent-shell-tool-use-expand-by-default)
                   (symbol-value 'agent-shell-user-message-expand-by-default)
                   (symbol-value 'agent-shell-activity-group-expand-by-default))
              "currently all expanded by default → no change"
            "currently mixed/collapsed → select to expand all block types by default"))
    (setf (map-elt table "* this session: expand by default")
          (if (and (local-variable-p 'agent-shell-thought-process-expand-by-default)
                   (local-variable-p 'agent-shell-tool-use-expand-by-default)
                   (local-variable-p 'agent-shell-user-message-expand-by-default)
                   (local-variable-p 'agent-shell-activity-group-expand-by-default))
              "currently buffer-local → already set for this session"
            "currently global defaults → select to expand all now and default to expanded in this session only"))
    (seq-do (lambda (cat)
              (let* ((entry (map-elt by-cat cat))
                     (total (car entry))
                     (n-collapsed (cdr entry))
                     (state-and-action
                      (cond ((zerop n-collapsed) "currently all expanded → select to collapse")
                            ((= n-collapsed total) "currently all collapsed → select to expand")
                            (t (format "currently %d/%d collapsed → select to collapse all" n-collapsed total)))))
                (setf (map-elt table cat) (format "%d block%s · %s"
                                                  total (if (= total 1) "" "s") state-and-action))))
            (sort (map-keys by-cat) #'string<))
    (seq-do (lambda (toggle)
              (setf (map-elt table (car toggle))
                    (if (symbol-value (cdr toggle))
                        "currently expanded by default → select to collapse"
                      "currently collapsed by default → select to expand")))
            toggles)
    (let ((choice (annotated-completing-read table
					     :prompt "agent-shell collapse: "
					     :category 'agent-shell-collapse
					     :require-match t
					     :history 'agent-shell-menu-select-collapse)))
      (cond
       ((equal choice "+ expand all")   (agent-shell-menu--set-collapse nil))
       ((equal choice "+ collapse all") (agent-shell-menu--set-collapse t))
       ((equal choice "~ set all: collapse by default")
        (set 'agent-shell-thought-process-expand-by-default nil)
        (set 'agent-shell-tool-use-expand-by-default nil)
        (set 'agent-shell-user-message-expand-by-default nil)
        (set 'agent-shell-activity-group-expand-by-default nil)
        (message "All block types set to collapse by default"))
       ((equal choice "~ set all: expand by default")
        (set 'agent-shell-thought-process-expand-by-default t)
        (set 'agent-shell-tool-use-expand-by-default t)
        (set 'agent-shell-user-message-expand-by-default t)
        (set 'agent-shell-activity-group-expand-by-default t)
        (message "All block types set to expand by default"))
       ((equal choice "* this session: expand by default")
        (setq-local agent-shell-thought-process-expand-by-default t)
        (setq-local agent-shell-tool-use-expand-by-default t)
        (setq-local agent-shell-user-message-expand-by-default t)
        (setq-local agent-shell-activity-group-expand-by-default t)
        (agent-shell-menu--set-collapse nil)
        (message "This session now defaults to expanded (buffer-local; other sessions unaffected)"))
       ((assoc choice toggles)
	(let ((var (cdr (assoc choice toggles))))
	  (set var (not (symbol-value var)))
	  (message "%s → %s" var (if (symbol-value var) "expanded" "collapsed"))))
       (t
	(let ((entry (map-elt by-cat choice)))
	  (agent-shell-menu--set-collapse (< (cdr entry) (car entry))
				     :category choice)))))))

;;; Session Diagnostics

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

(defvar agent-shell-menu-info-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map help-mode-map)
    map)
  "Keymap for agent-shell info help buffers.")

;;;###autoload
(defun agent-shell-menu-session-info ()
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
        (insert (format "  Session suspended   %s\n" (if session-paused "yes" "no")))))
    (when-let* ((buf (get-buffer info-buf-name)))
      (with-current-buffer buf
        (let ((m (make-composed-keymap (make-sparse-keymap) agent-shell-menu-info-map)))
          (define-key m (kbd "g") (lambda () (interactive)
                                    (with-current-buffer shell-buf
                                      (agent-shell-menu-session-info))))
          (use-local-map m))))))

;;; Transient Prefix Menus

(defmacro agent-shell-menu-mode-key (key fn)
  "Define `agent-shell-menu-output-key-KEY' and bind it in `agent-shell-mode-map'.
In the output area, or while the shell is busy, calls FN interactively.
Self-inserts KEY only when at the idle prompt, unless queue-only mode is active
(in which case routes to `agent-shell-queue-ready-capture' instead).
Also binds FN directly in `agent-shell-viewport-view-mode-map'."
  (let* ((key-str (if (stringp key) key (symbol-name key)))
         (name (intern (concat "agent-shell-menu-output-key-" key-str)))
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
       (when (boundp 'agent-shell-viewport-view-mode-map)
         (define-key agent-shell-viewport-view-mode-map (kbd ,key-str) #',fn)))))

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
(defun agent-shell-menu--prompt-select-available-p ()
  "Return non-nil when `agent-shell-prompt-select' is available."
  (fboundp 'agent-shell-prompt-select))
(defun agent-shell-menu--interjection-p ()
  "Return non-nil when in an active interjection buffer."
  (derived-mode-p 'agent-shell-queue-interjection-mode))

(defun agent-shell-menu--session-shell-buffer ()
  "Return the agent-shell buffer for the current window context.
Works from both agent-shell buffers and viewport buffers."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((agent-shell-viewport--shell-buffer))))

(defun agent-shell-menu--session-permission-pending-p ()
  "Return non-nil when a permission is pending in the relevant shell buffer."
  (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
    (agent-shell--permission-pending-p :shell-buffer shell)))

(defun agent-shell-menu--in-session-p ()
  "Return non-nil when the current context has an associated agent-shell session."
  (not (null (agent-shell-menu--session-shell-buffer))))

(defun agent-shell-menu--in-shell-p ()
  "Return non-nil when currently in an agent-shell buffer (not a viewport)."
  (derived-mode-p 'agent-shell-mode))

(defun agent-shell-menu--session-permission-button-action (shell-buf pos)
  "Return command activating permission button at POS in SHELL-BUF."
  (lambda ()
    (interactive)
    (with-current-buffer shell-buf
      (when-let* ((cmd (agent-shell-menu--permission-action-at pos))
                  ((functionp cmd))
                  ((commandp cmd)))
        (save-excursion
          (goto-char pos)
          (call-interactively cmd))))))

(defun agent-shell-menu--permission-suffixes-for (prefix)
  "Return transient suffixes for each pending permission button under PREFIX.
Keys are assigned as 1, 2, 3… in button order."
  (when-let* ((shell (agent-shell-menu--session-shell-buffer))
              (buttons (with-current-buffer shell
                         (agent-shell-menu--permission-buttons))))
    (seq-map-indexed
     (lambda (btn i)
       (transient-parse-suffix
        prefix
        (list (number-to-string (1+ i))
              (format "Permission: %s" (car btn))
              (agent-shell-menu--session-permission-button-action shell (cdr btn)))))
     buttons)))

(defun agent-shell-menu--permission-suffixes (_group)
  "Return transient suffixes for each pending permission button.
Keys are assigned as 1, 2, 3… in button order."
  (agent-shell-menu--permission-suffixes-for 'agent-shell-menu-dispatch))

;;;###autoload
(transient-define-prefix agent-shell-menu-dispatch ()
  "agent-shell operations — navigate, act, send, queue, and session management."
  [:description "Permissions"
   :class transient-column
   :if agent-shell-menu--session-permission-pending-p
   :setup-children agent-shell-menu--permission-suffixes]
  ;; Session management (always), act/write/settings/fork (session-conditional)
  [["Sessions"
    ("sn" "New shell" agent-shell-new-shell)
    ("st" "New temp shell" agent-shell-new-temp-shell)
    ("sh" "Hydrate (resume)" agent-shell-resume-session)
    ("sd" "New in directory" agent-shell-menu-new-shell-in-dir)
    ("ss" "Switch session" agent-shell-switch-buffer)
    ("rr" "Review changes" agent-review
     :if agent-shell-menu--agent-review-available-p)
    ("rs" "Send issues to review agent" agent-review-send-to-agent-shell
     :if agent-shell-menu--agent-review-available-p)]
   ["Actions" :if agent-shell-menu--in-session-p
    ("aa" "Action menu" agent-shell-menu-select-action)
    ("ai" "Interrupt" agent-shell-interrupt)
    ("ar" "Resolve permission" agent-shell-menu-resolve-permission)
    ("ac" "Command menu" agent-shell-menu-select-command)
    ("ap" "Prompt library" agent-shell-prompt-select
     :if agent-shell-menu--prompt-select-available-p)
    ("ax" "Collapse menu" agent-shell-menu-select-collapse)
    ("ij" "Interject" agent-shell-queue-interject
     :inapt-if-not agent-shell-queue-interject-available-p)
    ("is" "Send interjection" agent-shell-queue-interjection-send
     :if agent-shell-menu--interjection-p)
    ("ic" "Close/Abort interjection" agent-shell-queue-interjection-close
     :if agent-shell-menu--interjection-p)]
   ["Settings" :if agent-shell-menu--in-session-p
    ("mm" "Set mode" agent-shell-menu-select-session-mode)
    ("mv" "Set model" agent-shell-set-session-model)
    ("mc" "Cycle mode" agent-shell-cycle-session-mode)
    ("mi" "Copy session ID" agent-shell-copy-session-id)
    ("mt" "Open transcript" agent-shell-open-transcript)]
   ["Fork" :if agent-shell-menu--in-session-p
    ("ff" "Fork session" agent-shell-fork)
    ("fo" "Other (project)" agent-shell-menu-switch-project-session)
    ("fq" "Fork queue" agent-shell-queue-fork-session)
    ("fb" "Insert fork before" agent-shell-queue-insert-fork-before)
    ("fa" "Insert fork after" agent-shell-queue-insert-fork-after)
    ("fr" "Release pending fork" agent-shell-queue-release-pending-fork)]]
  ;; Queue row: global queue ops, intercept config, and capture
  [["Send"
    ("wb" "Send buffer" agent-shell-menu-send-buffer)
    ("wt" "Send file to…" agent-shell-send-file-to)
    ("wo" "Send other file" agent-shell-send-other-file)
    ("wr" "Send region to…" agent-shell-send-region-to)
    ("wc" "Send screenshot to…" agent-shell-send-screenshot-to)
    ("wi" "Send clipboard image to…" agent-shell-send-clipboard-image-to)
    ("ws" "Send dwim" agent-shell-send-dwim)]
   ["Queue"
    ("qq" "Open queue" agent-shell-queue-buffer-open)
    ("qb" "Switch to queue" agent-shell-queue-buffer-switch)
    ("qe" "Enqueue" agent-shell-queue-enqueue)
    ("qd" "Edit task" agent-shell-queue-edit-task)
    ("qp" "Suspend all dispatch" agent-shell-queue-pause)
    ("qr" "Resume all dispatch" agent-shell-queue-resume)
    ("qu" "Resume all sessions" agent-shell-queue-unpause-all-sessions)]
   ["Capture"
    ("cw" "Compose (write)" agent-shell-queue-capture)
    ("cu" "Unassigned" agent-shell-queue-capture-unassigned)
    ("cr" "From region" agent-shell-queue-capture-from-region)
    ("cy" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("cc" "From context" agent-shell-queue-capture-from-context)]
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

(agent-shell-menu-mode-key "e" agent-shell-queue-capture-menu)

;; Wire menu key into queue mode map

(define-key agent-shell-queue-mode-map (kbd "M") #'agent-shell-menu-dispatch)

(provide 'agent-shell-menu)

;;; agent-shell-menu.el ends here
