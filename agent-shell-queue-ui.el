;;; agent-shell-queue-ui.el --- Tabulated list UI and interactive modes for agent-shell-queue -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (transient "0.4.0") (annotated-completing-read "0.1"))

;;; Commentary:

;; User interface layer for agent-shell-queue.  Provides the tabulated list
;; buffer (*agent-shell-queue*), item inspection and action menus, prompt
;; capture buffers, overlays, and interjection modes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'agent-shell-queue-core)
(require 'transient nil t)
(require 'annotated-completing-read nil t)

(declare-function agent-shell-queue-persistence-request-save "agent-shell-queue-persistence")
(declare-function yaml-parse-string "yaml")
(declare-function yaml-encode "yaml")
(declare-function agent-shell-menu--session-shell-buffer "agent-shell-menu")
(declare-function markdown-mode "markdown-mode")

(defvar agent-shell-queue-show-kind-column)
(defvar agent-shell-queue-show-ordinal-column)
(defvar agent-shell-queue-show-age-column)
(defvar agent-shell-queue-show-buffer-column)
(defvar agent-shell-queue-multiline-format)
(defvar agent-shell-queue--item-view-id)
(defvar agent-shell-queue-input-mode)
(defvar agent-shell-queue-input-mode-default)
(defvar agent-shell-queue-only-mode)




(defface agent-shell-queue-blocked-face
  '((t :foreground "darkorange3"))
  "Face for queue items that are paused, deferred, or blocked."
  :group 'agent-shell-queue)

(defface agent-shell-queue-unassigned-face
  '((t :foreground "cornflowerblue"))
  "Face for queue items not yet assigned to any shell."
  :group 'agent-shell-queue)

(defface agent-shell-queue-detached-face
  '((t :foreground "orange" :slant italic))
  "Face for queue items whose target shell buffer no longer exists."
  :group 'agent-shell-queue)

(defface agent-shell-queue-compact-face
  '((t :foreground "steelblue3"))
  "Face for compact (non-LLM manual) work items."
  :group 'agent-shell-queue)

(defface agent-shell-queue-blocked-question-face
  '((t :foreground "darkorange1" :weight bold))
  "Face for queue items blocked on a Human-in-the-Loop question."
  :group 'agent-shell-queue)
(defface agent-shell-queue-draft-face
  '((t :foreground "gray50" :slant italic))
  "Face for queue items saved as drafts (not yet queued for dispatch)."
  :group 'agent-shell-queue)
;;; Prompt Composition and Enqueueing

(defvar agent-shell-queue-capture-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-capture-cancel)
    (define-key m (kbd "C-c C-s") #'agent-shell-queue-capture-save-draft)
    (define-key m (kbd "C-c C-b") #'agent-shell-queue-capture-enable-background-task)
    (define-key m (kbd "C-c M-b") #'agent-shell-queue-capture-disable-background-task)
    (define-key m (kbd "C-c C-y") #'agent-shell-queue-capture-yank-kill)
    (define-key m (kbd "C-c M-w") #'agent-shell-queue-capture-yank-clipboard)
    (define-key m (kbd "C-c C-p") #'agent-shell-queue-capture-insert-thing-at-point)
    (define-key m (kbd "C-c C-x") #'agent-shell-queue-capture-select-context)
    (define-key m (kbd "C-c C-f") #'agent-shell-queue-insert-file)
    (define-key m (kbd "C-c M-f") #'agent-shell-queue-insert-buffer)
    m)
  "Keymap for `agent-shell-queue-capture-mode'.")

(define-derived-mode agent-shell-queue-capture-mode markdown-mode "Queue-Capture"
  "Mode for composing a queued `agent-shell' prompt.
\\{agent-shell-queue-capture-mode-map}"
  (setq-local electric-indent-inhibit t))

(defvar-local agent-shell-queue--capture-target nil
  "Target `agent-shell' buffer for this capture session.")

(defvar-local agent-shell-queue--capture-origin nil
  "Buffer from which capture was launched, used for context insertion.")

(defvar-local agent-shell-queue--capture-background-task nil
  "When non-nil, the captured prompt will be flagged for background execution.")

(defvar-local agent-shell-queue--capture-after-id nil
  "When non-nil, the confirmed item will be inserted after this item ID.")

(defvar-local agent-shell-queue--capture-draft-id nil
  "ID of the draft item saved from this capture buffer, or nil.
Set by `agent-shell-queue-capture-save-draft' and used to update rather
than duplicate the draft on subsequent saves.")

(defvar-local agent-shell-queue--capture-delay-before nil
  "Pre-dispatch delay in seconds for confirmed capture item.")

(defvar-local agent-shell-queue--capture-delay-after nil
  "Post-completion delay in seconds for confirmed capture item.")

(defvar-local agent-shell-queue--capture-kind 'prompt
  "Kind of queue item to create when this capture buffer is confirmed.
Defaults to `prompt'; set to `emacs-lisp' for Emacs Lisp capture buffers.")

(defun agent-shell-queue--open-capture (target-buf &optional origin-buf initial-content kind mode
                                                  delay-before delay-after)
  "Open a capture buffer targeting TARGET-BUF (nil for unassigned queue).
Multiple capture buffers can be open simultaneously; each is named after
its target.  ORIGIN-BUF is used for context commands; defaults to current
buffer.  INITIAL-CONTENT is inserted before display if non-nil.
KIND sets `agent-shell-queue--capture-kind' (defaults to `prompt').
MODE, when non-nil, is a major-mode function used instead of
`agent-shell-queue-capture-mode'; \\[agent-shell-queue-capture-confirm] and \\[agent-shell-queue-capture-cancel] are bound in mode map.
Optional DELAY-BEFORE and DELAY-AFTER specify per-task delays in seconds."
  (let* ((bucket-name (if target-buf
                          (buffer-name target-buf)
                        agent-shell-queue--unassigned-key))
         (kind-label (when (and kind (not (eq kind 'prompt)))
                       (concat "  |  " (symbol-name kind))))
         (capture-buf (get-buffer-create
                       (if target-buf
                           (format "*agent-shell-queue-capture: %s*" (buffer-name target-buf))
                         "*agent-shell-queue-capture: unassigned*"))))
    (with-current-buffer capture-buf
      (erase-buffer)
      (if mode
          (progn
            (funcall mode)
            (use-local-map (copy-keymap (current-local-map)))
            (local-set-key (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
            (local-set-key (kbd "C-c C-k") #'agent-shell-queue-capture-cancel))
        (agent-shell-queue-capture-mode))
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (or origin-buf (current-buffer))
            agent-shell-queue--capture-background-task nil
            agent-shell-queue--capture-after-id nil
            agent-shell-queue--capture-kind (or kind 'prompt)
            agent-shell-queue--capture-delay-before delay-before
            agent-shell-queue--capture-delay-after delay-after)
      (when (and initial-content (not (string-empty-p initial-content)))
        (insert initial-content))
      (let* ((bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
             (depth (agent-shell-queue--active-item-count bucket-items))
             (state (agent-shell-queue--activity-state)))
        (setq-local header-line-format
                    (concat
                     (propertize (format " %s%s  |  " bucket-name (or kind-label "")) 'face 'shadow)
                     state
                     (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

(defun agent-shell-queue-capture-confirm ()
  "Confirm capture: queue the buffer contents and close."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task)
        (after-id agent-shell-queue--capture-after-id)
        (kind agent-shell-queue--capture-kind)
        (delay-before agent-shell-queue--capture-delay-before)
        (delay-after agent-shell-queue--capture-delay-after))
    (let ((use-blocked (agent-shell-queue--capture-plan-mode-choice buf)))
      (agent-shell-queue--close-capture-window)
      (unless (string-empty-p prompt)
        (message "agent-shell: %s" prompt)
        (cond
         (after-id
          (when-let* ((pair (agent-shell-queue--item-by-id after-id))
                      (bucket-name (car pair))
                      (item (agent-shell-queue--make-item prompt bg kind delay-before delay-after)))
            (when use-blocked
              (setf (agent-shell-queue-item-status item) 'blocked.skip))
            (let ((items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store)))))
              (if-let* ((idx (cl-position after-id items
                                          :key #'agent-shell-queue-item-id :test #'equal))
                        (cell (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
                  (setcdr cell (append (cl-subseq items 0 (1+ idx))
                                       (list item)
                                       (cl-subseq items (1+ idx))))
                (agent-shell-queue--add-item-to-bucket bucket-name item)))
            (when buf (agent-shell-queue--ensure-subscription buf))
            (agent-shell-queue--save)
            (agent-shell-queue--refresh-buffer)))
         (buf
          (with-agent-shell-queue
            (let ((item (agent-shell-queue--make-item prompt bg kind delay-before delay-after)))
              (when use-blocked
                (setf (agent-shell-queue-item-status item) 'blocked.skip))
              (setf (agent-shell-queue-item-directory item)
                    (buffer-local-value 'default-directory buf))
              (agent-shell-queue--add-item-to-bucket (buffer-name buf) item)
              (agent-shell-queue--ensure-subscription buf)))
          (unless use-blocked
            (agent-shell-queue--send-next-for-buffer buf)))
         (t
          (with-agent-shell-queue
            (let ((item (agent-shell-queue--make-item prompt bg kind delay-before delay-after)))
              (when use-blocked
                (setf (agent-shell-queue-item-status item) 'blocked.skip))
              (agent-shell-queue--add-item-to-bucket agent-shell-queue--unassigned-key item)))))))))

(defun agent-shell-queue-capture-cancel ()
  "Discard the capture buffer without queuing."
  (interactive)
  (agent-shell-queue--close-capture-window))

(defun agent-shell-queue-capture-save-draft ()
  "Save capture buffer contents as a draft queue item without closing.
The item is stored with `draft' status and skipped by dispatch.
If a draft was previously saved from this buffer it is updated in place."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (buf agent-shell-queue--capture-target)
        (bg agent-shell-queue--capture-background-task))
    (when (string-empty-p prompt)
      (user-error "Buffer is empty — nothing to save as draft"))
    (if-let* ((draft-id agent-shell-queue--capture-draft-id)
              (_ (agent-shell-queue--item-by-id draft-id)))
        (progn
          (agent-shell-queue-edit draft-id prompt)
          (message "agent-shell-queue: draft updated (%s)" draft-id))
      (let* ((item (agent-shell-queue--make-item prompt bg))
             (bucket-name (if buf (buffer-name buf) agent-shell-queue--unassigned-key)))
        (setf (agent-shell-queue-item-status item) 'draft)
        (agent-shell-queue--ensure-loaded)
        (agent-shell-queue--add-item-to-bucket bucket-name item)
        (when buf (agent-shell-queue--ensure-subscription buf))
        (agent-shell-queue--save)
        (agent-shell-queue--refresh-buffer)
        (setq agent-shell-queue--capture-draft-id (agent-shell-queue-item-id item))
        (message "agent-shell-queue: draft saved (%s)" agent-shell-queue--capture-draft-id)))))

(defun agent-shell-queue-capture-enable-background-task ()
  "Flag this capture for background sub-agent execution."
  (interactive)
  (setq agent-shell-queue--capture-background-task t)
  (message "Background: on"))

(defun agent-shell-queue-capture-disable-background-task ()
  "Clear the background sub-agent flag from this capture."
  (interactive)
  (setq agent-shell-queue--capture-background-task nil)
  (message "Background: off"))

(defun agent-shell-queue-capture-yank-kill ()
  "Insert the most recent `kill-ring' entry at point."
  (interactive)
  (when kill-ring (insert (car kill-ring))))

(defun agent-shell-queue-capture-yank-clipboard ()
  "Insert the current clipboard contents at point."
  (interactive)
  (when-let* ((sel (ignore-errors (gui-get-selection 'CLIPBOARD))))
    (insert sel)))

(defun agent-shell-queue-capture-insert-thing-at-point ()
  "Insert the thing at point from the buffer that opened this capture."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (thing (with-current-buffer origin
                       (or (thing-at-point 'url t)
                           (thing-at-point 'filename t)
                           (thing-at-point 'symbol t)
                           (thing-at-point 'word t)))))
    (insert thing)))

(defun agent-shell-queue-capture-select-context ()
  "Select a string from origin buffer context via ACR and insert it."
  (interactive)
  (when-let* ((origin agent-shell-queue--capture-origin)
              (_ (buffer-live-p origin))
              (text (with-current-buffer origin
                      (annotated-completing-read-context-from-point
                       :prompt "insert context: "
                       :history 'agent-shell-queue-capture-select-context)))
              (_ (not (string-empty-p text))))
    (insert text)))

(defun agent-shell-queue-insert-file ()
  "Prompt for a file and insert its contents at point.
Works in both capture and edit buffers."
  (interactive)
  (let ((file (read-file-name "Insert file: ")))
    (when (file-readable-p file)
      (insert-file-contents file))))

(defun agent-shell-queue-insert-buffer ()
  "Pick a buffer and insert its entire contents at point.
Works in both capture and edit buffers."
  (interactive)
  (when-let* ((name (annotated-completing-read
		     (map-into
		      (seq-map (lambda (buf)
				 (cons (buffer-name buf)
				       (with-current-buffer buf
					 (format "%-20s %s"
						 (symbol-name major-mode)
						 (or (buffer-file-name) "")))))
			       (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b)))
					   (buffer-list)))
		      '(hash-table :test equal))
                     :prompt "Insert buffer: "
                     :require-match t))
              (buf (get-buffer name)))
    (insert (with-current-buffer buf (buffer-string)))))

;;;###autoload
(defun agent-shell-queue-capture (&optional buf)
  "Open a capture buffer targeting BUF (nil add to the unassigned queue).
When called interactively from an `agent-shell' buffer, targets that buffer.
With a prefix argument, opens an unassigned capture instead."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture buf (current-buffer)))

;;;###autoload
(defun agent-shell-queue-enqueue (prompt &optional buf background delay-before delay-after)
  "Queue PROMPT for BUF, optionally flagged for BACKGROUND execution.
Send immediately if BUF is idle and no DELAY-BEFORE is set, otherwise
store in the queue.  When called interactively, opens a capture buffer
for composing the prompt.  Optional DELAY-BEFORE and DELAY-AFTER specify
pre-dispatch and post-completion delays in seconds."
  (interactive
   (let ((target (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                     (agent-shell-queue--pick-buffer "Enqueue to: "))))
     (agent-shell-queue--open-capture target (current-buffer))
     (list nil nil nil nil nil)))
  (when-let* ((buf (and prompt
                        (or buf
                            (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                            (agent-shell-queue--pick-buffer "Enqueue to: ")))))
    (with-current-buffer buf
      (if (or (shell-maker-busy)
              (and delay-before (numberp delay-before) (> delay-before 0)))
          (agent-shell-queue-add prompt buf background delay-before delay-after)
        (agent-shell-insert
         :text (if background
                   (concat (agent-shell-queue--get-background-prefix buf) prompt)
                 prompt)
         :submit t
         :no-focus t)))))

;;;###autoload
(defun agent-shell-queue-enqueue-clear (&optional buf)
  "Enqueue a clear command for BUF.
Uses the resolved clear command for BUF as the prompt."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Clear queue for: "))))
  (agent-shell-queue-enqueue (agent-shell-queue--get-clear-command buf) buf))

;;;###autoload
(defun agent-shell-queue-capture-unassigned ()
  "Open a capture buffer to compose a prompt for the unassigned queue.
Unassigned items display in blue and can later be assigned to a shell via key t."
  (interactive)
  (agent-shell-queue--open-capture nil (current-buffer)))

(defun agent-shell-queue-buffer-capture-after ()
  "Open a capture buffer for an item to be inserted after the item at point.
The confirmed item is spliced into the queue immediately after the current row,
rather than appended to the end."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id)))
    (let ((bucket-name (car pair)))
      (with-current-buffer
          (agent-shell-queue--open-capture
           (unless (equal bucket-name agent-shell-queue--unassigned-key)
             (get-buffer bucket-name))
           (current-buffer))
        (setq agent-shell-queue--capture-after-id id)))))

;;;###autoload
(defun agent-shell-queue-capture-from-region (&optional buf)
  "Open a capture buffer pre-seeded with the active region text.
When no region is active, opens an empty capture.  BUF is the target
`agent-shell' buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture buf (current-buffer)
                                   (when (use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end)))))

;;;###autoload
(defun agent-shell-queue-capture-from-context (&optional buf)
  "Open a capture buffer pre-seeded with a string selected from context.
Candidates include `thing-at-point', active region, current line, and
kill ring.  BUF is the target buffer; nil for unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (let ((text (annotated-completing-read-context-from-point
               :prompt "seed capture: "
               :history 'agent-shell-queue-capture-from-context)))
    (agent-shell-queue--open-capture
     buf (current-buffer)
     (unless (string-empty-p text)
       text))))

(defun agent-shell-queue-capture-from-clipboard (&optional buf)
  "Open a capture buffer pre-seeded with the current clipboard contents.
BUF is the target `agent-shell' buffer; nil adds to the unassigned queue."
  (interactive
   (list (cond
          (current-prefix-arg nil)
          ((derived-mode-p 'agent-shell-mode) (current-buffer))
          (t (agent-shell-queue--pick-buffer "Capture for: ")))))
  (agent-shell-queue--open-capture
   buf (current-buffer)
   (ignore-errors
     (gui-get-selection 'CLIPBOARD))))

;;;###autoload
(defun agent-shell-queue-enqueue-directory (dir)
  "Open a capture buffer for directory queue DIR."
  (interactive (list (read-directory-name "Directory queue: ")))
  (let ((default-directory (agent-shell-queue--canonicalize-dir dir)))
    (agent-shell-queue-enqueue nil)))

;;;###autoload
(defun agent-shell-queue-enqueue-emacs (buf)
  "Open an Emacs Lisp capture buffer to compose a form for BUF's queue.
The capture buffer is in `emacs-lisp-mode'.
Confirm with \\[agent-shell-queue-capture-confirm],
cancel with \\[agent-shell-queue-capture-cancel].  When dispatched, evaluated via
`eval'; errors are reported as messages and the item is marked done.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'emacs-lisp "Target (or unassigned): ")))
  (if buf
      (agent-shell-queue--open-elisp-capture buf)
    (agent-shell-queue--open-capture nil nil nil 'emacs-lisp 'emacs-lisp-mode)))

;;;###autoload
(defun agent-shell-queue-enqueue-emacs-command (command buf)
  "Enqueue an interactive COMMAND to run in Emacs for BUF's queue.
COMMAND is selected via `read-command' (completing-read over all commands).
When dispatched, the command is invoked with `call-interactively'.
BUF may be nil to enqueue to the unassigned bucket."
  (interactive
   (list (read-command "Emacs command: ")
         (agent-shell-queue--pick-buffer-for-kind 'emacs-command "Target (or unassigned): ")))
  (agent-shell-queue--enqueue-args (symbol-name command) 'emacs-command buf))

;;;###autoload
(defun agent-shell-queue-enqueue-shell-eshell (buf)
  "Open a shell capture buffer to compose a command for eshell BUF.
The buffer is in `sh-mode'.  Confirm with \\[agent-shell-queue-capture-confirm].
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'shell-eshell "eshell buffer (or unassigned): ")))
  (agent-shell-queue--open-capture buf nil nil 'shell-eshell 'sh-mode))

;;;###autoload
(defun agent-shell-queue-enqueue-shell-eat (buf)
  "Open a shell capture buffer to compose a command for eat BUF.
The capture buffer is in `sh-mode'.  Confirm with \\[agent-shell-queue-capture-confirm].
BUF may be nil to enqueue to the unassigned bucket."
  (interactive (list (agent-shell-queue--pick-buffer-for-kind 'shell-eat "eat buffer (or unassigned): ")))
  (agent-shell-queue--open-capture buf nil nil 'shell-eat 'sh-mode))

;; Capture buffers

(defun agent-shell-queue--open-elisp-capture (target-buf)
  "Open an Emacs Lisp capture buffer targeting TARGET-BUF's queue.
The buffer is in `emacs-lisp-mode' with capture confirm/cancel bindings.
Items created from this buffer have kind `emacs-lisp'."
  (let* ((capture-buf (get-buffer-create
                       (format "*agent-shell-queue-elisp: %s*"
                               (buffer-name target-buf))))
         (bucket-name (buffer-name target-buf)))
    (with-current-buffer capture-buf
      (erase-buffer)
      (emacs-lisp-mode)
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key (kbd "C-c C-c") #'agent-shell-queue-capture-confirm)
      (local-set-key (kbd "C-c C-k") #'agent-shell-queue-capture-cancel)
      (setq agent-shell-queue--capture-target target-buf
            agent-shell-queue--capture-origin (current-buffer)
            agent-shell-queue--capture-background-task nil
            agent-shell-queue--capture-after-id nil
            agent-shell-queue--capture-kind 'emacs-lisp)
      (let* ((bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
             (depth (agent-shell-queue--active-item-count bucket-items))
             (state (agent-shell-queue--activity-state)))
        (setq-local header-line-format
                    (concat
                     (propertize (format " %s  |  emacs-lisp  |  " bucket-name) 'face 'shadow)
                     state
                     (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
    (pop-to-buffer capture-buf '(display-buffer-below-selected))
    capture-buf))

(defun agent-shell-queue--close-capture-window ()
  "Delete the current capture window and kill its buffer.
Explicitly deletes the window first so the split disappears regardless of
how the window was opened (i.e. independent of `quit-restore' state)."
  (let ((win (selected-window))
        (buf (current-buffer)))
    (if (and (not (one-window-p)) (window-deletable-p win))
        (progn (delete-window win) (kill-buffer buf))
      (kill-buffer buf))))

(defun agent-shell-queue--capture-plan-mode-choice (buf)
  "When BUF's session is in a blocking mode, prompt for what to do.
Returns non-nil if the item should be queued as `blocked.skip'.
If the user chooses \"switch mode\", calls `agent-shell-set-session-mode'
Cancelling (\\`C-g\\') defaults to \"queue as blocked\"."
  (when (and buf (agent-shell-queue--session-mode-blocked-p buf))
    (let* ((mode-id (map-nested-elt (buffer-local-value 'agent-shell--state buf)
                                    '(:session :mode-id)))
           (choice (condition-case nil
                       (completing-read
                        (format "Session in %s mode: " mode-id)
                        '("queue as blocked" "switch mode")
                        nil t nil nil "queue as blocked")
                     (quit "queue as blocked"))))
      (if (equal choice "switch mode")
          (progn
            (with-current-buffer buf
              (call-interactively #'agent-shell-set-session-mode))
            nil)
        t))))



;;; Queue Buffer and Navigation

(defun agent-shell-queue--get-or-create-buffer ()
  "Return the *agent-shell-queue* buffer, initializing it if needed."
  (let ((buf (get-buffer-create "*agent-shell-queue*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-queue-mode)
        (agent-shell-queue-mode))
      (agent-shell-queue-buffer-refresh))
    buf))

;;;###autoload
(defun agent-shell-queue-buffer-open ()
  "Open (or refresh) the *agent-shell-queue* buffer."
  (interactive)
  (pop-to-buffer (agent-shell-queue--get-or-create-buffer)))

;;;###autoload
(defun agent-shell-queue-buffer-switch ()
  "Switch to the *agent-shell-queue* buffer in the current window."
  (interactive)
  (switch-to-buffer (agent-shell-queue--get-or-create-buffer)))

(defun agent-shell-queue--scope-label (scope)
  "Return a short human-readable string for SCOPE."
  (pcase scope
    ('nil "global")
    (`(buffer . ,name) (format "buffer:%s" name))
    (`(directory . ,dir) (abbreviate-file-name dir))))

(defun agent-shell-queue--scope-matches-p (buf-name scope)
  "Return non-nil if BUF-NAME belongs to SCOPE.
The unassigned bucket only matches the global scope."
  (pcase scope
    ('nil t)
    (`(buffer . ,name) (equal buf-name name))
    (`(directory . ,dir)
     (and (not (equal buf-name agent-shell-queue--unassigned-key))
          (or (and (agent-shell-queue--dir-bucket-p buf-name)
                   (string-prefix-p (expand-file-name dir)
                                    (expand-file-name (agent-shell-queue--dir-from-bucket buf-name))))
              (when-let* ((buf (get-buffer buf-name)))
                (string-prefix-p (expand-file-name dir)
                                 (expand-file-name
                                  (buffer-local-value 'default-directory buf)))))))))

(defun agent-shell-queue--scope-candidates ()
  "Return an alist of (LABEL . SCOPE) covering global, directories, and buffers.
Directories are derived from live shell buffers and directory queue buckets."
  (let* ((assigned (seq-remove (lambda (it)
                                 (equal (car it) agent-shell-queue--unassigned-key))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (buf-entries (seq-map (lambda (it) (cons (car it) (cons 'buffer (car it)))) assigned))
         (live-dirs (thread-last assigned
                      (seq-filter (lambda (it) (buffer-live-p (get-buffer (car it)))))
                      (seq-map (lambda (it)
                                 (expand-file-name
                                  (buffer-local-value 'default-directory (get-buffer (car it))))))))
         (bucket-dirs (thread-last assigned
                        (seq-filter (lambda (it) (agent-shell-queue--dir-bucket-p (car it))))
                        (seq-map (lambda (it)
                                   (expand-file-name (agent-shell-queue--dir-from-bucket (car it)))))))
         (dirs (seq-uniq (append live-dirs bucket-dirs))))
    (append
     (list (cons "global (all)" nil))
     (seq-map (lambda (it) (cons (abbreviate-file-name it) (cons 'directory it)))
              (sort dirs #'string<))
     buf-entries)))

(defun agent-shell-queue--active-item-count (items)
  "Return the count of ITEMS whose status is not `done'."
  (seq-count (lambda (item)
               (not (eq (agent-shell-queue-item-status item) 'done)))
             items))

;;;###autoload
(defun agent-shell-queue-set-scope ()
  "Narrow the queue buffer view: choose global, a directory, or a specific buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((candidates (agent-shell-queue--scope-candidates))
         (table (seq-map
                 (lambda (cand)
                   (let* ((scope (cdr cand))
                          (count (apply #'+
                                        (seq-map (lambda (it)
                                                   (if (agent-shell-queue--scope-matches-p (car it) scope)
                                                       (agent-shell-queue--active-item-count (cdr it))
                                                     0))
                                                 (agent-shell-queue-store-items agent-shell-queue--store)))))
                     (cons (car cand) (format "%d item(s)" count))))
                 candidates)))
    (let* ((label (annotated-completing-read table
                                             :prompt "queue scope => "
                                             :category 'agent-shell-queue-scope
                                             :require-match t
                                             :history 'agent-shell-queue-set-scope))
           (scope (cdr (assoc label candidates))))
      (setq-local agent-shell-queue--display-scope scope)
      (agent-shell-queue-buffer-refresh)
      (force-mode-line-update))))

;;;###autoload
(defun agent-shell-queue-scope-global ()
  "Reset the queue buffer to the global scope (show all items)."
  (interactive)
  (setq-local agent-shell-queue--display-scope nil)
  (agent-shell-queue-buffer-refresh)
  (force-mode-line-update))

(defvar agent-shell-queue-mode-map
  (let ((m (make-sparse-keymap)))
    ;; View / send / remove
    (define-key m (kbd "RET")      #'agent-shell-queue-buffer-view-item)
    (define-key m (kbd "C-c C-s")  #'agent-shell-queue-buffer-send)
    (define-key m (kbd "C-K")      #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "C-<DEL>")  #'agent-shell-queue-buffer-remove)
    (define-key m (kbd "C-c C-r")  #'agent-shell-queue-buffer-reenqueue)
    (define-key m (kbd "C-A")      #'agent-shell-queue-buffer-archive)
    (define-key m (kbd "z")        #'agent-shell-queue-buffer-mark-done)
    ;; Edit / enqueue
    (define-key m (kbd "e")        #'agent-shell-queue-enqueue-dispatch)
    (define-key m (kbd "C-e")      #'agent-shell-queue-edit-task)
    ;; Pause / schedule (suspend item from auto-dispatch without removing)
    (define-key m (kbd "p")        #'agent-shell-queue-buffer-pause)
    (define-key m (kbd "r")        #'agent-shell-queue-buffer-schedule)
    ;; Background flag
    (define-key m (kbd "b")        #'agent-shell-queue-buffer-enable-background-task)
    (define-key m (kbd "B")        #'agent-shell-queue-buffer-disable-background-task)
    ;; Move / assign
    (define-key m (kbd "a")        #'agent-shell-queue-buffer-assign)
    (define-key m (kbd "M-<up>")   #'agent-shell-queue-buffer-move-up)
    (define-key m (kbd "M-<down>") #'agent-shell-queue-buffer-move-down)
    ;; Pause / resume session queue dispatch
    (define-key m (kbd "C-c C-p")        #'agent-shell-queue-session-pause)
    (define-key m (kbd "C-c C-r")        #'agent-shell-queue-session-resume)
    (define-key m (kbd "C-c C-x")        #'agent-shell-queue-recover-stuck-shell)
    ;; Interjection
    (define-key m (kbd "i")        #'agent-shell-queue-interject)
    ;; Insert items
    (define-key m (kbd "C-d p")    #'agent-shell-queue-insert-pause)
    (define-key m (kbd "C-d C-c")  #'agent-shell-queue-insert-clear-context)
    (define-key m (kbd "C-w")      #'agent-shell-queue-insert-wait)
    (define-key m (kbd "C-d c")    #'agent-shell-queue-insert-compact)
    ;; Capture entry points
    (define-key m (kbd "c")        #'agent-shell-queue-capture)
    (define-key m (kbd "a")        #'agent-shell-queue-buffer-capture-after)
    (define-key m (kbd "u")        #'agent-shell-queue-capture-unassigned)
    (define-key m (kbd "y")        #'agent-shell-queue-capture-from-clipboard)
    ;; Navigation / display
    (define-key m (kbd "<down>")   #'agent-shell-queue-next-item)
    (define-key m (kbd "<up>")     #'agent-shell-queue-prev-item)
    (define-key m (kbd "TAB")      #'agent-shell-queue-buffer-jump-to-next)
    (define-key m (kbd "g")        #'agent-shell-queue-buffer-refresh)
    (define-key m (kbd "M-r")      #'agent-shell-queue-reload)
    (define-key m (kbd "D")        #'agent-shell-queue-show-disk-state)
    ;; Scope / narrowing
    (define-key m (kbd "n")        #'agent-shell-queue-set-scope)
    (define-key m (kbd "w")        #'agent-shell-queue-scope-global)
    (define-key m (kbd "SPC")      #'agent-shell-queue-buffer-context-menu)
    (define-key m (kbd "C-d x")    #'agent-shell-queue-raw-edit)
    (define-key m (kbd "C-d i")    #'agent-shell-queue-import)
    (define-key m (kbd "o")        #'agent-shell-queue-buffer-open-shell)
    (define-key m (kbd "C-d a")    #'agent-shell-queue-buffer-abort)
    (define-key m (kbd "C-v")      #'agent-shell-queue-select-columns)
    (define-key m (kbd "=")        #'agent-shell-queue-buffer-inspect-item)
    (define-key m (kbd "m")        #'agent-shell-queue-dispatch)
    (define-key m (kbd "?")        #'describe-bindings)
    (define-key m (kbd "q")        #'quit-window)
    m)
  "Keymap for `agent-shell-queue-mode'.")

(defvar-local agent-shell-queue--last-column-structure nil
  "Column structure key from the last `tabulated-list-init-header' call.
A list of (show-buffer-p show-ordinal-p show-age-p) used to avoid
reinitializing headers on pure content refreshes.")

(defun agent-shell-queue--on-queue-buffer-kill ()
  "Clean up in-flight items when the queue display buffer is killed.
Running items are marked aborted; active (scheduled) items are blocked.skip.
This prevents tasks from executing without any supervisory display."
  (agent-shell-queue--ensure-loaded)
  (seq-do (lambda (bucket)
            (seq-do (lambda (item)
                      (pcase (agent-shell-queue-item-status item)
                        ('running
                         (setf (agent-shell-queue-item-status item) 'aborted)
                         (setf (agent-shell-queue-item-outcome item) 'canceled))
                        ('active
                         (setf (agent-shell-queue-item-status item) 'blocked.skip))))
                    (cdr bucket)))
          (agent-shell-queue-store-items agent-shell-queue--store))
  (agent-shell-queue--save))

(define-derived-mode agent-shell-queue-mode tabulated-list-mode "Queue"
  "Major mode for reviewing and managing the `agent-shell' prompt queue."
  (setq tabulated-list-format
        (agent-shell-queue--column-format t (agent-shell-queue--prompt-width t)))
  (setq agent-shell-queue--last-column-structure
        (list t agent-shell-queue-show-kind-column agent-shell-queue-show-ordinal-column agent-shell-queue-show-age-column))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tab-line-mode 1)
  (setq tab-line-format
        '(:eval (let* ((state (agent-shell-queue--activity-state))
                       (sessions (length (agent-shell-buffers)))
                       (scope agent-shell-queue--display-scope)
                       (visible-items
                        (seq-filter
                         (lambda (pair)
                           (agent-shell-queue--scope-matches-p (car pair) scope))
                         (agent-shell-queue-store-items agent-shell-queue--store)))
                       (depth (apply #'+
                                     (seq-map (lambda (it)
                                                (agent-shell-queue--active-item-count (cdr it)))
                                              visible-items)))
                       (scope-display
                        (pcase scope
                          ('nil nil)
                          (`(buffer . ,name) (format "  |  Buffer: %s" name))
                          (_ (format "  |  Scope: %s"
                                     (agent-shell-queue--scope-label scope)))))
                       (flush-display
                        (if agent-shell-queue--last-flush-time
                            (format "%s ago" (agent-shell-queue--format-age
                                             (time-since agent-shell-queue--last-flush-time)))
                          "never"))
                       (next-display
                        (when agent-shell-queue--next-flush-time
                          (let ((remaining (float-time (time-subtract
                                                        agent-shell-queue--next-flush-time
                                                        (current-time)))))
                            (when (> remaining 0)
                              (format "  |  Next sync in %s"
                                      (agent-shell-queue--format-age
                                       (seconds-to-time remaining))))))))
                  (format " Queue: %s  |  Sessions: %d  |  Depth: %d%s  |  Flushed: %s%s"
                          state sessions depth
                          (or scope-display "")
                          flush-display (or next-display "")))))
  (add-hook 'kill-buffer-hook #'agent-shell-queue--on-queue-buffer-kill nil t))

(defun agent-shell-queue--activity-state ()
  "Return a propertized string describing the queue's current activity level."
  (cond
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'running))))
    (propertize "running" 'face 'success))
   ((thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                 (seq-mapcat #'cdr)
                 (seq-some (lambda (it) (eq (agent-shell-queue-item-status it) 'active))))
    (propertize "waiting" 'face 'font-lock-comment-face))
   (t
    (propertize "idle" 'face 'shadow))))

(defun agent-shell-queue--buffer-state-label (buf-name)
  "Return a short queue-state string for BUF-NAME's bucket.
Examples: \"paused\", \"running (2)\", \"3 pending\", \"idle\"."
  (let* ((paused-p (member buf-name
                           (agent-shell-queue-queue-session-paused
                            agent-shell-queue--queue)))
         (halted-p (agent-shell-queue--halted-on-abort-p buf-name))
         (items (cdr (assoc buf-name
                            (agent-shell-queue-store-items
                             agent-shell-queue--store))))
         (running (seq-some (lambda (it)
                              (eq (agent-shell-queue-item-status it) 'running))
                            items))
         (pending (seq-count (lambda (it)
                               (eq (agent-shell-queue-item-status it) 'active))
                             items)))
    (cond
     ((and halted-p (> pending 0)) (format "halted (abort: %d)" pending))
     (halted-p "halted (abort)")
     ((and paused-p (> pending 0)) (format "paused (%d)" pending))
     (paused-p "paused")
     ((and running (> pending 0)) (format "running (%d)" pending))
     (running "running")
     ((> pending 0) (format "%d pending" pending))
     (t "idle"))))

(defconst agent-shell-queue--status-column-width
  (- (max 6 (apply #'max
                   (seq-map #'length
                            '("invalid" "pending-fork" "done" "running.blocked"
                              "aborted" "running.active.bg" "running.active"
                              "editing" "blocked.runner" "blocked.task"
                              "blocked.dep" "blocked.cond" "blocked.pending" "blocked.skip"
                              "halted.abort" "draft" "scheduled.bg" "scheduled" "incomplete"))))
     3)
  "Width of Status column: max(6, longest status string) minus 3.")

(defconst agent-shell-queue--kind-column-width
  (apply #'max (seq-map #'length
                        '("agent-shell-prompt" "emacs-lisp" "emacs-command"
                          "context" "wait" "pause" "compact" "Kind")))
  "Width of the Kind column.")

(defun agent-shell-queue--column-format (show-buffer-p pw)
  "Build the `tabulated-list-format' vector for current display settings.
SHOW-BUFFER-P controls whether the Buffer column is included.
PW is the width allocated to the Prompt column."
  (let (cols)
    (push (list "Status" agent-shell-queue--status-column-width t) cols)
    (when agent-shell-queue-show-kind-column
      (push (list "Kind" agent-shell-queue--kind-column-width t) cols))
    (when show-buffer-p
      (push (list "Buffer" 19 t) cols))
    (when agent-shell-queue-show-ordinal-column
      (push (list "#" 4 nil) cols))
    (when agent-shell-queue-show-age-column
      (push (list "Age" 5 t) cols))
    (push (list "Prompt" pw nil) cols)
    (apply #'vector (nreverse cols))))

(defun agent-shell-queue--prompt-width (show-buffer-p)
  "Compute available width for the Prompt column.
SHOW-BUFFER-P indicates whether the Buffer column is included."
  (max 20 (- (window-width)
             (+ agent-shell-queue--status-column-width
                (if agent-shell-queue-show-kind-column (1+ agent-shell-queue--kind-column-width) 0)
                (if show-buffer-p 19 0)
                (if agent-shell-queue-show-ordinal-column 4 0)
                (if agent-shell-queue-show-age-column 5 0)
                ;; tabulated-list adds one space between columns
                (+ 1
                   (if agent-shell-queue-show-kind-column 1 0)
                   (if show-buffer-p 1 0)
                   (if agent-shell-queue-show-ordinal-column 1 0)
                   (if agent-shell-queue-show-age-column 1 0))))))

(defun agent-shell-queue--ordered-display-items ()
  "Return the display-ordered bucket list for the current scope.
Filters store items to those matching `agent-shell-queue--display-scope',
then places the unassigned bucket last."
  (let* ((scope agent-shell-queue--display-scope)
         (visible (seq-filter (lambda (it)
                                (agent-shell-queue--scope-matches-p (car it) scope))
                              (agent-shell-queue-store-items agent-shell-queue--store)))
         (unassigned (assoc agent-shell-queue--unassigned-key visible))
         (assigned (seq-remove (lambda (it) (equal (car it) agent-shell-queue--unassigned-key))
                               visible)))
    (if unassigned
        (append assigned (list unassigned))
      assigned)))

(defun agent-shell-queue-buffer-refresh ()
  "Rebuild the tabulated list from current queue state."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((ordered (seq-map #'agent-shell-queue--sanitize-bucket
                           (agent-shell-queue--ordered-display-items)))
         (show-buffer-p agent-shell-queue-show-buffer-column)
         (column-structure (list show-buffer-p
                                 agent-shell-queue-show-kind-column
                                 agent-shell-queue-show-ordinal-column
                                 agent-shell-queue-show-age-column))
         (next-id-map (seq-map (lambda (it)
                                 (cons (car it)
                                       (when-let* ((next (agent-shell-queue--next-dispatchable-item
                                                          (cdr it))))
                                         (agent-shell-queue-item-id next))))
                               (agent-shell-queue-store-items agent-shell-queue--store)))
         (pw (agent-shell-queue--prompt-width show-buffer-p)))
    (unless (equal column-structure agent-shell-queue--last-column-structure)
      (setq agent-shell-queue--last-column-structure column-structure)
      (setq tabulated-list-format (agent-shell-queue--column-format show-buffer-p pw))
      (tabulated-list-init-header))
    (setq tabulated-list-entries
          (thread-last ordered
            (seq-mapcat
             (lambda (pair)
               (seq-map
                (lambda (item)
                  (let* ((id (agent-shell-queue-item-id item))
                         (next-p (equal id (cdr (assoc (car pair) next-id-map))))
                         (display (agent-shell-queue--item-display item (car pair) next-p))
                         (status-str (car display))
                         (face (cdr display))
                         (cell (lambda (str) (if face (propertize str 'face face) str)))
                         (idx (cl-position id
                                           (cdr (assoc (car pair) (agent-shell-queue-store-items agent-shell-queue--store)))
                                           :key #'agent-shell-queue-item-id :test #'equal))
                         (ordinal (if idx (1+ idx) 0))
                         (status (agent-shell-queue-item-status item))
                         (dispatched (agent-shell-queue-item-dispatched item))
                         (completed (agent-shell-queue-item-completed item))
                         (age-str (cond
                                   ((and (eq status 'done) dispatched completed)
                                    (agent-shell-queue--format-age
                                     (time-subtract completed dispatched)))
                                   ((and (eq status 'running) dispatched)
                                    (agent-shell-queue--format-age (time-since dispatched)))
                                   (t "")))
                         (first-line (car (split-string
                                           (agent-shell-queue-item-args item) "\n")))
                         (buf-cell (funcall cell
                                            (if (equal (car pair) agent-shell-queue--unassigned-key)
                                                "(unassigned)" (car pair))))
                         (kind-str (agent-shell-queue--item-kind-string item))
                         (row (let (cols)
                                (push (funcall cell status-str) cols)
                                (when agent-shell-queue-show-kind-column
                                  (push (funcall cell kind-str) cols))
                                (when show-buffer-p (push buf-cell cols))
                                (when agent-shell-queue-show-ordinal-column
                                  (push (funcall cell (if (> ordinal 0)
                                                          (number-to-string ordinal) ""))
                                        cols))
                                (when agent-shell-queue-show-age-column
                                  (push (funcall cell age-str) cols))
                                (push (funcall cell (truncate-string-to-width
                                                     first-line pw nil nil "…"))
                                      cols)
                                (apply #'vector (nreverse cols)))))
                    (list id row)))
                (cdr pair))))))
    (tabulated-list-print t)
    (when agent-shell-queue-multiline-format
      (agent-shell-queue--expand-multiline))))

(defun agent-shell-queue--expand-multiline ()
  "Expand each tabulated entry with a second prompt line and a separator.
Must be called immediately after `tabulated-list-print'."
  (let* ((inhibit-read-only t)
         (sep-face 'shadow)
         (sep-char ?─)
         ;; Collect (id . line-start-pos) in reverse buffer order so that
         ;; inserting extra lines below each entry does not shift earlier positions.
         (positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id (tabulated-list-get-id)))
          (push (cons id (line-beginning-position)) positions))
        (forward-line 1)))
    ;; positions is already in reverse order due to push; process top-to-bottom
    ;; would corrupt offsets, so keep reverse (last entry first).
    (seq-do (lambda (it)
              (when-let* ((id (car it))
                          (line-start (cdr it))
                          (item (cdr (agent-shell-queue--item-by-id id)))
                          (prompt (agent-shell-queue-item-args item)))
                (let ((face (cdr (agent-shell-queue--item-display item nil nil))))
                  (save-excursion
                    (goto-char line-start)
                    (end-of-line)
                    (let ((insert-start (point))
                          (sep (propertize
                                (make-string (max 4 (1- (window-width))) sep-char)
                                'face sep-face)))
                      (insert "\n")
                      (insert (propertize (concat "  " prompt) 'face face))
                      (put-text-property insert-start (point) 'tabulated-list-id id)
                      (insert "\n" sep)
                      (put-text-property (1- (point)) (point)
                                         'agent-shell-queue-separator t))))))
            positions)))

(defun agent-shell-queue-next-item ()
  "Move point to the first line of the next queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line 1)
    (let ((current-id (tabulated-list-get-id)))
      (forward-line 1)
      (while (and (not (eobp))
                  (equal (tabulated-list-get-id) current-id))
        (forward-line 1)))))

(defun agent-shell-queue-prev-item ()
  "Move point to the first line of the previous queue item."
  (interactive)
  (if (not agent-shell-queue-multiline-format)
      (forward-line -1)
    (let ((current-id (tabulated-list-get-id))
          (target-id nil))
      ;; Step backward until a different id appears
      (forward-line -1)
      (while (and (not (bobp))
                  (or (null (tabulated-list-get-id))
                      (equal (tabulated-list-get-id) current-id)))
        (forward-line -1))
      (setq target-id (tabulated-list-get-id))
      ;; Now find the first (topmost) line of that item
      (when target-id
        (while (and (not (bobp))
                    (equal (get-text-property (line-beginning-position 0)
                                              'tabulated-list-id)
                           target-id))
          (forward-line -1))
        ;; If we overshot past the item, go forward one line
        (unless (equal (tabulated-list-get-id) target-id)
          (forward-line 1))))))

(defun agent-shell-queue-buffer-jump-to-next ()
  "Move point to the next item that will be dispatched."
  (interactive)
  (let ((next-ids (thread-last
                    (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-map (lambda (pair)
                               (agent-shell-queue--next-dispatchable-item (cdr pair))))
                    (seq-filter #'identity)
                    (seq-map #'agent-shell-queue-item-id))))
    (if (null next-ids)
        (message "No pending items in queue")
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (not (eobp)))
          (if (member (tabulated-list-get-id) next-ids)
              (setq found t)
            (forward-line 1)))
        (unless found
          (message "No pending items visible in current scope"))))))

(defun agent-shell-queue-buffer-pause ()
  "Pause the item at point — suspend it from auto-dispatch without removing it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'blocked.skip)
    (agent-shell-queue--save)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-schedule ()
  "Schedule the paused item at point — resume it for auto-dispatch.
For blocked.task items, cascades active to subsequent blocked.dep items.
Draft items are promoted directly to active without cascade."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (status (agent-shell-queue-item-status item)))
    (cond
     ((agent-shell-queue--blocked-status-p status)
      (agent-shell-queue-unblock id))
     ((eq status 'draft)
      (setf (agent-shell-queue-item-status item) 'active)
      (agent-shell-queue--save))
     (t (user-error "Item %s cannot be scheduled from status %s" id status)))
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-unblock ()
  "Unblock the item at point (blocked.task → active with cascade)."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-unblock id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-unblock ()
  "Unblock the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-unblock id)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-buffer-remove ()
  "Remove the item at point from the queue, with confirmation."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (agent-shell-queue--confirm-remove item)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-remove id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-send ()
  "Send the item at point to its target buffer now.
When another task is already running for the same session, behavior adapts
based on the item's current status — see `agent-shell-queue--send-now'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue--send-now id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-untrack-running ()
  "Remove the running item at point from queue tracking without aborting it."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-untrack-running id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-untrack-running ()
  "Remove the displayed running item from queue tracking without aborting it."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (quit-window)
    (agent-shell-queue-untrack-running id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-buffer-enqueue-running-copy ()
  "Append a copy of the running item at point to the end of its queue."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-enqueue-running-copy id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-item-view-enqueue-running-copy ()
  "Append a copy of the displayed running item to the end of its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-enqueue-running-copy id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-buffer-reenqueue ()
  "Re-enqueue the done or aborted item at point as a new active item."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue-buffer-refresh)))

(defvar agent-shell-queue-show-buffer-column t
  "Show the Buffer column in the queue buffer.
Toggle with `agent-shell-queue-toggle-buffer-column' (db in the menu).")

(defvar agent-shell-queue-show-ordinal-column t
  "Show the ordinal (#) column in the queue buffer.")

(defvar agent-shell-queue-show-age-column t
  "Show the Age column in the queue buffer.")

(defvar agent-shell-queue-show-kind-column t
  "Show the Kind column in the queue buffer.")

(defvar agent-shell-queue-multiline-format nil
  "Display prompt on a second line with a separator between items.
When non-nil, `<down>' and `<up>' move by item rather than by line.")

(defun agent-shell-queue-toggle-buffer-column ()
  "Toggle visibility of the Buffer column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-buffer-column (not agent-shell-queue-show-buffer-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue buffer column: %s"
	   (if agent-shell-queue-show-buffer-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-ordinal-column ()
  "Toggle visibility of the ordinal (#) column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-ordinal-column (not agent-shell-queue-show-ordinal-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue ordinal column: %s"
	   (if agent-shell-queue-show-ordinal-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-age-column ()
  "Toggle visibility of the Age column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-age-column (not agent-shell-queue-show-age-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue age column: %s"
           (if agent-shell-queue-show-age-column
	       "on"
	     "off")))

(defun agent-shell-queue-toggle-kind-column ()
  "Toggle visibility of the Kind column in the queue buffer."
  (interactive)
  (setq agent-shell-queue-show-kind-column (not agent-shell-queue-show-kind-column))
  (agent-shell-queue-buffer-refresh)
  (message "Queue kind column: %s"
           (if agent-shell-queue-show-kind-column "on" "off")))

(defun agent-shell-queue-toggle-multiline-format ()
  "Toggle multi-line display format for the queue buffer."
  (interactive)
  (setq agent-shell-queue-multiline-format
        (not agent-shell-queue-multiline-format))
  (agent-shell-queue-buffer-refresh)
  (message "Queue multi-line format: %s"
           (if agent-shell-queue-multiline-format "on" "off")))

;;;###autoload
(defun agent-shell-queue-select-columns ()
  "Pick column display options via `annotated-completing-read'.
Offers bulk presets, per-column visibility toggles, and multi-line switch.
Changes take effect immediately via `agent-shell-queue-buffer-refresh'."
  (interactive)
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Not in an agent-shell queue buffer"))
  (let* ((columns `(("Buffer column" . agent-shell-queue-show-buffer-column)
                    ("Ordinal # column" . agent-shell-queue-show-ordinal-column)
                    ("Age column" . agent-shell-queue-show-age-column)
                    ("Kind column" . agent-shell-queue-show-kind-column)))
         (table (map-into
                 (append
                  (list (cons "+ show all columns"
                              (if (and agent-shell-queue-show-buffer-column
                                       agent-shell-queue-show-ordinal-column
                                       agent-shell-queue-show-age-column
                                       agent-shell-queue-show-kind-column)
                                  "already showing all columns"
                                "enable Buffer, Ordinal, Age, and Kind columns"))
                        (cons "+ minimal: status and prompt only"
                              (if (not (or agent-shell-queue-show-buffer-column
                                           agent-shell-queue-show-ordinal-column
                                           agent-shell-queue-show-age-column
                                           agent-shell-queue-show-kind-column))
                                  "already minimal"
                                "hide Buffer, Ordinal, Age, and Kind columns")))
                  (seq-map (lambda (it)
                             (cons (car it)
                                   (if (symbol-value (cdr it))
                                       "visible · click to hide"
                                     "hidden · click to show")))
                           columns)
                  (list (cons "Multi-line format"
                              (if agent-shell-queue-multiline-format
                                  "on · prompt on second line · click to disable"
                                "off · single-line · click to enable"))))
                 '(hash-table :test equal))))
    (when-let* ((choice (annotated-completing-read
                        table
                        :prompt "queue columns: "
                        :category 'agent-shell-queue-column
                        :require-match t
                        :history 'agent-shell-queue-select-columns)))
      (cond
       ((equal choice "+ show all columns")
        (setq agent-shell-queue-show-buffer-column t
              agent-shell-queue-show-ordinal-column t
              agent-shell-queue-show-age-column t
              agent-shell-queue-show-kind-column t))
       ((equal choice "+ minimal: status and prompt only")
        (setq agent-shell-queue-show-buffer-column nil
              agent-shell-queue-show-ordinal-column nil
              agent-shell-queue-show-age-column nil
              agent-shell-queue-show-kind-column nil))
       ((equal choice "Multi-line format")
        (setq agent-shell-queue-multiline-format (not agent-shell-queue-multiline-format)))
       (t
        (when-let* ((var (cdr (assoc choice columns))))
          (set var (not (symbol-value var))))))
      (agent-shell-queue-buffer-refresh))))

(add-to-list 'savehist-additional-variables 'agent-shell-queue--queue)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-buffer-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-ordinal-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-age-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-show-kind-column)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-multiline-format)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-default-pause-delay)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-alert-on-pause-start)
(add-to-list 'savehist-additional-variables 'agent-shell-queue-alert-before-pause-end)
;;; Queue Flow Modifiers and Wait Items

;; Item view

;; Item-view action table — single source of truth for keys, transient, and ACR menu

(defconst agent-shell-queue--item-view-action-table
  (list
   ;; Plist fields: :key :label :cmd :group :annotation :if
   ;; :group nil  — keymap only, not shown in transient or ACR
   ;; :if nil     — always shown when :group is non-nil
   (list :key "s"
         :label "Dispatch now"
         :cmd 'agent-shell-queue-item-view-send
         :group "Manage Task"
         :annotation "Send item to target shell immediately"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted draft)))))
   (list :key "X"
         :label "Abort (interrupt)"
         :cmd 'agent-shell-queue-item-view-abort
         :group "Manage Task"
         :annotation "Interrupt running item, mark as aborted"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "E"
         :label "Enqueue copy (repeat after current run)"
         :cmd 'agent-shell-queue-item-view-enqueue-running-copy
         :group "Manage Task"
         :annotation "Append an active copy to the queue without interrupting the current run"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "U"
         :label "Untrack (remove without aborting)"
         :cmd 'agent-shell-queue-item-view-untrack-running
         :group "Manage Task"
         :annotation "Drop queue tracking for this item; the shell process continues"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'running)))
   (list :key "R"
         :label "Re-enqueue"
         :cmd 'agent-shell-queue-item-view-reenqueue
         :group "Manage Task"
         :annotation "Create a new active copy of this completed item"
         :if (lambda () (memq (agent-shell-queue--iv-status) '(done aborted))))
   (list :key "z"
         :label "Mark done"
         :cmd 'agent-shell-queue-item-view-mark-done
         :group "Manage Task"
         :annotation "Manually mark item done without dispatching"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "e"
         :label "Edit"
         :cmd 'agent-shell-queue-item-view-edit
         :group "Manage Task"
         :annotation "Open item in edit buffer"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "d"
         :label "Pause (suspend dispatch)"
         :cmd 'agent-shell-queue-item-view-pause
         :group "Manage Task"
         :annotation "Suspend item from being dispatched"
         :if (lambda () (eq (agent-shell-queue--iv-status) 'active)))
   (list :key "u"
         :label "Schedule (resume dispatch)"
         :cmd 'agent-shell-queue-item-view-schedule
         :group "Manage Task"
         :annotation "Return item to active dispatch queue"
         :if (lambda () (memq (agent-shell-queue--iv-status) '(draft))))
   (list :key "f"
         :label "Unblock"
         :cmd 'agent-shell-queue-item-view-unblock
         :group "Manage Task"
         :annotation "Unblock item and cascade to dependent items"
         :if (lambda () (agent-shell-queue--blocked-status-p (agent-shell-queue--iv-status))))
   (list :key "b"
         :label "Enable background task"
         :cmd 'agent-shell-queue-item-view-enable-background-task
         :group "Manage Task"
         :annotation "Prefix prompt with /background on dispatch"
         :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                             (not (agent-shell-queue--iv-bg-p)))))
   (list :key "B"
         :label "Disable background task"
         :cmd 'agent-shell-queue-item-view-disable-background-task
         :group "Manage Task"
         :annotation "Remove background task flag"
         :if (lambda () (and (not (memq (agent-shell-queue--iv-status) '(done running aborted)))
                             (agent-shell-queue--iv-bg-p))))
   (list :key "o"
         :label "Open shell buffer"
         :cmd 'agent-shell-queue-item-view-open-shell
         :group "Manage Task"
         :annotation "Switch to this item's target shell buffer"
         :if nil)
   (list :key "i"
         :label "Inspect raw"
         :cmd 'agent-shell-queue-item-view-inspect
         :group "Manage Task"
         :annotation "View raw serialization of this item"
         :if nil)
   (list :key "C-d"
         :label "Destructive…"
         :cmd 'agent-shell-queue-item-destructive-menu
         :group "Manage Task"
         :annotation "Archive, remove, or other destructive operations"
         :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
   ;; Move / Assign group
   (list :key "M-<up>"
         :label "Move up"
         :cmd 'agent-shell-queue-item-view-move-up
         :group "Move / Assign"
         :annotation "Move item earlier in its bucket queue"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "M-<down>"
         :label "Move down"
         :cmd 'agent-shell-queue-item-view-move-down
         :group "Move / Assign"
         :annotation "Move item later in its bucket queue"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   (list :key "t"
         :label "Assign to shell…"
         :cmd 'agent-shell-queue-item-view-assign
         :group "Move / Assign"
         :annotation "Move item to a different agent-shell buffer"
         :if (lambda () (not (memq (agent-shell-queue--iv-status) '(done running aborted)))))
   ;; Detached reassignment — only visible when target buffer is dead
   (list :key "T"
         :label "Reassign (this item)"
         :cmd 'agent-shell-queue-item-view-reassign-detached
         :group "Move / Assign"
         :annotation "Assign this detached item to an active or new shell"
         :if #'agent-shell-queue--iv-detached-p)
   (list :key "C-t"
         :label "Reassign (all in same bucket)"
         :cmd 'agent-shell-queue-item-view-reassign-bucket-detached
         :group "Move / Assign"
         :annotation "Assign all items from the same dead shell to a shell"
         :if #'agent-shell-queue--iv-detached-p)
   (list :key "C-T"
         :label "Reassign (all detached)"
         :cmd 'agent-shell-queue-item-view-reassign-all-detached
         :group "Move / Assign"
         :annotation "Assign every detached item across all buckets to a shell"
         :if #'agent-shell-queue--iv-detached-p)
   ;; Keymap-only entries (no transient/ACR group)
   (list :key "C-K"     :label "Remove"   :cmd 'agent-shell-queue-item-view-remove  :group nil :annotation nil :if nil)
   (list :key "C-<DEL>" :label "Remove"   :cmd 'agent-shell-queue-item-view-remove  :group nil :annotation nil :if nil)
   (list :key "C-A"     :label "Archive"  :cmd 'agent-shell-queue-item-view-archive :group nil :annotation nil :if nil)
   (list :key "g"       :label "Refresh"  :cmd 'agent-shell-queue-item-view-refresh :group nil :annotation nil :if nil)
   (list :key "m"       :label "Menu"     :cmd 'agent-shell-queue-item-menu         :group nil :annotation nil :if nil)
   (list :key "a"       :label "Actions"  :cmd 'agent-shell-queue-item-view-actions :group nil :annotation nil :if nil)
   (list :key "q"       :label "Close"    :cmd 'quit-window                          :group nil :annotation nil :if nil))
  "Action table for `agent-shell-queue-item-view-mode'.
Each entry is a plist with keys:
  :key        — key binding string for `kbd'
  :label      — human-readable label
  :cmd        — command symbol
  :group      — transient group name (nil = keymap only)
  :annotation — short annotation for ACR menu (nil = not in ACR)
  :if         — predicate function or nil (nil = always visible)")

(defconst agent-shell-queue--item-view-action-groups
  '("Manage Task" "Move / Assign")
  "Ordered group names for the item-view transient menu.")

(defun agent-shell-queue--item-view-build-map ()
  "Build `agent-shell-queue-item-view-mode-map' from the action table."
  (let ((m (make-sparse-keymap)))
    (seq-do (lambda (entry)
              (define-key m (kbd (plist-get entry :key)) (plist-get entry :cmd)))
            agent-shell-queue--item-view-action-table)
    m))

;;; Item View and Raw Inspection

(defvar agent-shell-queue-item-view-mode-map
  (agent-shell-queue--item-view-build-map)
  "Keymap for `agent-shell-queue-item-view-mode'.")

(define-derived-mode agent-shell-queue-item-view-mode markdown-mode "Queue-Item"
  "Read-only view of a single `agent-shell' queue item."
  (setq buffer-read-only t)
  (font-lock-mode -1))

(defvar-local agent-shell-queue--item-view-id nil
  "ID of the queue item displayed in this item-view buffer.")

(defvar-local agent-shell-queue--item-view-queue-buf nil
  "The queue buffer that spawned this item view.")

(defun agent-shell-queue--render-item-view (id item target)
  "Render ITEM with ID and TARGET into the current buffer."
  (setq-local fill-column 80)
  (let* ((created (agent-shell-queue-item-created item))
         (dispatched (agent-shell-queue-item-dispatched item))
         (completed (agent-shell-queue-item-completed item))
         (bg (agent-shell-queue-item-background item))
         (kind (agent-shell-queue-item-kind item))
         (next-p (when-let* ((first (agent-shell-queue--next-dispatchable-item
                                    (cdr (assoc target (agent-shell-queue-store-items agent-shell-queue--store))))))
                   (equal (agent-shell-queue-item-id first) id)))
         (field (lambda (label value)
                  (insert (propertize (format "%-12s" label) 'face 'bold))
                  (insert (format " %s\n" value)))))
    (funcall field "ID:" id)
    (funcall field "Target:"
             (if (equal target agent-shell-queue--unassigned-key)
                 "(unassigned)" target))
    (when-let* ((dir (agent-shell-queue-item-directory item)))
      (funcall field "Directory:" dir))
    (funcall field "Status:" (agent-shell-queue--status-string item target next-p))
    (when-let* ((outcome (agent-shell-queue-item-outcome item)))
      (funcall field "Outcome:" (symbol-name outcome)))
    (when-let* ((from-id (agent-shell-queue-item-reenqueued-from item)))
      (funcall field "Re-enq from:" from-id))
    (when-let* ((as-ids (agent-shell-queue-item-reenqueued-as item)))
      (funcall field "Re-enq as:" (string-join as-ids ", ")))
    (funcall field "Kind:" (symbol-name (or kind 'prompt)))
    (funcall field "Background:" (if bg "yes" "no"))
    (insert "\n")
    (funcall field "Created:"
             (format "%s (%s ago)"
                     (format-time-string "%F %T" created)
                     (agent-shell-queue--format-age (time-since created))))
    (when dispatched
      (funcall field "Dispatched:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" dispatched)
                       (agent-shell-queue--format-age (time-since dispatched)))))
    (when completed
      (funcall field "Completed:"
               (format "%s (%s ago)"
                       (format-time-string "%F %T" completed)
                       (agent-shell-queue--format-age (time-since completed)))))
    (when (and dispatched completed)
      (funcall field "Latency:"
               (agent-shell-queue--format-age (time-subtract completed dispatched))))
    (insert "\n")
    (insert (propertize "Prompt:\n" 'face 'bold))
    (insert (agent-shell-queue-item-args item) "\n")
    (when-let* ((response (agent-shell-queue-item-response item)))
      (insert "\n")
      (insert (propertize "Response:\n" 'face 'bold))
      (insert response "\n"))
    (when-let* ((ipromt (agent-shell-queue-item-interjection-prompt item)))
      (insert "\n")
      (insert (propertize "Interjection prompt:\n" 'face 'bold))
      (insert (string-replace "\n" "\n  " (concat "  " ipromt)) "\n")
      (when-let* ((iresult (agent-shell-queue-item-interjection-result item)))
        (insert "\n")
        (insert (propertize "Interjection response:\n" 'face 'bold))
        (insert (string-replace "\n" "\n  " (concat "  " iresult)) "\n")))
    (insert "\n")
    (insert (propertize "[m] menu  [a] actions  [q] close" 'face 'shadow))))

(defun agent-shell-queue-buffer-view-item ()
  "Open an item-view window below showing the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair))
              (queue-buf (current-buffer))
              (view-name (format "*agent-shell-queue-item: %s*" id))
              (view-buf (get-buffer-create view-name)))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-item-view-mode)
        (setq agent-shell-queue--item-view-id id
              agent-shell-queue--item-view-queue-buf queue-buf)
        (agent-shell-queue--render-item-view id item target)))
    (display-buffer view-buf '(display-buffer-below-selected
                               (window-height . 0.35)))))

(defun agent-shell-queue-find-item-command ()
  "Interactively pick any queue item and display it in the item-view buffer."
  (interactive)
  (when-let* ((pair (agent-shell-queue-find-item "Jump to item: "))
              (item (cdr pair))
              (id (agent-shell-queue-item-id item))
              (target (car pair))
              (view-name (format "*agent-shell-queue-item: %s*" id))
              (view-buf (get-buffer-create view-name)))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-item-view-mode)
        (setq agent-shell-queue--item-view-id id)
        (agent-shell-queue--render-item-view id item target)))
    (display-buffer view-buf '(display-buffer-below-selected
                               (window-height . 0.35)))))

(defun agent-shell-queue-item-view-refresh ()
  "Refresh the content of the current item-view buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (target (car pair))
              (inhibit-read-only t))
    (erase-buffer)
    (agent-shell-queue--render-item-view id item target)))

(defun agent-shell-queue-item-view-send ()
  "Send the displayed item to its target buffer now.
When another task is already running for the same session, behavior adapts
based on the item's current status — see `agent-shell-queue--send-now'."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair)))
    (when (not (agent-shell-queue--has-running-item-p buf-name))
      (quit-window))
    (agent-shell-queue--send-now id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-remove ()
  "Remove the displayed item from the queue, with confirmation."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              (_ (or (agent-shell-queue--assert-not-running item) t)))
    (when (agent-shell-queue--confirm-remove item)
      (quit-window)
      (agent-shell-queue-remove id)
      (agent-shell-queue--refresh-buffer))))

(defun agent-shell-queue-item-view-pause ()
  "Pause the displayed item — suspend it from auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'active)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'blocked.skip)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-schedule ()
  "Schedule the displayed item — resume it for auto-dispatch."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              ((eq (agent-shell-queue-item-status (cdr pair)) 'draft)))
    (setf (agent-shell-queue-item-status (cdr pair)) 'active)
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reenqueue ()
  "Re-enqueue the displayed done or aborted item as a new active item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (_ (memq (agent-shell-queue-item-status (cdr pair)) '(done aborted))))
    (quit-window)
    (agent-shell-queue-reenqueue id)
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-item-view-archive ()
  "Archive the displayed item and close the view.
Archiving must be enabled via `agent-shell-queue-archive-enabled'."
  (interactive)
  (unless agent-shell-queue-archive-enabled
    (user-error "Enable archiving by setting `agent-shell-queue-archive-enabled' to t"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue--write-archive (car pair) item)
    (quit-window)
    (agent-shell-queue-remove id)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: archived %s" id)))

(defun agent-shell-queue-item-view-enable-background-task ()
  "Flag the displayed item for background sub-agent execution."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-disable-background-task ()
  "Clear the background sub-agent flag from the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-up ()
  "Move the displayed item one position earlier in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-move-down ()
  "Move the displayed item one position later in its queue."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-assign ()
  "Assign the displayed item to a different `agent-shell' buffer."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (bufs (or (agent-shell-buffers)
                        (user-error "No live agent-shell buffers")))
              (_ (or (agent-shell-queue--assert-not-running (cdr pair)) t)))
    (let* ((current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (table (seq-map
                   (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (cons (buffer-name buf)
                             (if (and current-dir (equal dir current-dir))
                                 (concat "(same dir) " (abbreviate-file-name dir))
                               (abbreviate-file-name (or dir ""))))))
                   bufs)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)
        (agent-shell-queue--refresh-buffer)
        (agent-shell-queue-item-view-refresh)))))

;; Detached item reassignment

(defun agent-shell-queue--pick-shell-for-reassign (prompt)
  "Prompt with PROMPT for a live `agent-shell' buffer or offer to create a new one.
Returns a buffer name string, or nil if cancelled."
  (let* ((bufs (agent-shell-buffers))
         (live-entries (seq-map (lambda (b)
                                  (cons (buffer-name b)
                                        (abbreviate-file-name
                                         (or (buffer-local-value 'default-directory b) ""))))
                                bufs))
         (choices (cons (cons "(create new shell)" "Open a new agent-shell in a chosen directory")
                        live-entries))
         (choice (annotated-completing-read choices
                                            :prompt prompt
                                            :require-match t)))
    (if (equal choice "(create new shell)")
        (let* ((dir (read-directory-name "Shell directory: "))
               (before (agent-shell-buffers))
               (_ (let ((default-directory dir))
                    (call-interactively #'agent-shell-new-shell)))
               (_ (sit-for 0.1))
               (new-buf (seq-find (lambda (b) (not (memq b before)))
                                  (agent-shell-buffers))))
          (when new-buf (buffer-name new-buf)))
      choice)))

(defun agent-shell-queue-item-view-reassign-detached ()
  "Assign this detached item to an active or newly created shell."
  (interactive)
  (unless (agent-shell-queue--iv-detached-p)
    (user-error "This item is not detached"))
  (when-let* ((id agent-shell-queue--item-view-id)
              (new-name (agent-shell-queue--pick-shell-for-reassign "reassign to: ")))
    (agent-shell-queue--assign-item id new-name)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue-item-view-reassign-bucket-detached ()
  "Assign all items in the same dead bucket to an active or new shell."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (target (agent-shell-queue--iv-target))
              (_ (or (agent-shell-queue--item-detached-p target)
                     (user-error "This item is not detached")))
              (cell (assoc target (agent-shell-queue-store-items agent-shell-queue--store)))
              (ids (seq-map #'agent-shell-queue-item-id (cdr cell)))
              (new-name (agent-shell-queue--pick-shell-for-reassign
                         (format "reassign %d item(s) from dead shell to: " (length ids)))))
    (seq-do (lambda (item-id)
              (when (agent-shell-queue--item-by-id item-id)
                (agent-shell-queue--assign-item item-id new-name)))
            ids)
    (agent-shell-queue--refresh-buffer)
    (when (derived-mode-p 'agent-shell-queue-item-view-mode)
      (agent-shell-queue-item-view-refresh))))

(defun agent-shell-queue-item-view-reassign-all-detached ()
  "Assign all detached items across all buckets to an active or new shell."
  (interactive)
  (let* ((all-ids (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-filter (lambda (p) (agent-shell-queue--item-detached-p (car p))))
                    (seq-mapcat (lambda (p) (seq-map #'agent-shell-queue-item-id (cdr p)))))))
    (when (null all-ids)
      (user-error "No detached items in the queue"))
    (when-let* ((new-name (agent-shell-queue--pick-shell-for-reassign
                           (format "reassign %d detached item(s) to: " (length all-ids)))))
      (seq-do (lambda (item-id)
                (when (agent-shell-queue--item-by-id item-id)
                  (agent-shell-queue--assign-item item-id new-name)))
              all-ids)
      (agent-shell-queue--refresh-buffer)
      (when (derived-mode-p 'agent-shell-queue-item-view-mode)
        (agent-shell-queue-item-view-refresh)))))

(defun agent-shell-queue-item-view-edit ()
  "Open the edit buffer for the displayed item."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (item (cdr (agent-shell-queue--item-by-id id)))
              (qbuf agent-shell-queue--item-view-queue-buf)
              ((buffer-live-p qbuf)))
    (agent-shell-queue--assert-not-running item)
    (quit-window)
    (with-current-buffer qbuf
      (goto-char (point-min))
      (while (and (not (equal (tabulated-list-get-id) id))
                  (not (eobp)))
        (forward-line 1))
      (agent-shell-queue--open-edit-for-id id))))

;; Item raw inspect mode

(defvar-local agent-shell-queue--inspect-id nil
  "ID of the queue item shown in this inspect buffer.")

(defvar-local agent-shell-queue--inspect-format nil
  "Serialization format in inspect buffer (plist, json, or yaml).")

(defvar agent-shell-queue-inspect-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") #'agent-shell-queue-inspect-as-plist)
    (define-key m (kbd "j") #'agent-shell-queue-inspect-as-json)
    (define-key m (kbd "y") #'agent-shell-queue-inspect-as-yaml)
    (define-key m (kbd "g") #'agent-shell-queue-inspect-refresh)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `agent-shell-queue-inspect-mode'.")

(define-minor-mode agent-shell-queue-inspect-mode
  "Minor mode active in queue item inspect buffers.
Binds p/j/y to switch formats, g to refresh, q to quit."
  :lighter nil
  :keymap agent-shell-queue-inspect-mode-map)

(defun agent-shell-queue--inspect-buffer-name (id format)
  "Buffer name for the raw inspect view of item ID in FORMAT."
  (format "*agent-shell-queue-inspect: %s [%s]*" id format))

(defun agent-shell-queue-inspect-refresh ()
  "Refresh the current inspect buffer from live queue state."
  (interactive)
  (let ((id agent-shell-queue--inspect-id)
        (fmt agent-shell-queue--inspect-format))
    (unless (and id fmt)
      (user-error "Not in an agent-shell queue inspect buffer"))
    (when-let* ((pair (agent-shell-queue--item-by-id id))
                (inhibit-read-only t))
      (erase-buffer)
      (insert (agent-shell-queue--serialize-single-item (cdr pair) (car pair) fmt))
      (goto-char (point-min)))))

(defun agent-shell-queue--apply-inspect-format (format)
  "Switch the current inspect buffer to FORMAT and re-render."
  (unless agent-shell-queue--inspect-id
    (user-error "Not in an agent-shell queue inspect buffer"))
  (let ((saved-id agent-shell-queue--inspect-id)
        (inhibit-read-only t))
    (erase-buffer)
    (when-let* ((pair (agent-shell-queue--item-by-id saved-id)))
      (insert (agent-shell-queue--serialize-single-item
               (cdr pair) (car pair) format)))
    (rename-buffer (agent-shell-queue--inspect-buffer-name saved-id format) t)
    (pcase format
      ('plist (emacs-lisp-mode))
      ('json  (if (fboundp 'json-mode) (json-mode) (js-mode)))
      ('yaml  (if (fboundp 'yaml-mode) (yaml-mode) (fundamental-mode))))
    (setq-local agent-shell-queue--inspect-id saved-id)
    (setq-local agent-shell-queue--inspect-format format)
    (agent-shell-queue-inspect-mode 1)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun agent-shell-queue-inspect-as-plist ()
  "Show the current inspect item as a plist."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'plist))

(defun agent-shell-queue-inspect-as-json ()
  "Show the current inspect item as JSON."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'json))

(defun agent-shell-queue-inspect-as-yaml ()
  "Show the current inspect item as YAML."
  (interactive)
  (agent-shell-queue--apply-inspect-format 'yaml))

(defun agent-shell-queue--inspect-format-display ()
  "Return an alist of (LABEL . ANNOTATION) for format completion.
Labels are format symbol names; the on-disk format is annotated with [on-disk]."
  (let ((on-disk (agent-shell-queue-store-format agent-shell-queue--store)))
    (seq-filter
     #'identity
     (list (cons "plist" (if (eq on-disk 'plist) "[on-disk]" ""))
           (cons "json"  (if (eq on-disk 'json)  "[on-disk]" ""))
           (when (fboundp 'yaml-encode)
             (cons "yaml" (if (eq on-disk 'yaml) "[on-disk]" "")))))))

(defun agent-shell-queue--inspect-prompt-format ()
  "Prompt for a serialization format; return the format symbol."
  (intern (annotated-completing-read
           (agent-shell-queue--inspect-format-display)
           :prompt "inspect format => "
           :category 'agent-shell-queue-inspect-format
           :require-match t
           :history 'agent-shell-queue-inspect-format)))

(defun agent-shell-queue--inspect-open (id format)
  "Open or refresh the inspect buffer for item ID in FORMAT."
  (let* ((pair (or (agent-shell-queue--item-by-id id)
                   (user-error "Item %s not found in queue" id)))
         (buf (get-buffer-create (agent-shell-queue--inspect-buffer-name id format))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (agent-shell-queue--serialize-single-item (cdr pair) (car pair) format)))
      (pcase format
        ('plist (emacs-lisp-mode))
        ('json  (if (fboundp 'json-mode) (json-mode) (js-mode)))
        ('yaml  (if (fboundp 'yaml-mode) (yaml-mode) (fundamental-mode))))
      (setq-local agent-shell-queue--inspect-id id)
      (setq-local agent-shell-queue--inspect-format format)
      (agent-shell-queue-inspect-mode 1)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun agent-shell-queue-buffer-inspect-item ()
  "Open a read-only raw-serialization view of the queue item at point.
Prompts for the serialization format (p=plist j=json y=yaml in the buffer)."
  (interactive)
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Not in an agent-shell queue buffer"))
  (agent-shell-queue--inspect-open
   (or (tabulated-list-get-id) (user-error "No item at point"))
   (agent-shell-queue--inspect-prompt-format)))

;;;###autoload
(defun agent-shell-queue-item-view-inspect ()
  "Open a raw-serialization view of the item shown in this buffer.
Prompts for the serialization format (p=plist j=json y=yaml in the buffer)."
  (interactive)
  (agent-shell-queue--inspect-open
   (or agent-shell-queue--item-view-id
       (user-error "Not in a queue item view buffer"))
   (agent-shell-queue--inspect-prompt-format)))

(defun agent-shell-queue-buffer-abort ()
  "Interrupt the running item at point and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))
    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (setf (agent-shell-queue-item-outcome item) 'canceled)
      (agent-shell-queue--insert-resume-task buf-name item)
      (agent-shell-queue--pause-and-save buf-name))))

(defun agent-shell-queue-item-view-abort ()
  "Interrupt the running displayed item and mark it as aborted.
Pauses the session queue — call `agent-shell-queue-session-resume' to restart."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id)
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair))
              ((eq (agent-shell-queue-item-status item) 'running)))

    (let ((buf-name (car pair)))
      (when-let* ((buf (get-buffer buf-name))
                  (_ (buffer-live-p buf)))
        (with-current-buffer buf
          (agent-shell-interrupt)))
      (setf (agent-shell-queue-item-status item) 'aborted)
      (setf (agent-shell-queue-item-outcome item) 'canceled)
      (agent-shell-queue--insert-resume-task buf-name item)
      (agent-shell-queue--pause-and-save buf-name)
      (agent-shell-queue-item-view-refresh))))

;; Transient predicates

(defun agent-shell-queue--iv-item ()
  "Return the item being viewed in the current item-view buffer, or nil."
  (when (and (boundp 'agent-shell-queue--item-view-id) agent-shell-queue--item-view-id)
    (cdr (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--item-detached-p (buf-name)
  "Return non-nil when BUF-NAME names a bucket whose buffer is not live.
Does not match the unassigned bucket — only truly detached (dead) targets."
  (and buf-name
       (not (equal buf-name agent-shell-queue--unassigned-key))
       (not (buffer-live-p (get-buffer buf-name)))))

(defun agent-shell-queue--iv-target ()
  "Return the bucket key for the item currently shown in this item-view buffer."
  (when (and (boundp 'agent-shell-queue--item-view-id)
             agent-shell-queue--item-view-id)
    (car (agent-shell-queue--item-by-id agent-shell-queue--item-view-id))))

(defun agent-shell-queue--iv-detached-p ()
  "Return non-nil if the viewed item's target shell is no longer live."
  (agent-shell-queue--item-detached-p (agent-shell-queue--iv-target)))

(defun agent-shell-queue--iv-status ()
  "Return the status of the item being viewed, or nil."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--iv-bg-p ()
  "Return non-nil if the viewed item has background mode enabled."
  (when-let* ((item (agent-shell-queue--iv-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-item ()
  "Return the queue item at point in the queue buffer, or nil."
  (when-let* ((id (and (derived-mode-p 'agent-shell-queue-mode)
                      (tabulated-list-get-id))))
    (cdr (agent-shell-queue--item-by-id id))))

(defun agent-shell-queue--point-status ()
  "Return the status of the queue item at point, or nil."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-status item)))

(defun agent-shell-queue--point-bg-p ()
  "Return non-nil if the item at point has background mode enabled."
  (when-let* ((item (agent-shell-queue--point-item)))
    (agent-shell-queue-item-background item)))

(defun agent-shell-queue--point-running-p ()
  "Return non-nil when the item at point is running."
  (eq (agent-shell-queue--point-status) 'running))

(defun agent-shell-queue--point-not-running-p ()
  "Return non-nil when the item at point is not running."
  (not (agent-shell-queue--point-running-p)))

(defun agent-shell-queue--point-active-p ()
  "Return non-nil when the item at point is active."
  (eq (agent-shell-queue--point-status) 'active))

(defun agent-shell-queue--point-deferred-p ()
  "Return non-nil when the item at point is in any blocked state or draft."
  (let ((status (agent-shell-queue--point-status)))
    (or (eq status 'draft)
        (agent-shell-queue--blocked-status-p status))))

(defun agent-shell-queue--point-blocked-p ()
  "Return non-nil when the item at point has any blocked.* status."
  (agent-shell-queue--blocked-status-p (agent-shell-queue--point-status)))

(defun agent-shell-queue--point-done-p ()
  "Return non-nil when the item at point is done or aborted."
  (memq (agent-shell-queue--point-status) '(done aborted)))

(defun agent-shell-queue--point-dispatchable-p ()
  "Return non-nil when the item at point can be dispatched."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil draft))))

(defun agent-shell-queue--point-not-done-p ()
  "Return non-nil when the item at point is in a not-done, non-running state."
  (not (memq (agent-shell-queue--point-status)
             '(done running aborted nil))))

(defun agent-shell-queue--point-editable-p ()
  "Return non-nil when item at point can be edited or moved.
Aborted items remain editable; only running, done, or absent items are
excluded.")

(transient-define-prefix agent-shell-queue-item-destructive-menu ()
  "Destructive actions for the item shown in the current item-view buffer."
  [["Destructive"
    ("A" "Archive" agent-shell-queue-item-view-archive
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
    ("k" "Remove" agent-shell-queue-item-view-remove
     :if (lambda () (not (eq (agent-shell-queue--iv-status) 'running))))
    ("x" "Disable archiving" agent-shell-queue-toggle-archive
     :if (lambda () agent-shell-queue-archive-enabled))
    ("x" "Enable archiving" agent-shell-queue-toggle-archive
     :if (lambda () (not agent-shell-queue-archive-enabled)))]])

(defun agent-shell-queue-item-view-actions ()
  "Show available item-view actions via `annotated-completing-read'."
  (interactive)
  (let* ((visible (thread-last agent-shell-queue--item-view-action-table
                    (seq-filter (lambda (a)
                                  (let ((if-fn (plist-get a :if)))
                                    (and (plist-get a :group)
                                         (plist-get a :annotation)
                                         (or (null if-fn) (funcall if-fn))))))))
         (table (seq-map (lambda (a)
                           (cons (plist-get a :label) (plist-get a :annotation)))
                         visible)))
    (when-let* ((label (annotated-completing-read table
                                                  :prompt "item action: "
                                                  :category 'agent-shell-queue-item-action
                                                  :require-match t))
                (entry (seq-find (lambda (a) (equal (plist-get a :label) label))
                                 visible))
                (cmd (plist-get entry :cmd)))
      (call-interactively cmd))))

(defun agent-shell-queue--build-item-menu ()
  "Regenerate `agent-shell-queue-item-menu' from action table."
  (let* ((action-entries (seq-filter (lambda (a) (plist-get a :group))
                                     agent-shell-queue--item-view-action-table))
         (group-forms
          (seq-map
           (lambda (gname)
             (apply #'vector
                    gname
                    (seq-map
                     (lambda (a)
                       (let ((key    (plist-get a :key))
                             (label  (plist-get a :label))
                             (cmd    (plist-get a :cmd))
                             (if-fn  (plist-get a :if)))
                         (if if-fn
                             (list key label cmd :if if-fn)
                           (list key label cmd))))
                     (seq-filter (lambda (a) (equal (plist-get a :group) gname))
                                 action-entries))))
           agent-shell-queue--item-view-action-groups)))
    (eval
     `(transient-define-prefix agent-shell-queue-item-menu ()
        "Actions for the item shown in the current item-view buffer."
        ,@group-forms)
     t)))

(agent-shell-queue--build-item-menu)

(defun agent-shell-queue-buffer-move-up ()
  "Move the item at point one position earlier."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-up id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-move-down ()
  "Move the item at point one position later."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-move-down id)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-enable-background-task ()
  "Flag the item at point for background sub-agent execution."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id t)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-disable-background-task ()
  "Clear the background sub-agent flag from the item at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (item (cdr (agent-shell-queue--item-by-id id))))
    (agent-shell-queue--assert-not-running item)
    (agent-shell-queue-set-background-task id nil)
    (agent-shell-queue-buffer-refresh)))

(defun agent-shell-queue-buffer-assign ()
  "Assign the item at point to a compatible buffer or unassigned.
Candidate buffers are filtered by the item's kind via the type registry.
Offers nil/unassigned as an option for deferred assignment."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id)))
    (agent-shell-queue--assert-not-running (cdr pair))
    (let* ((item (cdr pair))
           (kind (agent-shell-queue-item-kind item))
           (type (agent-shell-queue--type-for-kind kind))
           (pred (when type (agent-shell-queue-item-type-buffer-pred type)))
           (bufs (if pred
                     (seq-filter (lambda (b) (and (buffer-live-p b) (funcall pred b)))
                                 (buffer-list))
                   (agent-shell-buffers)))
           (current-dir (when-let* ((b (get-buffer (car pair))))
                          (buffer-local-value 'default-directory b)))
           (rows (seq-map (lambda (it)
                            (let* ((name (buffer-name it))
                                   (dir (buffer-local-value 'default-directory it))
                                   (dir-str (if (and current-dir (equal dir current-dir))
                                                (concat "(same dir) " (abbreviate-file-name dir))
                                              (abbreviate-file-name (or dir "")))))
                              (cons name
                                    (agent-shell-queue--annotation
                                     (format "%s  %s" dir-str
                                             (agent-shell-queue--buffer-state-label name))
                                     60))))
                          bufs))
           (table (cons (cons agent-shell-queue--unassigned-key "defer — unassigned bucket")
                        rows)))
      (when-let* ((new-name (annotated-completing-read table
                                                       :prompt "assign to: "
                                                       :category 'agent-shell-buffer
                                                       :require-match t
                                                       :history 'agent-shell-queue-buffer-assign))
                  ((not (equal new-name (car pair)))))
        (agent-shell-queue--assign-item id new-name)))))

(defun agent-shell-queue-buffer-context-menu ()
  "Offer context-sensitive actions for the item at point via `completing-read'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (let* ((status (agent-shell-queue-item-status item))
           (done (eq status 'done))
           (bg (agent-shell-queue-item-background item))
           (running (eq status 'running))
           (cmds (append
                  (unless done
                    (seq-remove #'null
                                (list
                       (cons "send now" #'agent-shell-queue-buffer-send)
                       (when (eq status 'active)
                         (cons "pause (suspend from dispatch)" #'agent-shell-queue-buffer-pause))
                       (when (agent-shell-queue--blocked-status-p status)
                         (cons "unblock" #'agent-shell-queue-buffer-unblock))
                       (when (agent-shell-queue--blocked-status-p status)
                         (cons "schedule (resume dispatch)" #'agent-shell-queue-buffer-schedule))
                       (when running
                         (cons "enqueue copy (repeat after current run)" #'agent-shell-queue-buffer-enqueue-running-copy))
                       (when running
                         (cons "untrack (remove from queue without aborting)" #'agent-shell-queue-buffer-untrack-running))
                       (unless running
                         (if bg
                             (cons "disable background sub-agent" #'agent-shell-queue-buffer-disable-background-task)
                           (cons "enable background sub-agent" #'agent-shell-queue-buffer-enable-background-task)))
                       (unless running (cons "assign to shell" #'agent-shell-queue-buffer-assign))
                       (unless running (cons "move up" #'agent-shell-queue-buffer-move-up))
                       (unless running (cons "move down" #'agent-shell-queue-buffer-move-down))
                       (cons "insert pause checkpoint" #'agent-shell-queue-insert-pause)
                       (cons "insert context drop" #'agent-shell-queue-insert-clear-context))))
                  (when done
                    (list (cons "re-enqueue (new active copy)" #'agent-shell-queue-buffer-reenqueue)))
                  (unless running
                    (list (cons "remove" #'agent-shell-queue-buffer-remove)))))
           (table (seq-map (lambda (it)
                             (cons (car it)
                                   (or (car (split-string (or (documentation (cdr it)) "") "\n")) "")))
                           cmds)))
      (when-let* ((choice (annotated-completing-read table
                                                     :prompt "action => "
                                                     :category 'agent-shell-queue-action
                                                     :require-match t
                                                     :history 'agent-shell-queue-buffer-context-menu))
                  (cmd (cdr (assoc choice cmds))))
        (call-interactively cmd)))))

(defun agent-shell-queue-buffer-toggle-only-mode ()
  "Toggle queue-only mode in the shell buffer for the item at point.
Done and aborted items cannot be attached to a shell — re-enqueue them first.
When the shell buffer is dead, picks a live replacement via `completing-read'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair))
              (item (cdr pair)))
    (when (memq (agent-shell-queue-item-status item) '(done aborted))
      (user-error "Cannot toggle queue-only mode for a %s item; re-enqueue it first"
                  (agent-shell-queue-item-status item)))
    (let ((buf (or (get-buffer buf-name)
                   (agent-shell-queue--pick-buffer
                    (format "Buffer '%s' is gone. Enable queue-only mode in: " buf-name))
                   (user-error "No live agent-shell buffers available"))))
      (with-current-buffer buf
        (agent-shell-queue-set-input-mode
         (if (eq agent-shell-queue-input-mode 'queue-only) 'default 'queue-only))))))

(defun agent-shell-queue-intercept-p ()
  "Return non-nil when queue-intercept mode is active in the current shell buffer."
  (and (featurep 'agent-shell-queue)
       (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
         (eq (buffer-local-value 'agent-shell-queue-input-mode shell) 'queue-intercept))))

(defun agent-shell-queue-input-mode-value ()
  "Return `agent-shell-queue-input-mode' for current shell buffer or default."
  (or (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
        (buffer-local-value 'agent-shell-queue-input-mode shell))
      'default))

(defun agent-shell-queue-only-enable ()
  "Enable queue-only input mode in current shell buffer."
  (interactive)
  (agent-shell-queue-set-input-mode 'queue-only))

(defun agent-shell-queue-only-p ()
  "Return non-nil when queue-only mode is active in the current shell buffer."
  (and (featurep 'agent-shell-queue)
       (when-let* ((shell (agent-shell-menu--session-shell-buffer)))
         (eq (buffer-local-value 'agent-shell-queue-input-mode shell) 'queue-only))))

(defun agent-shell-queue-only-disable-in-buffer (&optional buf)
  "Disable queue-only input mode in BUF (defaults to current buffer)."
  (interactive)
  (with-current-buffer (or (when (bufferp buf) buf)
                           (when (get-buffer buf) buf)
                           (current-buffer))
    (when (eq agent-shell-queue-input-mode 'queue-only)
      (agent-shell-queue-set-input-mode 'default))))

(defun agent-shell-queue-only-disable ()
  "Disable queue-only input mode in current shell buffer."
  (interactive)
  (agent-shell-queue-only-disable-in-buffer))

(defun agent-shell-queue-only-disable-all ()
  "Disable queue-only input mode across all `agent-shell' buffers."
  (interactive)
  (let ((cleared (seq-filter (lambda (buf)
                               (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                   'queue-only))
                             (agent-shell-buffers))))
    (seq-do #'agent-shell-queue-only-disable-in-buffer cleared)
    (when cleared (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: queue-only disabled in %d buffer(s)" (length cleared))))
(defun agent-shell-queue-buffer-open-shell ()
  "Switch to the shell buffer for the item at point.
If the buffer is not live and the item's executor provides a create
function, offer to create a new buffer of the same type."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pair (agent-shell-queue--item-by-id id))
              (buf-name (car pair)))
    (if-let* ((buf (get-buffer buf-name)))
        (pop-to-buffer buf)
      (if-let* ((item (cdr pair))
                (executor-fn (agent-shell-queue-item-executor item))
                (executor-name (agent-shell-queue--executor-name executor-fn))
                (entry (agent-shell-queue--find-executor executor-name))
                (create-fn (agent-shell-queue-executor-create entry)))
          (when (y-or-n-p (format "Buffer %s is not live.  Create a new one? " buf-name))
            (when-let* ((new-buf (funcall create-fn)))
              (pop-to-buffer new-buf)))
        (user-error "Shell buffer %s is not live" buf-name)))))

(defun agent-shell-queue-item-view-open-shell ()
  "Switch to the shell buffer for the item shown in this view.
If the buffer is not live and the item's executor provides a create
function, offer to create a new buffer of the same type."
  (interactive)
  (when-let* ((buf-name (agent-shell-queue--iv-target)))
    (if-let* ((buf (get-buffer buf-name)))
        (pop-to-buffer buf)
      (if-let* ((item (agent-shell-queue--iv-item))
                (executor-fn (agent-shell-queue-item-executor item))
                (executor-name (agent-shell-queue--executor-name executor-fn))
                (entry (agent-shell-queue--find-executor executor-name))
                (create-fn (agent-shell-queue-executor-create entry)))
          (when (y-or-n-p (format "Buffer %s is not live.  Create a new one? " buf-name))
            (when-let* ((new-buf (funcall create-fn)))
              (pop-to-buffer new-buf)))
        (user-error "Shell buffer %s is not live" buf-name)))))

(transient-define-prefix agent-shell-queue-destructive-menu ()
  "Destructive actions for the item at point in the queue buffer."
  [["Destructive"
    ("A" "Archive" agent-shell-queue-buffer-archive
     :if agent-shell-queue--point-not-running-p)
    ("k" "Remove" agent-shell-queue-buffer-remove
     :if agent-shell-queue--point-not-running-p)
    ("x" "Disable archiving" agent-shell-queue-toggle-archive
     :if-non-nil agent-shell-queue-archive-enabled)
    ("x" "Enable archiving" agent-shell-queue-toggle-archive
     :if-nil agent-shell-queue-archive-enabled)]])

(define-advice agent-shell-queue-destructive-menu (:before () guard-queue-buffer)
  "Signal an error when not invoked from an `agent-shell' queue overview buffer."
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Queue menu is only available from the agent-shell queue buffer")))

(transient-define-prefix agent-shell-queue-dispatch ()
  "Actions for the item at point in the queue buffer."
  [["Session"
    (".p" "Pause" agent-shell-queue-session-pause
     :inapt-if agent-shell-queue-session-paused-p)
    (".r" "Resume" agent-shell-queue-session-resume
     :inapt-if-not agent-shell-queue-session-paused-p)
    (".k" "Recover stuck" agent-shell-queue-recover-stuck-shell)
    (".m" agent-shell-queue-toggle-input-mode
     :description (lambda ()
                    (format "Input mode: [%s]" agent-shell-queue-input-mode)))]
   ["Fork"
    :if agent-shell-queue--point-item
    ("ff" "Fork queue" agent-shell-queue-buffer-fork)
    ("fb" "Insert before" agent-shell-queue-buffer-insert-fork-before)
    ("fa" "Insert after" agent-shell-queue-buffer-insert-fork-after)
    ("fr" "Release pending" agent-shell-queue-release-pending-fork)]
   ["All"
    ("gp" "Pause all" agent-shell-queue-pause)
    ("gr" "Resume all" agent-shell-queue-resume)
    ("ga" "Resume all sessions" agent-shell-queue-unpause-all-sessions)
    ("gi" "Reset all to default" agent-shell-queue-reset-all-input-modes)
    ("gm" agent-shell-queue-set-input-mode-default
     :description (lambda ()
                    (format "[%s] Input mode default" agent-shell-queue-input-mode-default)))]
   ["Task"
    :if agent-shell-queue--point-item
    ("!" "Dispatch now" agent-shell-queue-buffer-send
     :if agent-shell-queue--point-dispatchable-p)
    ("a" "Abort" agent-shell-queue-buffer-abort
     :if agent-shell-queue--point-running-p)
    ("R" "Re-enqueue" agent-shell-queue-buffer-reenqueue
     :if agent-shell-queue--point-done-p)
    ("z" "Mark done" agent-shell-queue-buffer-mark-done
     :if agent-shell-queue--point-not-done-p)
    ("e" "Enqueue" agent-shell-queue-enqueue-dispatch)
    ("tp" "Pause item" agent-shell-queue-buffer-pause
     :if agent-shell-queue--point-active-p)
    ("tr" "Schedule" agent-shell-queue-buffer-schedule
     :if agent-shell-queue--point-deferred-p)
    ("tu" "Unblock" agent-shell-queue-buffer-unblock
     :if agent-shell-queue--point-blocked-p)
    ("tc" "Enqueue copy" agent-shell-queue-buffer-enqueue-running-copy
     :if agent-shell-queue--point-running-p)
    ("tk" "Untrack running" agent-shell-queue-buffer-untrack-running
     :if agent-shell-queue--point-running-p)
    ("te" "Edit" agent-shell-queue-edit-task)
    ("tbe" "Background on" agent-shell-queue-buffer-enable-background-task
     :if agent-shell-queue--point-editable-p
     :inapt-if agent-shell-queue--point-bg-p)
    ("tbd" "Background off" agent-shell-queue-buffer-disable-background-task
     :if agent-shell-queue--point-editable-p
     :inapt-if-not agent-shell-queue--point-bg-p)
    ("td" "Destructive…" agent-shell-queue-destructive-menu
     :if agent-shell-queue--point-not-running-p)
    ("jo" "Open shell" agent-shell-queue-buffer-open-shell)
    ("lu" "Move up" agent-shell-queue-buffer-move-up
     :if agent-shell-queue--point-editable-p)
    ("ld" "Move down" agent-shell-queue-buffer-move-down
     :if agent-shell-queue--point-editable-p)
    ("ta" "Assign to shell…" agent-shell-queue-buffer-assign
     :if agent-shell-queue--point-editable-p)]]
  [["Capture"
    ("cw" "Compose" agent-shell-queue-capture)
    ("ca" "After point" agent-shell-queue-buffer-capture-after)
    ("cu" "Unassigned" agent-shell-queue-capture-unassigned)
    ("cr" "From region" agent-shell-queue-capture-from-region)
    ("cy" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("cc" "From context" agent-shell-queue-capture-from-context)
    ("ce" "Enqueue prompt" agent-shell-queue-enqueue)
    ("cx" "Enqueue clear" agent-shell-queue-enqueue-clear)]
   ["Insert"
    ("ip" "Pause checkpoint" agent-shell-queue-insert-pause)
    ("id" "Context drop" agent-shell-queue-insert-clear-context)
    ("ic" "Compact (manual)" agent-shell-queue-insert-compact)
    ("ie" "Emacs call" agent-shell-queue-enqueue-emacs)
    ("iw" "Wait-until (timer)" agent-shell-queue-insert-wait)]
   ["Scope / Export"
    ("sn" "Set scope" agent-shell-queue-set-scope)
    ("sw" "Global scope" agent-shell-queue-scope-global)
    ("v" "Export to YAML" agent-shell-queue-export)
    ("sf" "Flush to disk" agent-shell-queue-flush)
    ("sd" "Show disk state" agent-shell-queue-show-disk-state)
    ("=" "Inspect item…" agent-shell-queue-buffer-inspect-item
     :if agent-shell-queue--point-item)]
   ["Display"
    ("dv" "Column options" agent-shell-queue-select-columns)
    ("db" agent-shell-queue-toggle-buffer-column
     :description (lambda ()
                    (if agent-shell-queue-show-buffer-column
                        "[x] Buffer column"
                      "[ ] Buffer column")))
    ("dn" agent-shell-queue-toggle-ordinal-column
     :description (lambda ()
                    (if agent-shell-queue-show-ordinal-column
                        "[x] Ordinal column"
                      "[ ] Ordinal column")))
    ("da" agent-shell-queue-toggle-age-column
     :description (lambda ()
                    (if agent-shell-queue-show-age-column
                        "[x] Age column"
                      "[ ] Age column")))
    ("dk" agent-shell-queue-toggle-kind-column
     :description (lambda ()
                    (if agent-shell-queue-show-kind-column
                        "[x] Kind column"
                      "[ ] Kind column")))
    ("dm" agent-shell-queue-toggle-multiline-format
     :description (lambda ()
                    (if agent-shell-queue-multiline-format
                        "[x] Multi-line"
                      "[ ] Multi-line")))]
   ["Edit / Import"
    ("ly" "Raw edit (YAML)" agent-shell-queue-raw-edit)
    ("li" "Import (YAML)" agent-shell-queue-import)
    ("lr" "Reload from disk" agent-shell-queue-reload)]])

(define-advice agent-shell-queue-dispatch (:before () guard-queue-buffer)
  "Signal an error when not invoked from an `agent-shell' queue overview buffer."
  (unless (derived-mode-p 'agent-shell-queue-mode)
    (user-error "Queue menu is only available from the agent-shell queue buffer")))

;; Enqueue dispatch

(defun agent-shell-queue-enqueue-dispatch ()
  "Choose kind and target buffer via ACR, then collect input.
Choices are built from the item-type registry.  nil/unassigned is always
offered as a target so items can be deferred for later assignment."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((choices (seq-map
                   (lambda (type)
                     (cons (agent-shell-queue-item-type-label type)
                           (let ((pred (agent-shell-queue-item-type-buffer-pred type)))
                             (cond
                              ((null pred)                                           "any buffer or unassigned")
                              ((eq pred #'agent-shell-queue--agent-shell-buffer-p)  "agent-shell session")
                              ((eq pred #'agent-shell-queue--eshell-buffer-p)       "eshell buffer")
                              ((eq pred #'agent-shell-queue--eat-buffer-p)          "eat buffer")
                              (t                                                     "compatible buffer")))))
                   agent-shell-queue--item-types))
         (choice (annotated-completing-read choices :prompt "enqueue: " :require-match t))
         (type (seq-find (lambda (e)
                           (equal (agent-shell-queue-item-type-label e) choice))
                         agent-shell-queue--item-types)))
    (when type
      (let ((buf (agent-shell-queue--pick-buffer-for-kind
                  (agent-shell-queue-item-type-kind type)
                  "Target (or unassigned): ")))
        (agent-shell-queue--invoke-input-for-type type buf)))))

(defun agent-shell-queue--yaml-str (s)
  "Format string S as a quoted YAML scalar, escaping special characters."
  (concat "\""
          (replace-regexp-in-string
           "\""
           "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
          "\""))

(defun agent-shell-queue--yaml-block (s indent)
  "Format S as a YAML literal block scalar with INDENT prefix on lines."
  (concat "|\n"
          (mapconcat (lambda (line)
                       (if (string-empty-p line) "" (concat indent line)))
                     (split-string s "\n") "\n")))

(defun agent-shell-queue--item-to-yaml-export (item)
  "Format ITEM as a YAML mapping string for export.
Multi-line fields are formatted as literal block scalars."
  (with-temp-buffer
    (let* ((id (agent-shell-queue-item-id item))
           (args (agent-shell-queue-item-args item))
           (response (agent-shell-queue-item-response item))
           (status (symbol-name (agent-shell-queue-item-status item)))
           (kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
           (bg (agent-shell-queue-item-background item))
           (created (agent-shell-queue-item-created item))
           (dispatched (agent-shell-queue-item-dispatched item))
           (completed (agent-shell-queue-item-completed item)))
      (insert "  - id: " id "\n")
      (insert "    prompt: ")
      (if (string-match-p "\n" args)
          (insert (agent-shell-queue--yaml-block args "      "))
        (insert (agent-shell-queue--yaml-str args) "\n"))
      (insert "    status: " status "\n")
      (insert "    kind: " kind "\n")
      (insert "    background: " (if bg "true" "false") "\n")
      (when created (insert "    created: " (number-to-string created) "\n"))
      (when dispatched (insert "    dispatched: " (number-to-string dispatched) "\n"))
      (when completed (insert "    completed: " (number-to-string completed) "\n"))
      (when response
        (insert "    response: ")
        (if (string-match-p "\n" response)
            (insert (agent-shell-queue--yaml-block response "      "))
          (insert (agent-shell-queue--yaml-str response) "\n"))))
    (buffer-string)))

;;;###autoload
(defun agent-shell-queue-export ()
  "Export items in the current scope to a read-only YAML buffer.
Multi-line prompt/response fields are formatted as literal block scalars."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((scope agent-shell-queue--display-scope)
         (out-name (format "*agent-shell-queue-export: %s*"
                           (agent-shell-queue--scope-label scope)))
         (multi-p (> (apply #'+
                            (seq-map (lambda (it) (length (cdr it)))
                                     (agent-shell-queue-store-items agent-shell-queue--store)))
                     1))
         (visible (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                    (seq-filter (lambda (it) (agent-shell-queue--scope-matches-p (car it) scope)))
                    (seq-map (lambda (it)
                               (let ((items (if multi-p
                                                (seq-remove
                                                 (lambda (item)
                                                   (and (eq (agent-shell-queue-item-status item) 'done)
                                                        (memq (agent-shell-queue-item-kind item)
                                                              '(pause compact context))))
                                                 (cdr it))
                                              (cdr it))))
                                 (cons (car it) items))))
                    (seq-remove (lambda (pair) (null (cdr pair))))))
         (yaml-str
          (with-temp-buffer
            (seq-do (lambda (pair)
                      (insert "- buffer: " (agent-shell-queue--yaml-str (car pair)) "\n")
                      (insert "  items:\n")
                      (seq-do (lambda (item)
                                (insert (agent-shell-queue--item-to-yaml-export item)))
                              (cdr pair)))
                    visible)
            (buffer-string))))
    (with-current-buffer (get-buffer-create out-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if visible yaml-str ""))
        (goto-char (point-min))
        (when (fboundp 'yaml-mode)
          (yaml-mode)))
      (display-buffer (current-buffer)))))

(defvar agent-shell-queue-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-edit-cancel)
    (define-key m (kbd "C-x C-s") #'agent-shell-queue-edit-save-and-flush)
    (define-key m (kbd "C-c C-f") #'agent-shell-queue-insert-file)
    (define-key m (kbd "C-c M-f") #'agent-shell-queue-insert-buffer)
    m)
  "Keymap for `agent-shell-queue-edit-mode'.")

(define-derived-mode agent-shell-queue-edit-mode markdown-mode "Queue-Edit"
  "Mode for editing a queued prompt in a popup buffer.")

(defvar-local agent-shell-queue--editing-id nil
  "Item ID being edited in this `agent-shell-queue-edit-mode' buffer.")

(defun agent-shell-queue--open-edit-for-id (id)
  "Open an edit popup for the item with ID.
Enforces the one-edit-at-a-time constraint: if a different item is already
being edited, switches to that buffer and signals an error."
  (when-let* ((pair (agent-shell-queue--item-by-id id))
              (item (cdr pair)))
    (if-let* ((existing (get-buffer "*agent-shell-queue-edit*"))
              (_ (buffer-live-p existing))
              (_ (not (equal (buffer-local-value 'agent-shell-queue--editing-id existing) id))))
        (progn
          (pop-to-buffer existing '(display-buffer-below-selected))
          (user-error "Agent-shell-queue: already editing item %s — save or cancel first"
                      (buffer-local-value 'agent-shell-queue--editing-id existing)))
      (let ((edit-buf (get-buffer-create "*agent-shell-queue-edit*")))
          (with-current-buffer edit-buf
            (when-let* ((prev-id agent-shell-queue--editing-id))
              (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
                    (delete prev-id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
            (erase-buffer)
            (insert (agent-shell-queue-item-args item))
            (agent-shell-queue-edit-mode)
            (setq-local agent-shell-queue--editing-id id)
            (let* ((bucket-name (car pair))
                   (bucket-items (cdr (assoc bucket-name (agent-shell-queue-store-items agent-shell-queue--store))))
                   (depth (agent-shell-queue--active-item-count bucket-items))
                   (state (agent-shell-queue--activity-state)))
              (setq-local header-line-format
                          (concat
                           (propertize (format " %s  |  " bucket-name) 'face 'shadow)
                           state
                           (propertize (format "  |  depth: %d" depth) 'face 'shadow)))))
          (cl-pushnew id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue) :test #'equal)
          (agent-shell-queue--refresh-buffer)
          (pop-to-buffer edit-buf '(display-buffer-below-selected))))))

;;;###autoload
(defun agent-shell-queue-edit-task (&optional select)
  "Edit a queued item's prompt.
In `agent-shell-queue-mode' without SELECT (prefix argument): edit the item at
point immediately.  With SELECT, or when point carries no item, or when called
from outside `agent-shell-queue-mode': select via `annotated-completing-read'.
Candidates include all non-done, non-running items across all buffers."
  (interactive "P")
  (agent-shell-queue--ensure-loaded)
  (if-let* ((_ (not select))
            (_ (derived-mode-p 'agent-shell-queue-mode))
            (id (tabulated-list-get-id)))
      (agent-shell-queue--open-edit-for-id id)
    (let ((table (make-hash-table :test #'equal))
          (id-by-key (make-hash-table :test #'equal)))
      (seq-do (lambda (pair)
                (let* ((buf-name (car pair))
                       (buf (get-buffer buf-name))
                       (buf-state (cond
                                   ((member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)) "paused")
                                   ((and buf (with-current-buffer buf (shell-maker-busy))) "busy")
                                   (t "idle")))
                       (it-index 0))
                  (seq-do
                   (lambda (it)
                     (unless (memq (agent-shell-queue-item-status it) '(done running))
                       (let* ((id (agent-shell-queue-item-id it))
                              (prompt (agent-shell-queue-item-args it))
                              (status (agent-shell-queue--status-string it))
                              (age (agent-shell-queue--format-age
                                    (time-since (agent-shell-queue-item-created it))))
                              (pos (1+ it-index))
                              (key (format "%s: %s" id
                                           (agent-shell-queue--annotation prompt 60)))
                              (ann (format "#%d · %s [%s] · %s · %s"
                                           pos buf-name buf-state status age)))
                         (setf (map-elt table key) ann)
                         (setf (map-elt id-by-key key) id)))
                     (cl-incf it-index))
                   (cdr pair))))
              (agent-shell-queue-store-items agent-shell-queue--store))
      (when (zerop (hash-table-count table))
        (user-error "No editable queued items"))
      (when-let* ((choice (annotated-completing-read table
                                                     :prompt "edit task: "
                                                     :category 'agent-shell-queue-item
                                                     :require-match t
                                                     :history 'agent-shell-queue-edit-task))
                  (id (map-elt id-by-key choice)))
        (agent-shell-queue--open-edit-for-id id)))))

(defun agent-shell-queue-edit-save-and-flush ()
  "Save the edited prompt, close the popup, and flush the queue to disk."
  (interactive)
  (agent-shell-queue-edit-confirm)
  (agent-shell-queue--save)
  (message "agent-shell-queue: edit saved and flushed to disk"))

(defun agent-shell-queue-edit-confirm ()
  "Save the edited prompt and close the popup."
  (interactive)
  (let ((new-prompt (string-trim (buffer-string)))
        (id agent-shell-queue--editing-id))
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)))
    (quit-window t)
    (unless (string-empty-p new-prompt)
      (agent-shell-queue-edit id new-prompt))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-edit-cancel ()
  "Discard edits and close the popup."
  (interactive)
  (let ((id agent-shell-queue--editing-id))
    (setf (agent-shell-queue-queue-editing-ids agent-shell-queue--queue)
          (delete id (agent-shell-queue-queue-editing-ids agent-shell-queue--queue))))
  (quit-window t)
  (agent-shell-queue--refresh-buffer))

(defvar agent-shell-queue-raw-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-raw-edit-confirm)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-raw-edit-cancel)
    m)
  "Keymap for `agent-shell-queue-raw-edit-mode'.")

(define-derived-mode agent-shell-queue-raw-edit-mode text-mode "Queue-RawEdit"
  "Mode for directly editing the queue in YAML format.
Every session not already paused is paused while this buffer is live.
Confirm with \\[agent-shell-queue-raw-edit-confirm], cancel with \\[agent-shell-queue-raw-edit-cancel].
\\{agent-shell-queue-raw-edit-mode-map}")

(defvar-local agent-shell-queue--raw-edit-snapshot nil
  "Hash-table of id→item for the queue state when raw edit was started.")

(defvar-local agent-shell-queue--raw-edit-newly-paused nil
  "Buffer names added to the session-paused list when raw edit was started.
Only these are removed again on confirm/cancel, so a buffer that was already
individually paused before raw edit began stays paused afterward.")

(defun agent-shell-queue--item-to-yaml-edit (item)
  "Convert ITEM to a hash-table for raw editing; omits nil timestamp fields."
  (map-into
   (append
    (list (cons "id" (agent-shell-queue-item-id item))
          (cons "prompt" (agent-shell-queue-item-args item))
          (cons "status" (symbol-name (agent-shell-queue-item-status item)))
          (cons "kind" (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
          (cons "background" (if (agent-shell-queue-item-background item) t nil))
          (cons "created" (agent-shell-queue-item-created item)))
    (when (agent-shell-queue-item-dispatched item)
      (list (cons "dispatched" (agent-shell-queue-item-dispatched item))))
    (when (agent-shell-queue-item-completed item)
      (list (cons "completed" (agent-shell-queue-item-completed item))))
    (when (agent-shell-queue-item-directory item)
      (list (cons "directory" (agent-shell-queue-item-directory item)))))
   '(hash-table :test equal)))

(defun agent-shell-queue--render-to-yaml ()
  "Render active/deferred queue items to a YAML string for raw editing."
  (unless (fboundp 'yaml-encode)
    (error "Yaml-encode not available; install the `yaml' package"))
  ;; if-let*
  (let ((buckets (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                              (seq-map (lambda (pair)
                                         (when-let* ((items (seq-remove
                                                             (lambda (item)
                                                               (memq (agent-shell-queue-item-status item)
                                                                     '(done running)))
                                                             (cdr pair))))
                                           (map-into (list (cons "buffer" (car pair))
                                                           (cons "items"
                                                                 (vconcat (seq-map #'agent-shell-queue--item-to-yaml-edit items))))
                                                     '(hash-table :test equal)))))
                              (seq-remove #'null))))
    (if buckets
	(yaml-encode (vconcat buckets))
      "")))

(defun agent-shell-queue--make-edit-snapshot ()
  "Return a hash-table mapping item ID to item struct for all current items."
  (map-into
   (seq-map (lambda (it) (cons (agent-shell-queue-item-id it) it))
            (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store)))
   '(hash-table :test equal)))

;;;###autoload
(defun agent-shell-queue-raw-edit ()
  "Open the queue for direct YAML editing.
Every session not already paused is paused while the edit buffer is live.
Confirm changes with \\[agent-shell-queue-raw-edit-confirm];
cancel with \\[agent-shell-queue-raw-edit-cancel]."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (unless (fboundp 'yaml-encode)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((buf (get-buffer-create "*agent-shell-queue-raw-edit*"))
         (newly-paused
          (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                       (seq-map #'car)
                       (seq-remove (lambda (name)
                                     (member name (agent-shell-queue-queue-session-paused
                                                   agent-shell-queue--queue)))))))
    (seq-do #'agent-shell-queue--session-pause-name newly-paused)
    (agent-shell-queue--refresh-buffer)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-shell-queue-raw-edit-mode)
        (setq agent-shell-queue--raw-edit-snapshot (agent-shell-queue--make-edit-snapshot)
              agent-shell-queue--raw-edit-newly-paused newly-paused)
        (insert (agent-shell-queue--render-to-yaml))))
    (pop-to-buffer buf '(display-buffer-below-selected (window-height . 0.5)))))

(defun agent-shell-queue--raw-edit-fail (text errors)
  "Save TEXT to a timestamped fail file, report ERRORS, leave queue paused."
  (let ((file (expand-file-name
               (format "agent-shell-queue-edit-failed-%s.yaml" (format-time-string "%Y%m%dT%H%M%S"))
               (file-name-directory (agent-shell-queue--state-file)))))
    (with-temp-file file
      (insert text))
    (seq-do (lambda (it) (message "agent-shell-queue raw edit: %s" it))
            (nreverse errors))
    (message "agent-shell-queue: %d error(s) — buffer saved to %s (queue remains paused)"
             (length errors) file)))

(cl-defun agent-shell-queue-raw-edit-confirm ()
  "Validate and apply the YAML in this raw-edit buffer."
  (interactive)
  (unless (fboundp 'yaml-parse-string)
    (user-error "Raw edit requires the `yaml' package"))
  (let* ((text (buffer-string))
         (snapshot agent-shell-queue--raw-edit-snapshot)
         (newly-paused agent-shell-queue--raw-edit-newly-paused)
         errors
         parsed)
    (condition-case err
        (setq parsed (yaml-parse-string text
                                        :object-type 'hash-table
                                        :sequence-type 'list
                                        :null-object nil
                                        :false-object nil))
      (error (push (format "YAML parse error: %s" (cadr err)) errors)
             (cl-return-from agent-shell-queue-raw-edit-confirm
               (agent-shell-queue--raw-edit-fail text errors))))
    (let ((all-ids nil)
          (new-buckets nil))
      (thread-last (agent-shell-queue--yaml-buckets parsed)
        (seq-filter #'hash-table-p)
        (seq-do (lambda (bucket)
                  (let* ((buf-name (map-elt bucket "buffer"))
                         (items-raw (map-elt bucket "items"))
                         (items-list (if (vectorp items-raw)
                                         (append items-raw nil)
                                       items-raw))
                         (bucket-items))
                    (unless buf-name
                      (push "a bucket is missing the 'buffer' field" errors))
                    (seq-do (lambda (item-h)
                              (let ((id (map-elt item-h "id")))
                                (when (and id (member id all-ids))
                                  (push (format "duplicate ID '%s'" id) errors))
                                (when id (push id all-ids))
                                (let ((result (agent-shell-queue--parse-yaml-item item-h snapshot)))
                                  (if (cdr result)
                                      (setq errors (append errors (cdr result)))
                                    (push (car result) bucket-items)))))
                            (seq-filter #'hash-table-p items-list))
                    (when (and buf-name bucket-items)
                      (push (cons buf-name (nreverse bucket-items)) new-buckets))))))
      (when errors
        (cl-return-from agent-shell-queue-raw-edit-confirm
          (agent-shell-queue--raw-edit-fail text errors)))
      ;; Preserve running/done items from current queue
      (let ((preserved (thread-last (agent-shell-queue-store-items agent-shell-queue--store)
                         (seq-map (lambda (it)
                                    (let ((kept (seq-filter
                                                 (lambda (item)
                                                   (memq (agent-shell-queue-item-status item)
                                                         '(running done)))
                                                 (cdr it))))
                                      (when kept (cons (car it) kept)))))
                         (seq-remove #'null))))
        (let ((result (nreverse new-buckets)))
          (seq-do (lambda (it)
                    (if-let* ((cell (assoc (car it) result)))
                        (setcdr cell (append (cdr cell) (cdr it)))
                      (push it result)))
                  preserved)
          (setf (agent-shell-queue-store-items agent-shell-queue--store)
                (seq-remove #'agent-shell-queue--bucket-empty-p result))))
      (seq-do #'agent-shell-queue--session-unpause-name newly-paused)
      (agent-shell-queue--save)
      (quit-window t)
      (agent-shell-queue--refresh-buffer)
      (message "agent-shell-queue: raw edit applied%s"
               (if newly-paused
                   (format " (%d session(s) resumed)" (length newly-paused))
                 "")))))

(defun agent-shell-queue-raw-edit-cancel ()
  "Cancel raw edit; resume exactly the sessions this raw edit newly paused."
  (interactive)
  (let ((newly-paused agent-shell-queue--raw-edit-newly-paused))
    (quit-window t)
    (seq-do #'agent-shell-queue--session-unpause-name newly-paused)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: raw edit cancelled%s"
             (if newly-paused
                 (format " (%d session(s) resumed)" (length newly-paused))
               ""))))

;;;###autoload
(defun agent-shell-queue-import (&optional source)
  "Import queue items from SOURCE (YAML).
With no prefix arg reads from clipboard; with prefix arg prompts for file.
For items whose ID exists, prompts to keep, replace, or assign new ID."
  (interactive (list (if current-prefix-arg 'file 'clipboard)))
  (unless (fboundp 'yaml-parse-string)
    (user-error "Import requires the `yaml' package"))
  (agent-shell-queue--ensure-loaded)
  (let* ((text
          (if (eq source 'file)
              (let ((f (read-file-name "Import YAML from file: ")))
                (with-temp-buffer (insert-file-contents f) (buffer-string)))
            (or (ignore-errors (gui-get-selection 'CLIPBOARD))
                (user-error "Clipboard is empty"))))
         (parsed
          (condition-case err
              (yaml-parse-string text
                                 :object-type 'hash-table
                                 :sequence-type 'list
                                 :null-object nil
                                 :false-object nil)
            (error (user-error "YAML parse error: %s" (cadr err)))))
         (added 0)
         (skipped 0))
    (thread-last (agent-shell-queue--yaml-buckets parsed)
      (seq-filter #'hash-table-p)
      (seq-do (lambda (bucket)
                (let* ((buf-name (map-elt bucket "buffer"))
                       (items-raw (map-elt bucket "items"))
                       (items-list (cond
                                    ((vectorp items-raw) (append items-raw nil))
                                    ((listp items-raw) items-raw)
                                    (t nil))))
                  (seq-do (lambda (it)
                            (let* ((raw-id (map-elt it "id"))
                                   (existing (and raw-id (agent-shell-queue--item-by-id raw-id)))
                                   (final-id
                                    (cond
                                     ((null raw-id) (agent-shell-queue--gen-id))
                                     ((null existing) raw-id)
                                     (t (let ((choice (completing-read
                                                       (format "ID '%s' exists — " raw-id)
                                                       '("keep existing (skip)"
                                                         "replace existing"
                                                         "assign new ID")
                                                       nil t)))
                                          (cond
                                           ((string-prefix-p "keep" choice) 'skip)
                                           ((string-prefix-p "replace" choice) raw-id)
                                           (t (agent-shell-queue--gen-id))))))))
                              (cond
                               ((eq final-id 'skip) (cl-incf skipped))
                               (t
                                (when (and (stringp final-id) (equal final-id raw-id) existing)
                                  (agent-shell-queue-remove raw-id))
                                (let* ((prompt (or (map-elt it "args") (map-elt it "prompt") ""))
                                       (status-str (map-elt it "status" "active"))
                                       (status (condition-case nil (intern status-str) (error 'active)))
                                       (kind-str (map-elt it "kind" "prompt"))
                                       (kind (condition-case nil (intern kind-str) (error 'prompt)))
                                       (bg (eq t (map-elt it "background")))
                                       (target-buf (and buf-name
                                                        (not (equal buf-name agent-shell-queue--unassigned-key))
                                                        (get-buffer buf-name)))
                                       (item (agent-shell-queue-item--make
                                              :id final-id
                                              :args (if (string-empty-p (string-trim (or prompt "")))
                                                        "(imported)" (string-trim prompt))
                                              :status (if (or (memq status '(active invalid))
                                                              (agent-shell-queue--blocked-status-p status))
                                                          status 'active)
                                              :kind (if (memq kind '(prompt pause context emacs wait compact)) kind 'prompt)
                                              :background bg
                                              :created (or (map-elt it "created") (float-time)))))
                                  (when target-buf
                                    (agent-shell-queue--ensure-subscription target-buf))
                                  (agent-shell-queue--add-item-to-bucket
                                   (if (and buf-name (not (string-empty-p buf-name)))
                                       buf-name
                                     agent-shell-queue--unassigned-key)
                                   item)
                                  (cl-incf added))))))
                          (seq-filter #'hash-table-p items-list))))))
    (when (> added 0)
      (agent-shell-queue--save)
      (agent-shell-queue--refresh-buffer))
    (let ((skip-note (if (> skipped 0) (format " (%d skipped)" skipped) "")))
      (message "agent-shell-queue: imported %d item(s)%s" added skip-note))))

;;; Session Management

(defface agent-shell-queue-pending-fork-face
  '((t :foreground "mediumpurple3" :slant italic))
  "Face for queue items held pending a fork operation."
  :group 'agent-shell-queue)

(defun agent-shell-queue-buffer-mark-done ()
  "Mark the item at point as done."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (agent-shell-queue-mark-done id)))

(defun agent-shell-queue-item-view-mark-done ()
  "Mark the displayed item as done."
  (interactive)
  (when-let* ((id agent-shell-queue--item-view-id))
    (agent-shell-queue-mark-done id)
    (agent-shell-queue-item-view-refresh)))

(defun agent-shell-queue--fork-build-opts ()
  "Build fork options plist interactively using `annotated-completing-read'.
Prompts for fork mode, worktree settings, and capture-pending flag.
Returns a plist suitable for `agent-shell-queue-fork-session' or nil to abort."
  (let* ((mode-choice (annotated-completing-read
                       '(("new session" . "Create a clean new session via agent-shell-new-shell")
                         ("fork session (ACP)" . "Fork via agent-shell-fork (preserves context)"))
                       :prompt "fork mode: "
                       :category 'agent-shell-fork-mode
                       :require-match t
                       :history 'agent-shell-queue-fork-mode))
         (fork-mode (if (equal mode-choice "fork session (ACP)") 'fork 'new))
         (wt-choice (annotated-completing-read
                     '(("no worktree" . "New session opens in the same working directory")
                       ("create worktree" . "Run git worktree add and open the session in the new tree"))
                     :prompt "worktree: "
                     :category 'agent-shell-fork-worktree
                     :require-match t
                     :history 'agent-shell-queue-fork-worktree))
         (use-worktree (equal wt-choice "create worktree"))
         (worktree-branch (when use-worktree
                            (let ((b (read-string "Branch name (empty = auto): ")))
                              (unless (string-empty-p b) b))))
         (worktree-path (when use-worktree
                          (let ((p (read-string "Worktree path (empty = auto): ")))
                            (unless (string-empty-p p) p))))
         (cp-choice (annotated-completing-read
                     '(("move items to new session" . "Items are moved; original session resumes automatically")
                       ("capture pending (freeze & pause)" . "Items stay in original session as pending-fork; session stays paused"))
                     :prompt "after fork: "
                     :category 'agent-shell-fork-capture
                     :require-match t
                     :history 'agent-shell-queue-fork-capture))
         (capture-pending (equal cp-choice "capture pending (freeze & pause)")))
    (list :fork-mode fork-mode
          :use-worktree use-worktree
          :worktree-branch worktree-branch
          :worktree-path worktree-path
          :capture-pending capture-pending)))

;;;###autoload
(defun agent-shell-queue-buffer-fork ()
  "Fork the queue starting at the item at point into a new session.
Prompts interactively for fork options; uses `annotated-completing-read' when
called outside the queue buffer to build options without task-at-point context."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Fork session: ")))
         (from-id (when pair id))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-fork-session buf from-id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-before ()
  "Insert a fork queue item before the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-before in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-before buf id opts)))

;;;###autoload
(defun agent-shell-queue-buffer-insert-fork-after ()
  "Insert a fork queue item after the item at point.
Prompts for fork options interactively."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((id (and (derived-mode-p 'agent-shell-queue-mode) (tabulated-list-get-id)))
         (pair (and id (agent-shell-queue--item-by-id id)))
         (buf (if pair
                  (get-buffer (car pair))
                (agent-shell-queue--pick-buffer "Insert fork-after in: ")))
         (opts (agent-shell-queue--fork-build-opts)))
    (agent-shell-queue-insert-fork-after buf id opts)))

;; File sending to capture buffers

(defun agent-shell-queue--live-capture-buffers ()
  "Return all live buffers in `agent-shell-queue-capture-mode'."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'agent-shell-queue-capture-mode)))
              (buffer-list)))

(defun agent-shell-queue--ad-agent-shell-send-file-to (orig-fn &optional prompt-for-file)
  "Around advice using ORIG-FN and PROMPT-FOR-FILE to include capture buffers.
When a capture buffer is chosen, the file context is inserted at point-max
of that buffer instead of being sent via `agent-shell-insert'."
  (let* ((capture-bufs (agent-shell-queue--live-capture-buffers))
         (shell-names (seq-map #'buffer-name (agent-shell-buffers)))
         (capture-names (seq-map #'buffer-name capture-bufs))
         (all-names (append shell-names capture-names)))
    (cond
     ((null capture-bufs)
      ;; No open capture buffers — delegate unchanged.
      (funcall orig-fn prompt-for-file))
     ((null all-names)
      (user-error "No shells or capture buffers available"))
     (t
      (let* ((chosen-name (completing-read "Send file to: " all-names nil t))
             (chosen-buf (get-buffer chosen-name)))
        (if (and chosen-buf
                 (with-current-buffer chosen-buf
                   (derived-mode-p 'agent-shell-queue-capture-mode)))
            ;; Capture buffer: intercept agent-shell-insert and insert there.
            (cl-letf (((symbol-function 'agent-shell-insert)
                       (lambda (&rest args)
                         (when-let* ((text (plist-get args :text)))
                           (with-current-buffer chosen-buf
                             (goto-char (point-max))
                             (unless (bolp) (insert "\n"))
                             (insert text))))))
              (agent-shell-send-file prompt-for-file nil))
          ;; Regular shell: pre-select via completing-read intercept.
          (cl-letf* ((real-cr (symbol-function 'completing-read))
                     ((symbol-function 'completing-read)
                      (lambda (prompt collection &rest args)
                        (if (string-match-p "[Ss]hell" prompt)
                            chosen-name
                          (apply real-cr prompt collection args)))))
            (funcall orig-fn prompt-for-file))))))))

(advice-add 'agent-shell-send-file-to :around
            #'agent-shell-queue--ad-agent-shell-send-file-to)

(defvar agent-shell-queue-interjection-continuation-suffix
  "\n\nAfter addressing the above, please resume previous task."
  "Text appended to the interjection prompt before sending.
Set to nil to send the user's text verbatim without a continuation instruction.")

(defvar-local agent-shell-queue-interjection--item nil
  "The queue item being interject-edited in this capture buffer.")

(defvar-local agent-shell-queue-interjection--shell nil
  "The `agent-shell' buffer associated with this interjection capture buffer.")

(defvar agent-shell-queue-interjection-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'agent-shell-queue-interjection-send)
    (define-key m (kbd "C-c C-k") #'agent-shell-queue-interjection-close)
    (define-key m (kbd "q")       #'agent-shell-queue-interjection-close)
    m)
  "Keymap for `agent-shell-queue-interjection-mode'.")

(define-derived-mode agent-shell-queue-interjection-mode text-mode "ASQ-Interject"
  "Capture mode for interjection messages.
Confirm with \\[agent-shell-queue-interjection-send], close with \\[agent-shell-queue-interjection-close].")

(defun agent-shell-queue--open-interjection-buffer (item buf-name)
  "Create and display an interjection capture buffer for ITEM in BUF-NAME shell."
  (let* ((shell-buf (get-buffer buf-name))
         (capture-buf (generate-new-buffer "*asq-interjection*")))
    (with-current-buffer capture-buf
      (setq-local agent-shell-queue-interjection--item item)
      (setq-local agent-shell-queue-interjection--shell shell-buf)
      (let ((inhibit-read-only t))
        (insert (format "Interjecting: %s — %s\n"
                        (agent-shell-queue-item-id item)
                        (car (split-string (agent-shell-queue-item-args item) "\n"))))
        (insert "Original prompt (truncated):\n")
        (seq-do (lambda (line) (insert "  " line "\n"))
                (seq-take (split-string (agent-shell-queue-item-args item) "\n") 3))
        (insert (make-string 60 ?─) "\n\n")
        (add-text-properties (point-min) (point) '(read-only t front-sticky (read-only))))
      (goto-char (point-max))
      (agent-shell-queue-interjection-mode))
    (pop-to-buffer capture-buf)))

(defun agent-shell-queue--interjection-readable-prompt (raw-text)
  "Return the editable user portion of RAW-TEXT in the interjection buffer.
Strips the read-only header by looking for the separator line."
  (let ((sep (make-string 60 ?─)))
    (if (string-match (concat (regexp-quote sep) "\n\n?") raw-text)
        (substring raw-text (match-end 0))
      raw-text)))

(defun agent-shell-queue-interjection-send ()
  "Send the interjection message to the agent shell and close this buffer."
  (interactive)
  (let* ((item agent-shell-queue-interjection--item)
         (shell-buf agent-shell-queue-interjection--shell)
         (buf-name (buffer-name shell-buf))
         (raw (buffer-substring-no-properties (point-min) (point-max)))
         (user-text (string-trim (agent-shell-queue--interjection-readable-prompt raw)))
         (full-text (if (and agent-shell-queue-interjection-continuation-suffix
                             (not (string-empty-p user-text)))
                        (concat user-text agent-shell-queue-interjection-continuation-suffix)
                      user-text)))
    (when (string-empty-p user-text)
      (user-error "Interjection prompt is empty — type a message or use C-c C-k to close"))
    (setf (agent-shell-queue-item-interjection-prompt item) user-text)
    ;; Remove any stale response-start entry from the original dispatch, then
    ;; track the new start position so --capture-response finds the right text.
    (setq agent-shell-queue--response-start-positions
          (seq-remove (lambda (it) (equal (car it) (agent-shell-queue-item-id item)))
                      agent-shell-queue--response-start-positions))
    (agent-shell-insert :text full-text :submit t :no-focus t :shell-buffer shell-buf)
    (push (cons (agent-shell-queue-item-id item)
                (with-current-buffer shell-buf (point-max)))
          agent-shell-queue--response-start-positions)
    (kill-buffer (current-buffer))
    (message "agent-shell-queue: interjection sent to %s — waiting for response…" buf-name)))

(defun agent-shell-queue--interjection-mark-aborted (item buf-name)
  "Mark ITEM in BUF-NAME as aborted, clear interjection-pending."
  (setf (agent-shell-queue-item-status item) 'aborted)
  (setf (agent-shell-queue-item-completed item) (float-time))
  (setf (agent-shell-queue-item-outcome item) 'interrupted)
  (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) nil)
  ;; Remove any stale response-start position.
  (setq agent-shell-queue--response-start-positions
        (seq-remove (lambda (it) (equal (car it) (agent-shell-queue-item-id item)))
                    agent-shell-queue--response-start-positions))
  (agent-shell-queue--append-done-log buf-name item)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer))

(defun agent-shell-queue-interjection-close ()
  "Close interjection buffer with choice of handling interrupted task."
  (interactive)
  (let* ((item agent-shell-queue-interjection--item)
         (shell-buf agent-shell-queue-interjection--shell)
         (buf-name (buffer-name shell-buf))
         (choice
          (annotated-completing-read
           '(("resume previous work"
              . "Send 'please continue your previous task' to the agent and close")
             ("mark done and continue"
              . "Mark item done without result; clear pause; dispatch next item")
             ("mark done and pause"
              . "Mark item done without result; leave queue paused")
             ("clear context and continue"
              . "Mark item aborted; clear session pause; let next item start fresh")
             ("insert resume task"
              . "Mark item aborted; queue a blocked resume task; stay paused"))
           :prompt "Interjection abort: "
           :require-match t)))
    (pcase choice
      ("resume previous work"
       (setf (agent-shell-queue-item-interjection-prompt item) "")
       (agent-shell-insert
        :text "Please resume your previous task where you left off."
        :submit t :no-focus t :shell-buffer shell-buf)
       (push (cons (agent-shell-queue-item-id item)
                   (with-current-buffer shell-buf (point-max)))
             agent-shell-queue--response-start-positions)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: asking agent to resume previous work in %s…" buf-name))
      ("mark done and continue"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--session-unpause-name buf-name)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (when-let* ((buf (get-buffer buf-name)))
         (agent-shell-queue--send-next-for-buffer buf))
       (message "agent-shell-queue: item aborted, queue continuing in %s" buf-name))
      ("mark done and pause"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: item aborted, queue paused in %s" buf-name))
      ("clear context and continue"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--session-unpause-name buf-name)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (when-let* ((buf (get-buffer buf-name)))
         (agent-shell-queue--send-next-for-buffer buf))
       (message "agent-shell-queue: item aborted (clear context), queue continuing in %s" buf-name))
      ("insert resume task"
       (agent-shell-queue--interjection-mark-aborted item buf-name)
       (agent-shell-queue--insert-resume-task buf-name item)
       (agent-shell-queue--save)
       (agent-shell-queue--refresh-buffer)
       (kill-buffer (current-buffer))
       (message "agent-shell-queue: resume task inserted in %s (queue remains paused)" buf-name)))))

(defun agent-shell-queue-interject-available-p ()
  "Return non-nil when `agent-shell-queue-interject' can be called.
True when queue data is loaded, a task is running or interjecting, and no
interjection buffer is already pending."
  (condition-case nil
    (and agent-shell-queue--queue
         (not (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue))
         (seq-find (lambda (item)
                     (memq (agent-shell-queue-item-status item) '(running interjecting)))
                   (seq-mapcat #'cdr
                     (agent-shell-queue-store-items agent-shell-queue--store))))
    (args-out-of-range nil)))

(defun agent-shell-queue-interject ()
  "Interrupt the currently running queue task and open an interjection buffer."
  (interactive)
  (agent-shell-queue--ensure-loaded)
  (let* ((running-item
          (seq-find (lambda (item)
                      (memq (agent-shell-queue-item-status item) '(running interjecting)))
                    (seq-mapcat #'cdr (agent-shell-queue-store-items agent-shell-queue--store))))
         (buf-name (when running-item
                     (car (agent-shell-queue--item-by-id
                           (agent-shell-queue-item-id running-item))))))
    (unless running-item
      (user-error "No running item to interject"))
    (when (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue)
      (user-error "An interjection is already in progress"))
    (setf (agent-shell-queue-item-status running-item) 'interjecting)
    (setf (agent-shell-queue-queue-interjection-pending agent-shell-queue--queue) t)
    (when-let* ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (agent-shell-interrupt)))
    (agent-shell-queue--save)
    (agent-shell-queue--refresh-buffer)
    (agent-shell-queue--open-interjection-buffer running-item buf-name)))

;;; Input Routing and Queue-Only Mode

(defvar-local agent-shell-queue-intercept-mode nil
  "When non-nil in `agent-shell' buffer, capture user-typed turns as queue items.")

(defvar-local agent-shell-queue-input-mode 'default
  "Current input routing mode for this `agent-shell' buffer.
One of `default' (normal shell input), `queue-intercept' (capture user
input as queue items while still submitting), or `queue-only' (no prompt;
all input routed through the queue).  Set via
`agent-shell-queue-set-input-mode'.")

(defface agent-shell-queue-intercept-face
  '((t :foreground "orange" :weight bold))
  "Face for [intercept] indicator appended to prompt in intercept mode."
  :group 'agent-shell-queue)

(defvar-local agent-shell-queue--intercept-overlay nil)
(defvar-local agent-shell-queue--intercept-sub-prompt nil)

(defun agent-shell-queue--on-submit-intercept (&rest _)
  "Capture user-typed shell turn as a queue item when intercept mode is active.
Installed as :before advice on `shell-maker-submit'."
  (when-let* ((_ agent-shell-queue-intercept-mode)
              (_ (called-interactively-p 'interactive))
              (_ (derived-mode-p 'agent-shell-mode))
              (buf-name (buffer-name (current-buffer)))
              (input (save-excursion
                       (goto-char (point-max))
                       (when (re-search-backward comint-prompt-regexp nil t)
                         (string-trim
                          (buffer-substring-no-properties (match-end 0) (point-max))))))
              (_ (not (string-empty-p input)))
              (_ (or (agent-shell-queue--ensure-loaded) t))
              (item (agent-shell-queue--make-item input nil 'prompt)))
  (setf (agent-shell-queue-item-status item) 'running)
  (setf (agent-shell-queue-item-dispatched item) (float-time))
  (agent-shell-queue--add-item-to-bucket buf-name item)
  (agent-shell-queue--ensure-subscription (current-buffer))
  (push (cons (agent-shell-queue-item-id item) (point-max))
        agent-shell-queue--response-start-positions)
  (agent-shell-queue--save)
  (agent-shell-queue--refresh-buffer)))

(advice-add 'shell-maker-submit :before #'agent-shell-queue--on-submit-intercept)

(defun agent-shell-queue--intercept-clear ()
  "Remove the input-intercept prompt overlay in current buffer."
  (when (overlayp agent-shell-queue--intercept-overlay)
    (delete-overlay agent-shell-queue--intercept-overlay)
    (setq agent-shell-queue--intercept-overlay nil)))

(defun agent-shell-queue--intercept-show (_event)
  "Display the input-intercept prompt overlay for _EVENT."
  (when (and (eq agent-shell-queue-input-mode 'queue-intercept)
             (derived-mode-p 'agent-shell-mode))
    (agent-shell-queue--intercept-clear)
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'after-string
                   (propertize " [intercept]" 'face 'agent-shell-queue-intercept-face))
      (overlay-put ov 'agent-shell-queue-intercept t)
      (setq agent-shell-queue--intercept-overlay ov))))

(defun agent-shell-queue-set-input-mode (mode &optional buf)
  "Set input MODE for BUF (default: current buffer).
MODE must be one of `default', `queue-intercept', or `queue-only'.
Enforces mutual exclusivity and updates the prompt indicator."
  (unless (memq mode '(default queue-intercept queue-only))
    (user-error "Invalid input mode %s: expected default, queue-intercept, or queue-only"
                mode))
  (with-current-buffer (or buf (current-buffer))
    (setq agent-shell-queue-input-mode mode)
    (setq agent-shell-queue-intercept-mode (eq mode 'queue-intercept))
    (cond
     ((eq mode 'queue-only)
      (agent-shell-queue--intercept-clear)
      (when agent-shell-queue--intercept-sub-prompt
        (agent-shell-unsubscribe :subscription agent-shell-queue--intercept-sub-prompt)
        (setq agent-shell-queue--intercept-sub-prompt nil))
      (unless agent-shell-queue-only-mode
        (agent-shell-queue-only-mode 1)))
     ((eq mode 'queue-intercept)
      (when agent-shell-queue-only-mode
        (agent-shell-queue-only-mode -1))
      (unless agent-shell-queue--intercept-sub-prompt
        (setq agent-shell-queue--intercept-sub-prompt
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'prompt-ready
               :on-event #'agent-shell-queue--intercept-show)))
      (unless (shell-maker-busy)
        (agent-shell-queue--intercept-show nil)))
     (t
      (when agent-shell-queue-only-mode
        (agent-shell-queue-only-mode -1))
      (agent-shell-queue--intercept-clear)
      (when agent-shell-queue--intercept-sub-prompt
        (agent-shell-unsubscribe :subscription agent-shell-queue--intercept-sub-prompt)
        (setq agent-shell-queue--intercept-sub-prompt nil))))
    (agent-shell-queue--refresh-buffer)))

(defun agent-shell-queue-toggle-input-mode ()
  "Cycle input mode: default → queue-intercept → queue-only → default."
  (interactive)
  (agent-shell-queue-set-input-mode
   (pcase agent-shell-queue-input-mode
     ('default 'queue-intercept)
     ('queue-intercept 'queue-only)
     (_ 'default))))

(defun agent-shell-queue-toggle-intercept-mode (&optional buf)
  "Toggle queue-intercept mode for BUF; when active, user-typed input is queued."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Toggle intercept for: "))))
  (when buf
    (with-current-buffer buf
      (agent-shell-queue-set-input-mode
       (if (eq agent-shell-queue-input-mode 'queue-intercept) 'default 'queue-intercept))
      (message "agent-shell-queue: input mode %s in %s"
               agent-shell-queue-input-mode (buffer-name buf)))))

(defun agent-shell-queue-enable-intercept-mode (&optional buf)
  "Enable queue-intercept mode in BUF so user-typed input is queued."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Enable intercept for: "))))
  (when buf
    (with-current-buffer buf
      (agent-shell-queue-set-input-mode 'queue-intercept)
      (message "agent-shell-queue: queue-intercept ENABLED in %s" (buffer-name buf)))))

(defun agent-shell-queue-disable-intercept-mode (&optional buf)
  "Disable queue-intercept mode in BUF, returning it to default input mode."
  (interactive
   (list (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
             (agent-shell-queue--pick-buffer "Disable intercept for: "))))
  (when buf
    (with-current-buffer buf
      (when (eq agent-shell-queue-input-mode 'queue-intercept)
        (agent-shell-queue-set-input-mode 'default))
      (message "agent-shell-queue: queue-intercept disabled in %s" (buffer-name buf)))))

;;;###autoload
(defun agent-shell-queue-disable-intercept-mode-all ()
  "Reset all live `agent-shell' buffers in queue-intercept mode to default."
  (interactive)
  (let ((cleared (seq-filter (lambda (buf)
                               (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                   'queue-intercept))
                             (agent-shell-buffers))))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode 'default)))
            cleared)
    (agent-shell-queue--refresh-buffer)
    (message "agent-shell-queue: queue-intercept disabled in %d buffer(s)" (length cleared))))

(defcustom agent-shell-queue-input-mode-default 'default
  "Default input routing mode for new `agent-shell' sessions.
One of `default' (normal shell input), `queue-intercept' (capture user
input as queue items while still submitting), or `queue-only' (no prompt;
all input routed through the queue).
Use `agent-shell-queue-set-input-mode-default' to change this and sync
all existing sessions simultaneously."
  :type '(choice (const :tag "Normal (direct input)" default)
                 (const :tag "Queue intercept (capture to queue)" queue-intercept)
                 (const :tag "Queue only (all input via queue)" queue-only))
  :group 'agent-shell-queue)

(defun agent-shell-queue--apply-input-mode-default ()
  "Apply `agent-shell-queue-input-mode-default' to the current buffer.
Installed on `agent-shell-mode-hook'."
  (unless (eq agent-shell-queue-input-mode-default 'default)
    (agent-shell-queue-set-input-mode agent-shell-queue-input-mode-default)))

(add-hook 'agent-shell-mode-hook #'agent-shell-queue--apply-input-mode-default)

;;;###autoload
(defun agent-shell-queue-set-input-mode-default (mode)
  "Set `agent-shell-queue-input-mode-default' to MODE and sync sessions.
MODE is prompted interactively from the three valid options.
All live `agent-shell' buffers are immediately updated to the new default."
  (interactive
   (list (intern (completing-read "Input mode default: "
                                  '("default" "queue-intercept" "queue-only")
                                  nil t nil nil "default"))))
  (unless (memq mode '(default queue-intercept queue-only))
    (user-error "Invalid mode %s: expected default, queue-intercept, or queue-only" mode))
  (setq agent-shell-queue-input-mode-default mode)
  (let ((bufs (agent-shell-buffers)))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode mode)))
            bufs)
    (when bufs (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: input mode default → %s (%d session(s) updated)"
             mode (length bufs))))

;;;###autoload
(defun agent-shell-queue-reset-all-input-modes ()
  "Reset all live `agent-shell' buffers to default input mode."
  (interactive)
  (let ((changed (seq-filter (lambda (buf)
                               (not (eq (buffer-local-value 'agent-shell-queue-input-mode buf)
                                        'default)))
                             (agent-shell-buffers))))
    (seq-do (lambda (buf)
              (with-current-buffer buf
                (agent-shell-queue-set-input-mode 'default)))
            changed)
    (when changed (agent-shell-queue--refresh-buffer))
    (message "agent-shell-queue: reset %d session(s) to default" (length changed))))

;;;###autoload
(defun agent-shell-queue-toggle-intercept-default ()
  "Toggle queue-intercept as the default input mode and sync all sessions."
  (interactive)
  (agent-shell-queue-set-input-mode-default
   (if (eq agent-shell-queue-input-mode-default 'queue-intercept) 'default 'queue-intercept)))

;;;###autoload
(defun agent-shell-queue-toggle-only-default ()
  "Toggle queue-only as the default input mode and sync all sessions."
  (interactive)
  (agent-shell-queue-set-input-mode-default
   (if (eq agent-shell-queue-input-mode-default 'queue-only) 'default 'queue-only)))

(defface agent-shell-queue-ready-face
  '((t :foreground "red" :weight bold))
  "Face for the <ready> indicator shown in `agent-shell-queue-only-mode'."
  :group 'agent-shell-queue)

(defvar-local agent-shell-queue--ready-overlay nil)
(defvar-local agent-shell-queue--ready-sub-prompt nil)
(defvar-local agent-shell-queue--ready-sub-submit nil)

(defun agent-shell-queue--ready-clear ()
  "Remove the ready prompt overlay in current buffer."
  (when (overlayp agent-shell-queue--ready-overlay)
    (delete-overlay agent-shell-queue--ready-overlay)
    (setq agent-shell-queue--ready-overlay nil)))

(defun agent-shell-queue--ready-show (_event)
  "Display the ready prompt overlay for _EVENT."
  (when (and agent-shell-queue-only-mode
             (derived-mode-p 'agent-shell-mode))
    (agent-shell-queue--ready-clear)
    (when-let* ((proc (get-buffer-process (current-buffer)))
                (pmark (process-mark proc)))
      (let ((inhibit-read-only t))
        (delete-region (marker-position pmark) (point-max))))
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'after-string
                   (propertize "<ready>" 'face 'agent-shell-queue-ready-face))
      (overlay-put ov 'agent-shell-queue-ready t)
      (setq agent-shell-queue--ready-overlay ov))))

(defun agent-shell-queue--ready-hide (_event)
  "Hide the ready prompt overlay for _EVENT."
  (agent-shell-queue--ready-clear))

;;;###autoload
(defun agent-shell-queue-ready-capture ()
  "Clear the ready overlay and open the queue enqueue dispatch menu.
Any keypress in queue-only mode at the idle prompt routes here, giving
access to all registered item kinds rather than prompt-only capture."
  (interactive)
  (agent-shell-queue--ready-clear)
  (call-interactively #'agent-shell-queue-enqueue-dispatch))

(defvar-keymap agent-shell-queue-only-mode-map
  "SPC" #'agent-shell-queue-ready-capture
  "RET" #'agent-shell-queue-ready-capture
  "<remap> <self-insert-command>" #'agent-shell-queue-ready-capture)

;;;###autoload
(define-minor-mode agent-shell-queue-only-mode
  "Route all `agent-shell' input through the queue; show <ready> when idle."
  :lighter " Q⌛"
  :keymap agent-shell-queue-only-mode-map
  (if agent-shell-queue-only-mode
      (progn
        (setq-local buffer-read-only t)
        (setq agent-shell-queue--ready-sub-prompt
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'prompt-ready
               :on-event #'agent-shell-queue--ready-show))
        (setq agent-shell-queue--ready-sub-submit
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'input-submitted
               :on-event #'agent-shell-queue--ready-hide))
        (unless (shell-maker-busy)
          (agent-shell-queue--ready-show nil)))
    (setq-local buffer-read-only nil)
    (agent-shell-queue--ready-clear)
    (when agent-shell-queue--ready-sub-prompt
      (agent-shell-unsubscribe
       :subscription agent-shell-queue--ready-sub-prompt)
      (setq agent-shell-queue--ready-sub-prompt nil))
    (when agent-shell-queue--ready-sub-submit
      (agent-shell-unsubscribe
       :subscription agent-shell-queue--ready-sub-submit)
      (setq agent-shell-queue--ready-sub-submit nil))))

(provide 'agent-shell-queue-ui)

;;; agent-shell-queue-ui.el ends here
