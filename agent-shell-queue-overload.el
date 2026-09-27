;;; agent-shell-queue-overload.el --- Overload inbuilt agent-shell prompt queue with ASQ -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a configurable overload mode (`agent-shell-queue-overload-mode')
;; that routes inbuilt `agent-shell-prompt-queue' operations into the
;; persistent, introspectable `agent-shell-queue' system.
;;
;; When active, prompt queueing gains:
;; - Persistence across Emacs sessions (JSON, plist, YAML)
;; - Full introspection via `agent-shell-queue' UI (*agent-shell-queue*)
;; - Queue pausing and session resumption
;; - Interjection and pause-after-step support
;; - Configurable input modes: default minibuffer read with an escape
;;   binding to switch mid-composition to a multi-line capture buffer,
;;   or direct capture buffer creation.
;;
;; Integrates with upstream `agent-shell' hook variables when available,
;; and falls back to advice-based interception so it works seamlessly on
;; any version of `agent-shell'.

;;; Code:

(declare-function agent-shell-queue-send-next "agent-shell-queue-core")

(require 'subr-x)
(require 'agent-shell-queue-core)
(require 'agent-shell-queue-ui)
(require 'agent-shell-prompt-queue nil t)
(eval-when-compile (require 'cl-lib))

(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--echo "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--prompt-queue-read "agent-shell-prompt-queue")
(declare-function shell-maker-busy "shell-maker")

(defgroup agent-shell-queue-overload nil
  "Overload configuration for inbuilt agent-shell prompt queue."
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-overload-entry-method 'minibuffer
  "Method used to input prompts when `agent-shell-prompt-queue' is called.
When `minibuffer', prompt is read in the minibuffer with an escape key
bound to switch mid-composition to an `agent-shell-queue' capture buffer.
When `capture-buffer', an `agent-shell-queue' multi-line capture buffer
is opened directly."
  :type '(choice (const :tag "Minibuffer (with escape binding)" minibuffer)
                 (const :tag "Capture buffer" capture-buffer))
  :group 'agent-shell-queue-overload)

(defcustom agent-shell-queue-overload-escape-key "C-c C-e"
  "Key sequence in minibuffer to escape to an `agent-shell-queue' capture buffer.
When pressed in the prompt minibuffer, the current contents are moved into
a newly opened capture buffer."
  :type 'string
  :group 'agent-shell-queue-overload)

(defcustom agent-shell-queue-overload-echo-queue t
  "Whether to echo active and queued prompt status after enqueueing."
  :type 'boolean
  :group 'agent-shell-queue-overload)

(defvar agent-shell-queue-overload--escape-handoff nil
  "Internal handoff cell of (TARGET-BUFFER . TEXT) when escaping to capture.")

(defvar-local agent-shell-queue-overload--current-shell-buffer nil
  "Target shell buffer stored in the minibuffer during prompt reading.")

;;; Buffer and item helpers

(defun agent-shell-queue-overload--target-shell-buffer (&optional buf)
  "Resolve the active shell buffer for BUF."
  (or buf
      (if (fboundp 'agent-shell--shell-buffer)
          (agent-shell--shell-buffer :no-create t)
        (current-buffer))))

(defun agent-shell-queue-overload--items-for-buffer (&optional buf)
  "Return list of all queued items for BUF."
  (agent-shell-queue--ensure-loaded)
  (let* ((target (agent-shell-queue-overload--target-shell-buffer buf))
         (buf-name (if (bufferp target) (buffer-name target) (or target ""))))
    (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store)))))

(defun agent-shell-queue-overload--active-items (&optional buf)
  "Return active queued items for BUF."
  (seq-filter (lambda (item)
                (eq (agent-shell-queue-item-status item) 'active))
              (agent-shell-queue-overload--items-for-buffer buf)))

(defun agent-shell-queue-overload--echo-status (new-prompt buf)
  "Echo queue status showing active turn and queued prompts in BUF."
  (let* ((active-prompt (when (bound-and-true-p comint-input-ring)
                          (and (not (ring-empty-p comint-input-ring))
                               (ring-ref comint-input-ring 0))))
         (pending (agent-shell-queue-overload--active-items buf)))
    (if (fboundp 'agent-shell--echo)
        (let ((available (- (frame-width) 8)))
          (agent-shell--echo
           "%s"
           (mapconcat
            (lambda (row)
              (concat
               (propertize (string-pad (alist-get :status row) 6)
                           'face (alist-get :face row))
               "  "
               (truncate-string-to-width
                (or (car (split-string (alist-get :prompt row) "\n" t)) "")
                available nil nil t)))
            (append
             (when active-prompt
               (list `((:status . "active")
                       (:face . success)
                       (:prompt . ,active-prompt))))
             (seq-map (lambda (item)
                        `((:status . "queued")
                          (:face . agent-shell-secondary)
                          (:prompt . ,(or (agent-shell-queue-item-args item) ""))))
                      pending))
            "\n")))
      (message "Queued in ASQ (%d pending): %s"
               (length pending)
               (truncate-string-to-width new-prompt 50 nil nil "...")))))

;;; Core overload operations

(defun agent-shell-queue-overload-enqueue (prompt &optional buf)
  "Enqueue PROMPT into `agent-shell-queue' for BUF.
If the shell is idle, submits immediately.  If busy, creates a persistent
`agent-shell-queue' item with unique ID, persistence, and pause support."
  (let* ((target-buf (agent-shell-queue-overload--target-shell-buffer buf))
         (busy (and (buffer-live-p target-buf)
                    (fboundp 'shell-maker-busy)
                    (with-current-buffer target-buf (shell-maker-busy)))))
    (if (not busy)
        (with-current-buffer target-buf
          (if (fboundp 'agent-shell--insert-to-shell-buffer)
              (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
            (insert prompt)
            (when (fboundp 'comint-send-input)
              (comint-send-input))))
      (let ((item (agent-shell-queue-add prompt target-buf)))
        (when agent-shell-queue-overload-echo-queue
          (agent-shell-queue-overload--echo-status prompt target-buf))
        item))))

(defun agent-shell-queue-overload-process-next ()
  "Process the next pending prompt from `agent-shell-queue'.
Called upon turn completion or manual resume."
  (let ((buf (agent-shell-queue-overload--target-shell-buffer)))
    (when (buffer-live-p buf)
      (agent-shell-queue--send-next-for-buffer buf))))

(defun agent-shell-queue-overload-resume ()
  "Resume processing pending prompts in `agent-shell-queue' for the shell buffer.
If the shell session is paused in ASQ, resumes it.  Otherwise dispatches
the next queued prompt if not busy."
  (interactive)
  (let* ((buf (agent-shell-queue-overload--target-shell-buffer))
         (buf-name (and (buffer-live-p buf) (buffer-name buf))))
    (agent-shell-queue--ensure-loaded)
    (cond
     ((and buf-name agent-shell-queue--queue (member buf-name (agent-shell-queue-queue-session-paused agent-shell-queue--queue)))
      (agent-shell-queue-session-resume buf-name)
      (message "Resumed paused agent-shell-queue session: %s" buf-name))
     (t
      (let ((pending (agent-shell-queue-overload--active-items buf)))
        (when (seq-empty-p pending)
          (user-error "No pending prompts in queue"))
        (if (and (fboundp 'shell-maker-busy)
                 (with-current-buffer buf (shell-maker-busy)))
            (message "Shell is busy, prompts will auto-resume when ready")
          (agent-shell-queue-send-next buf)))))))

(defun agent-shell-queue-overload-remove (&optional remove-index)
  "Remove pending prompts from `agent-shell-queue'.
If REMOVE-INDEX is an integer, removes that prompt by index.
When called interactively, prompts via `completing-read' to remove a specific
item or remove all."
  (interactive)
  (let* ((buf (agent-shell-queue-overload--target-shell-buffer))
         (pending (agent-shell-queue-overload--active-items buf)))
    (when (seq-empty-p pending)
      (user-error "No pending prompts"))
    (if (and remove-index (numberp remove-index))
        (when-let* ((item (nth remove-index pending)))
          (when (y-or-n-p (format "Remove [%s] \"%s\"?"
                                  (agent-shell-queue-item-id item)
                                  (truncate-string-to-width
                                   (or (agent-shell-queue-item-args item) "") 50 nil nil "...")))
            (agent-shell-queue-remove (agent-shell-queue-item-id item))
            (message "Removed item %s (%d remaining)"
                     (agent-shell-queue-item-id item)
                     (1- (length pending)))))
      (let* ((choices (append
                       '(("Remove all" . remove-all))
                       (seq-map-indexed
                        (lambda (item idx)
                          (cons (format "%d [%s]: %s"
                                        (1+ idx)
                                        (agent-shell-queue-item-id item)
                                        (truncate-string-to-width
                                         (or (agent-shell-queue-item-args item) "") 60 nil nil "..."))
                                item))
                        pending)))
             (selection (cdr (assoc (completing-read "Remove: " choices nil t) choices))))
        (if (eq selection 'remove-all)
            (when (y-or-n-p (format "Remove all %d pending prompts?" (length pending)))
              (seq-do (lambda (it) (agent-shell-queue-remove (agent-shell-queue-item-id it)))
                      pending)
              (message "Removed all pending prompts"))
          (when (and selection (agent-shell-queue-item-p selection))
            (when (y-or-n-p (format "Remove [%s] \"%s\"?"
                                    (agent-shell-queue-item-id selection)
                                    (truncate-string-to-width
                                     (or (agent-shell-queue-item-args selection) "") 50 nil nil "...")))
              (agent-shell-queue-remove (agent-shell-queue-item-id selection))
              (message "Removed item %s" (agent-shell-queue-item-id selection)))))))))

(defun agent-shell-queue-overload-display ()
  "Display pending prompts for the current shell buffer from `agent-shell-queue'."
  (let* ((buf (agent-shell-queue-overload--target-shell-buffer))
         (pending (agent-shell-queue-overload--active-items buf)))
    (unless (seq-empty-p pending)
      (if (fboundp 'agent-shell--update-fragment)
          (agent-shell--update-fragment
           :state (with-current-buffer buf (agent-shell--state))
           :block-id (format "%s-pending-prompts"
                             (with-current-buffer buf
                               (map-elt (agent-shell--state) :request-count)))
           :body (format "Pending prompts: %d (agent-shell-queue)

%s

Resume:  M-x agent-shell-prompt-queue-resume
Remove:  M-x agent-shell-prompt-queue-remove
Queue:   M-x agent-shell-queue
"
                         (length pending)
                         (mapconcat
                          (lambda (idx-item)
                            (let* ((idx (car idx-item))
                                   (item (cdr idx-item))
                                   (id (agent-shell-queue-item-id item))
                                   (prompt (or (agent-shell-queue-item-args item) ""))
                                   (first-line (car (split-string prompt "\n" t))))
                              (format "  %d [%s]: \"%s\""
                                      (1+ idx)
                                      id
                                      (truncate-string-to-width (or first-line "") 70 nil nil "..."))))
                          (seq-map-indexed (lambda (it idx) (cons idx it)) pending)
                          "\n"))
           :create-new t)
        (message "Pending prompts (%d): %s"
                 (length pending)
                 (mapconcat #'agent-shell-queue-item-id pending ", "))))))

;;; Minibuffer escape to capture buffer

(defun agent-shell-queue-overload-escape-to-capture ()
  "Abort minibuffer prompt reading and open an ASQ capture buffer with contents."
  (interactive)
  (let ((text (minibuffer-contents))
        (shell-buf (or (bound-and-true-p agent-shell-queue-overload--current-shell-buffer)
                       (agent-shell-queue-overload--target-shell-buffer))))
    (setq agent-shell-queue-overload--escape-handoff (cons shell-buf text))
    (abort-recursive-edit)))

(defun agent-shell-queue-overload-setup-minibuffer (info)
  "Minibuffer setup hook for `agent-shell-prompt-queue'.
INFO is an alist that contains `:shell-buffer'."
  (let ((shell-buf (alist-get :shell-buffer info)))
    (setq-local agent-shell-queue-overload--current-shell-buffer shell-buf)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd agent-shell-queue-overload-escape-key)
                  #'agent-shell-queue-overload-escape-to-capture)
      (use-local-map map))))

(defun agent-shell-queue-overload-read (&rest args)
  "Read prompt for prompt queue with escape-to-capture support.
ARGS accepts `:initial'."
  (let ((initial (plist-get args :initial)))
    (if (eq agent-shell-queue-overload-entry-method 'capture-buffer)
        (let ((shell-buf (agent-shell-queue-overload--target-shell-buffer)))
          (agent-shell-queue--open-capture shell-buf nil initial)
          nil)
      (setq agent-shell-queue-overload--escape-handoff nil)
      (condition-case nil
          (let ((shell-buffer (current-buffer)))
            (minibuffer-with-setup-hook
                (lambda ()
                  (agent-shell-queue-overload-setup-minibuffer
                   `((:shell-buffer . ,shell-buffer)))
                  (when (boundp 'agent-shell-prompt-queue-setup-minibuffer-functions)
                    (run-hook-with-args 'agent-shell-prompt-queue-setup-minibuffer-functions
                                        `((:shell-buffer . ,shell-buffer))))
                  (when initial
                    (insert initial)))
              (read-string (if (fboundp 'agent-shell--state)
                               (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                                   "Enqueue prompt: ")
                             "Enqueue prompt: "))))
        (quit
         (if agent-shell-queue-overload--escape-handoff
             (let ((target (car agent-shell-queue-overload--escape-handoff))
                   (text (cdr agent-shell-queue-overload--escape-handoff)))
               (setq agent-shell-queue-overload--escape-handoff nil)
               (agent-shell-queue--open-capture target nil text)
               nil)
           (signal 'quit nil)))))))

(defun agent-shell-queue-overload-prompt-queue (prompt)
  "Command to queue or send PROMPT using `agent-shell-queue'.
When called interactively:
- If `agent-shell-queue-overload-entry-method' is `capture-buffer', opens
  an ASQ capture buffer.
- If `minibuffer', reads from minibuffer, with escape key
  available to escape mid-typing to a capture buffer."
  (interactive
   (let* ((shell-buf (agent-shell-queue-overload--target-shell-buffer)))
     (if (eq agent-shell-queue-overload-entry-method 'capture-buffer)
         (progn
           (agent-shell-queue--open-capture shell-buf)
           (list :capture-buffer))
       (setq agent-shell-queue-overload--escape-handoff nil)
       (let ((read-val
              (condition-case nil
                  (with-current-buffer shell-buf
                    (if (fboundp 'agent-shell--prompt-queue-read)
                        (agent-shell--prompt-queue-read)
                      (read-string "Enqueue prompt: ")))
                (quit
                 (if agent-shell-queue-overload--escape-handoff
                     (let ((target (car agent-shell-queue-overload--escape-handoff))
                           (text (cdr agent-shell-queue-overload--escape-handoff)))
                       (setq agent-shell-queue-overload--escape-handoff nil)
                       (agent-shell-queue--open-capture target nil text)
                       :escaped-to-capture)
                   (signal 'quit nil))))))
         (list read-val)))))
  (unless (memq prompt '(:capture-buffer :escaped-to-capture nil))
    (when (string-empty-p (string-trim prompt))
      (user-error "No prompt given"))
    (agent-shell-queue-overload-enqueue prompt)))

(cl-defun agent-shell-queue-overload--advice-enqueue (&key prompt)
  "Advice override for `agent-shell--prompt-queue-enqueue'."
  (agent-shell-queue-overload-enqueue prompt))

;;; Minor Mode

;;;###autoload
(define-minor-mode agent-shell-queue-overload-mode
  "Global minor mode routing inbuilt agent-shell queue to ASQ.
Provides disk persistence, queue introspection, pausing, and mid-composition
escape to multi-line capture buffers."
  :global t
  :group 'agent-shell-queue-overload
  (if agent-shell-queue-overload-mode
      ;; Enable overload
      (progn
        ;; 1. Set upstream hooks if available
        (when (boundp 'agent-shell-prompt-queue-enqueue-function)
          (setq agent-shell-prompt-queue-enqueue-function #'agent-shell-queue-overload-enqueue))
        (when (boundp 'agent-shell-prompt-queue-process-next-function)
          (setq agent-shell-prompt-queue-process-next-function #'agent-shell-queue-overload-process-next))
        (when (boundp 'agent-shell-prompt-queue-display-function)
          (setq agent-shell-prompt-queue-display-function #'agent-shell-queue-overload-display))
        (when (boundp 'agent-shell-prompt-queue-resume-function)
          (setq agent-shell-prompt-queue-resume-function #'agent-shell-queue-overload-resume))
        (when (boundp 'agent-shell-prompt-queue-remove-function)
          (setq agent-shell-prompt-queue-remove-function #'agent-shell-queue-overload-remove))
        (when (boundp 'agent-shell-prompt-queue-function)
          (setq agent-shell-prompt-queue-function #'agent-shell-queue-overload-prompt-queue))
        (when (boundp 'agent-shell-prompt-queue-read-function)
          (setq agent-shell-prompt-queue-read-function #'agent-shell-queue-overload-read))

        ;; 2. Advise functions for environments without upstream hook variables
        (when (fboundp 'agent-shell--prompt-queue-enqueue)
          (advice-add 'agent-shell--prompt-queue-enqueue :override #'agent-shell-queue-overload--advice-enqueue))
        (when (fboundp 'agent-shell-prompt-queue)
          (advice-add 'agent-shell-prompt-queue :override #'agent-shell-queue-overload-prompt-queue))
        (when (fboundp 'agent-shell-prompt-queue-resume)
          (advice-add 'agent-shell-prompt-queue-resume :override #'agent-shell-queue-overload-resume))
        (when (fboundp 'agent-shell-prompt-queue-remove)
          (advice-add 'agent-shell-prompt-queue-remove :override #'agent-shell-queue-overload-remove))
        (when (fboundp 'agent-shell--prompt-queue-display)
          (advice-add 'agent-shell--prompt-queue-display :override #'agent-shell-queue-overload-display))

        ;; 3. Obsolete commands compatibility
        (when (fboundp 'agent-shell-queue-request)
          (advice-add 'agent-shell-queue-request :override #'agent-shell-queue-overload-prompt-queue))
        (when (fboundp 'agent-shell-resume-pending-requests)
          (advice-add 'agent-shell-resume-pending-requests :override #'agent-shell-queue-overload-resume))
        (when (fboundp 'agent-shell-remove-pending-request)
          (advice-add 'agent-shell-remove-pending-request :override #'agent-shell-queue-overload-remove))

        ;; 4. Minibuffer setup hook for escape binding
        (add-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
                  #'agent-shell-queue-overload-setup-minibuffer))

    ;; Disable overload
    (progn
      ;; 1. Clear upstream hooks
      (when (boundp 'agent-shell-prompt-queue-enqueue-function)
        (setq agent-shell-prompt-queue-enqueue-function nil))
      (when (boundp 'agent-shell-prompt-queue-process-next-function)
        (setq agent-shell-prompt-queue-process-next-function nil))
      (when (boundp 'agent-shell-prompt-queue-display-function)
        (setq agent-shell-prompt-queue-display-function nil))
      (when (boundp 'agent-shell-prompt-queue-resume-function)
        (setq agent-shell-prompt-queue-resume-function nil))
      (when (boundp 'agent-shell-prompt-queue-remove-function)
        (setq agent-shell-prompt-queue-remove-function nil))
      (when (boundp 'agent-shell-prompt-queue-function)
        (setq agent-shell-prompt-queue-function nil))
      (when (boundp 'agent-shell-prompt-queue-read-function)
        (setq agent-shell-prompt-queue-read-function nil))

      ;; 2. Remove advices
      (when (fboundp 'agent-shell--prompt-queue-enqueue)
        (advice-remove 'agent-shell--prompt-queue-enqueue #'agent-shell-queue-overload--advice-enqueue))
      (when (fboundp 'agent-shell-prompt-queue)
        (advice-remove 'agent-shell-prompt-queue #'agent-shell-queue-overload-prompt-queue))
      (when (fboundp 'agent-shell-prompt-queue-resume)
        (advice-remove 'agent-shell-prompt-queue-resume #'agent-shell-queue-overload-resume))
      (when (fboundp 'agent-shell-prompt-queue-remove)
        (advice-remove 'agent-shell-prompt-queue-remove #'agent-shell-queue-overload-remove))
      (when (fboundp 'agent-shell--prompt-queue-display)
        (advice-remove 'agent-shell--prompt-queue-display #'agent-shell-queue-overload-display))

      (when (fboundp 'agent-shell-queue-request)
        (advice-remove 'agent-shell-queue-request #'agent-shell-queue-overload-prompt-queue))
      (when (fboundp 'agent-shell-resume-pending-requests)
        (advice-remove 'agent-shell-resume-pending-requests #'agent-shell-queue-overload-resume))
      (when (fboundp 'agent-shell-remove-pending-request)
        (advice-remove 'agent-shell-remove-pending-request #'agent-shell-queue-overload-remove))

      ;; 3. Remove minibuffer setup hook
      (remove-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
                   #'agent-shell-queue-overload-setup-minibuffer))))

(provide 'agent-shell-queue-overload)

;;; agent-shell-queue-overload.el ends here
