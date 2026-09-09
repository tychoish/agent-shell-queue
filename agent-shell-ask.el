;;; agent-shell-ask.el --- Human-in-the-loop question queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell, hitl
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (annotated-completing-read "0.1"))

;;; Commentary:

;; Implements a Human-in-the-Loop (HITL) prompt and question queue subsystem
;; integrated into agent-shell-queue. Supports single-choice, multi-choice
;; (via acr-multi), free-text, boolean, file, and form questions with
;; declarative follow-up actions, cursor-driven queue iteration, MCP tool
;; exposure, and shell resurrection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'annotated-completing-read nil t)

(defgroup agent-shell-ask nil
  "Human-in-the-loop question queue for agent-shell."
  :group 'agent-shell-queue)

(defface agent-shell-ask-pending-face
  '((t :foreground "orange" :weight bold))
  "Face for pending human questions.")

(defface agent-shell-ask-answered-face
  '((t :foreground "forestgreen"))
  "Face for answered human questions.")

(defface agent-shell-ask-cancelled-face
  '((t :foreground "gray50" :slant italic))
  "Face for cancelled or expired human questions.")

;;; Data Structure

(cl-defstruct (agent-shell-ask-question
               (:constructor agent-shell-ask-question--make)
               (:copier nil))
  id              ; String UUID/hash identifier
  prompt          ; String prompt text shown to user
  kind            ; Symbol: 'single-choice, 'multi-choice, 'text, 'boolean, 'file, 'form
  options         ; List of strings or alist of (key . label) or (:key "K" :label "L" :value "V")
  default-value   ; Default choice or string
  status          ; Symbol: 'pending, 'answered, 'rejected, 'expired, 'cancelled
  response        ; Human response payload (string, list of strings, boolean, etc.)
  target-shell    ; Target buffer name or directory bucket string
  directory       ; Target default-directory string
  created         ; Float timestamp
  answered-at     ; Float timestamp
  timeout         ; Optional timeout in seconds
  followup-action ; Plist describing action on answer
  metadata)       ; Extra key-value metadata plist

;;; Memory Store & Cursors

(defvar agent-shell-ask-store (make-hash-table :test #'equal)
  "Hash table mapping question ID strings to `agent-shell-ask-question' structs.")

(defvar agent-shell-ask-cursors (make-hash-table :test #'equal)
  "Hash table mapping cursor ID strings to last-seen question ID strings.")

(defvar agent-shell-ask-on-question-created-functions nil
  "Hook functions called with (QUESTION) when a new question is created.")

(defvar agent-shell-ask-on-question-answered-functions nil
  "Hook functions called with (QUESTION RESPONSE) when a question is answered.")

;;; Question Lifecycle API

(defun agent-shell-ask-generate-id ()
  "Generate a unique question ID string."
  (format "ask-%s-%x"
          (format-time-string "%s")
          (random #xffff)))

(cl-defun agent-shell-ask-create
    (&key prompt (kind 'single-choice) options default-value target-shell
          directory timeout followup-action metadata id)
  "Create and register a new `agent-shell-ask-question'.
PROMPT is the text shown to the user.
KIND is one of 'single-choice, 'multi-choice, 'text, 'boolean, 'file, 'form.
OPTIONS is a list of choices or alist.
RETURNS the created question struct."
  (let* ((qid (or id (agent-shell-ask-generate-id)))
         (dir (or directory (when target-shell (ignore-errors (with-current-buffer target-shell default-directory))) default-directory))
         (q (agent-shell-ask-question--make
             :id qid
             :prompt prompt
             :kind kind
             :options options
             :default-value default-value
             :status 'pending
             :response nil
             :target-shell (when target-shell (if (bufferp target-shell) (buffer-name target-shell) target-shell))
             :directory dir
             :created (float-time)
             :answered-at nil
             :timeout timeout
             :followup-action followup-action
             :metadata metadata)))
    (puthash qid q agent-shell-ask-store)
    (run-hook-with-args 'agent-shell-ask-on-question-created-functions q)
    (when (fboundp 'agent-shell-queue-persistence-request-save)
      (funcall 'agent-shell-queue-persistence-request-save))
    q))

(defun agent-shell-ask-get (id)
  "Retrieve question by ID."
  (gethash id agent-shell-ask-store))

(defun agent-shell-ask-list-pending (&optional target-shell)
  "List all pending `agent-shell-ask-question' structs, optionally filtered by TARGET-SHELL."
  (let ((items nil))
    (maphash
     (lambda (_id q)
       (when (and (eq (agent-shell-ask-question-status q) 'pending)
                  (or (null target-shell)
                      (equal (agent-shell-ask-question-target-shell q) target-shell)))
         (push q items)))
     agent-shell-ask-store)
    (sort items (lambda (a b) (< (agent-shell-ask-question-created a)
                                (agent-shell-ask-question-created b))))))

(defun agent-shell-ask-list-all ()
  "List all `agent-shell-ask-question' structs in chronological order."
  (let ((items nil))
    (maphash (lambda (_id q) (push q items)) agent-shell-ask-store)
    (sort items (lambda (a b) (< (agent-shell-ask-question-created a)
                                (agent-shell-ask-question-created b))))))

;;; Cursor-driven Queue Iteration

(defun agent-shell-ask-cursor-next (&optional cursor-id target-shell)
  "Return the next pending `agent-shell-ask-question' for CURSOR-ID.
If CURSOR-ID is nil, defaults to \"default\".
Maintains last-seen position and advances the cursor to the returned item."
  (let* ((cid (or cursor-id "default"))
         (last-id (gethash cid agent-shell-ask-cursors))
         (pending (agent-shell-ask-list-pending target-shell))
         (next-q nil))
    (if (null last-id)
        (setq next-q (car pending))
      (let ((after-last nil))
        (dolist (q pending)
          (if after-last
              (unless next-q (setq next-q q))
            (when (equal (agent-shell-ask-question-id q) last-id)
              (setq after-last t))))
        (unless next-q
          (setq next-q (car pending)))))
    (when next-q
      (puthash cid (agent-shell-ask-question-id next-q) agent-shell-ask-cursors))
    next-q))

(defun agent-shell-ask-cursor-reset (&optional cursor-id)
  "Reset cursor CURSOR-ID."
  (remhash (or cursor-id "default") agent-shell-ask-cursors))

;;; Answering & Follow-up Dispatch

(defun agent-shell-ask-answer (id response)
  "Mark question ID as answered with RESPONSE payload and trigger follow-up action."
  (let ((q (agent-shell-ask-get id)))
    (unless q
      (user-error "Question %s not found" id))
    (unless (eq (agent-shell-ask-question-status q) 'pending)
      (user-error "Question %s is not pending (status: %s)" id (agent-shell-ask-question-status q)))
    (setf (agent-shell-ask-question-status q) 'answered)
    (setf (agent-shell-ask-question-response q) response)
    (setf (agent-shell-ask-question-answered-at q) (float-time))
    (run-hook-with-args 'agent-shell-ask-on-question-answered-functions q response)
    (agent-shell-ask-execute-followup q response)
    (when (fboundp 'agent-shell-queue-persistence-request-save)
      (funcall 'agent-shell-queue-persistence-request-save))
    q))

(defun agent-shell-ask-cancel (id &optional reason)
  "Mark question ID as cancelled."
  (let ((q (agent-shell-ask-get id)))
    (when q
      (setf (agent-shell-ask-question-status q) 'cancelled)
      (when reason
        (setf (agent-shell-ask-question-response q) (format "Cancelled: %s" reason)))
      (when (fboundp 'agent-shell-queue-persistence-request-save)
        (funcall 'agent-shell-queue-persistence-request-save))
      q)))

(defun agent-shell-ask-execute-followup (q response)
  "Execute post-answer follow-up action for question Q with RESPONSE."
  (let ((action (agent-shell-ask-question-followup-action q)))
    (when action
      (let ((type (plist-get action :type)))
        (pcase type
          (:function
           (let ((fn (plist-get action :function))
                 (args (plist-get action :args)))
             (when (fboundp fn)
               (apply fn response args))))
          (:enqueue
           (let ((prompt-fmt (plist-get action :prompt))
                 (bucket (plist-get action :bucket))
                 (target-shell (agent-shell-ask-question-target-shell q)))
             (when (fboundp 'agent-shell-queue-enqueue)
               (let ((prompt-str (if prompt-fmt (format prompt-fmt response) (format "%s" response))))
                 (funcall 'agent-shell-queue-enqueue prompt-str :bucket bucket :shell target-shell)))))
          (:send-shell
           (let* ((shell-name (or (plist-get action :shell-name)
                                  (agent-shell-ask-question-target-shell q)))
                  (text-fmt (or (plist-get action :text) "%s\n"))
                  (text (format text-fmt response))
                  (dir (agent-shell-ask-question-directory q))
                  (buf (when shell-name (get-buffer shell-name))))
             (unless (and buf (buffer-live-p buf))
               (when (fboundp 'agent-shell-queue--resurrect-shell)
                 (setq buf (funcall 'agent-shell-queue--resurrect-shell shell-name dir))))
             (when (and buf (buffer-live-p buf))
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert text)
                 (when (fboundp 'comint-send-input)
                   (comint-send-input))))))
          (:sprite
           (let ((task-spec (plist-get action :task-spec)))
             (when (fboundp 'sprite-direct)
               (funcall 'sprite-direct task-spec)))))))))

;;; Minibuffer & Interactive UI Widgets

(defun agent-shell-ask-prompt-question (q)
  "Prompt the user interactively for question Q and return response."
  (let* ((kind (agent-shell-ask-question-kind q))
         (prompt (format "[HITL Question] %s " (agent-shell-ask-question-prompt q)))
         (options (agent-shell-ask-question-options q))
         (def (agent-shell-ask-question-default-value q)))
    (pcase kind
      ('boolean
       (y-or-n-p prompt))
      ('single-choice
       (if (and (fboundp 'annotated-completing-read) options)
           (annotated-completing-read options :prompt prompt :default def)
         (completing-read prompt options nil t nil nil def)))
      ('multi-choice
       (if (and (fboundp 'annotated-completing-read) options)
           (annotated-completing-read options :prompt prompt :multiple t :default def)
         (completing-read-multiple prompt options nil t nil nil def)))
      ('file
       (read-file-name prompt (or def default-directory)))
      ('text
       (read-string prompt def))
      (_
       (read-string prompt def)))))

(defun agent-shell-ask-prompt (&optional question-id)
  "Interactively prompt user to answer a pending question.
If QUESTION-ID is provided, answer that question; otherwise select from pending."
  (interactive)
  (let ((q (if question-id
               (agent-shell-ask-get question-id)
             (let ((pending (agent-shell-ask-list-pending)))
               (unless pending
                 (user-error "No pending HITL questions"))
               (if (= (length pending) 1)
                   (car pending)
                 (let* ((table (mapcar (lambda (item)
                                         (cons (format "[%s] %s"
                                                       (agent-shell-ask-question-id item)
                                                       (agent-shell-ask-question-prompt item))
                                               item))
                                       pending))
                        (choice (completing-read "Select question to answer: " table nil t)))
                   (cdr (assoc choice table))))))))
    (when q
      (let ((resp (agent-shell-ask-prompt-question q)))
        (agent-shell-ask-answer (agent-shell-ask-question-id q) resp)
        (message "Question %s answered." (agent-shell-ask-question-id q))))))

;;; Tabulated List Buffer UI (*agent-shell-ask*)

(defvar agent-shell-ask-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-ask-ui-answer-at-point)
    (define-key map (kbd "c") #'agent-shell-ask-ui-cancel-at-point)
    (define-key map (kbd "g") #'agent-shell-ask-ui-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-shell-ask-mode'.")

(define-derived-mode agent-shell-ask-mode tabulated-list-mode "ASQ-Ask"
  "Major mode for browsing and answering HITL agent questions."
  (setq tabulated-list-format
        [("ID" 14 t)
         ("Status" 10 t)
         ("Kind" 14 t)
         ("Target Shell" 20 t)
         ("Prompt" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun agent-shell-ask-ui-refresh ()
  "Refresh the `*agent-shell-ask*' tabulated list buffer."
  (interactive)
  (let ((buf (get-buffer-create "*agent-shell-ask*")))
    (with-current-buffer buf
      (agent-shell-ask-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (q)
               (let* ((qid (agent-shell-ask-question-id q))
                      (status (agent-shell-ask-question-status q))
                      (kind (symbol-name (agent-shell-ask-question-kind q)))
                      (target (or (agent-shell-ask-question-target-shell q) "-"))
                      (prompt (agent-shell-ask-question-prompt q))
                      (status-str (propertize (symbol-name status)
                                              'face (pcase status
                                                      ('pending 'agent-shell-ask-pending-face)
                                                      ('answered 'agent-shell-ask-answered-face)
                                                      (_ 'agent-shell-ask-cancelled-face)))))
                 (list qid (vector qid status-str kind target prompt))))
             (agent-shell-ask-list-all)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun agent-shell-ask-ui-answer-at-point ()
  "Answer question at point in `*agent-shell-ask*' buffer."
  (interactive)
  (let ((qid (tabulated-list-get-id)))
    (when qid
      (agent-shell-ask-prompt qid)
      (agent-shell-ask-ui-refresh))))

(defun agent-shell-ask-ui-cancel-at-point ()
  "Cancel question at point in `*agent-shell-ask*' buffer."
  (interactive)
  (let ((qid (tabulated-list-get-id)))
    (when qid
      (agent-shell-ask-cancel qid "Cancelled from UI")
      (agent-shell-ask-ui-refresh))))

;;; Serialization Helpers

(defun agent-shell-ask-question-to-plist (q)
  "Serialize question struct Q to a plist."
  (list :id (agent-shell-ask-question-id q)
        :prompt (agent-shell-ask-question-prompt q)
        :kind (agent-shell-ask-question-kind q)
        :options (agent-shell-ask-question-options q)
        :default-value (agent-shell-ask-question-default-value q)
        :status (agent-shell-ask-question-status q)
        :response (agent-shell-ask-question-response q)
        :target-shell (agent-shell-ask-question-target-shell q)
        :directory (agent-shell-ask-question-directory q)
        :created (agent-shell-ask-question-created q)
        :answered-at (agent-shell-ask-question-answered-at q)
        :timeout (agent-shell-ask-question-timeout q)
        :followup-action (agent-shell-ask-question-followup-action q)
        :metadata (agent-shell-ask-question-metadata q)))

(defun agent-shell-ask-question-from-plist (plist)
  "Deserialize PLIST to an `agent-shell-ask-question' struct."
  (agent-shell-ask-question--make
   :id (plist-get plist :id)
   :prompt (plist-get plist :prompt)
   :kind (plist-get plist :kind)
   :options (plist-get plist :options)
   :default-value (plist-get plist :default-value)
   :status (plist-get plist :status)
   :response (plist-get plist :response)
   :target-shell (plist-get plist :target-shell)
   :directory (plist-get plist :directory)
   :created (plist-get plist :created)
   :answered-at (plist-get plist :answered-at)
   :timeout (plist-get plist :timeout)
   :followup-action (plist-get plist :followup-action)
   :metadata (plist-get plist :metadata)))

(defun agent-shell-ask-serialize-store ()
  "Serialize entire question store into a list of plists."
  (mapcar #'agent-shell-ask-question-to-plist (agent-shell-ask-list-all)))

(defun agent-shell-ask-deserialize-store (data)
  "Populate question store from list of question plists DATA."
  (clrhash agent-shell-ask-store)
  (dolist (item data)
    (let ((q (agent-shell-ask-question-from-plist item)))
      (puthash (agent-shell-ask-question-id q) q agent-shell-ask-store))))

(provide 'agent-shell-ask)

;;; agent-shell-ask.el ends here
