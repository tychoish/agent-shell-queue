;;; agent-shell-ask.el --- Human-in-the-loop question adapter for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell, hitl
;; Package-Requires: ((emacs "29.1") (hitl "0.1.0") (agent-shell "0.1") (annotated-completing-read "0.1"))

;;; Commentary:

;; Compatibility adapter providing the legacy `agent-shell-ask' API
;; backed by the universal `hitl' engine.  All question queuing, lifecycle
;; tracking, cursor iteration, and interactive prompters delegate to `hitl`.
;; Follow-up actions (:function, :enqueue, :send-shell) and shell resurrection
;; remain available for agent-shell integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hitl nil t)
(require 'annotated-completing-read nil t)

(declare-function agent-shell-queue-persistence-request-save "agent-shell-queue-persistence")
(declare-function agent-shell-queue-enqueue "agent-shell-queue")
(declare-function comint-send-input "comint")

(defgroup agent-shell-ask nil
  "Human-in-the-loop question queue for `agent-shell'."
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

;;; Aliased Store, Cursors & Hooks

(defvar agent-shell-ask-store nil
  "Legacy variable for backward compatibility; storage is managed by `hitl'.")

(defvar agent-shell-ask-cursors nil
  "Legacy variable for backward compatibility; cursors are managed by `hitl'.")

(defvaralias 'agent-shell-ask-on-question-created-functions 'hitl-on-question-created-functions
  "Hook functions called with (QUESTION) when a new question is created.")

(defvaralias 'agent-shell-ask-on-question-answered-functions 'hitl-on-question-answered-functions
  "Hook functions called with (QUESTION RESPONSE) when a question is answered.")

(defvaralias 'agent-shell-ask-on-question-cancelled-functions 'hitl-on-question-cancelled-functions
  "Hook functions called with (QUESTION REASON) when a question is cancelled.")

(defun agent-shell-ask--request-persistence-save (&rest _args)
  "Request queue persistence save if available."
  (when (fboundp 'agent-shell-queue-persistence-request-save)
    (agent-shell-queue-persistence-request-save)))

(add-hook 'agent-shell-ask-on-question-created-functions #'agent-shell-ask--request-persistence-save)
(add-hook 'agent-shell-ask-on-question-answered-functions #'agent-shell-ask--request-persistence-save)
(add-hook 'agent-shell-ask-on-question-cancelled-functions #'agent-shell-ask--request-persistence-save)

;;; Question Struct Accessors & Setters (Backed by hitl-question)

(defalias 'agent-shell-ask-question-p #'hitl-question-p)

(defsubst agent-shell-ask-question-id (q)
  (hitl-question-id q))
(gv-define-setter agent-shell-ask-question-id (v q)
  `(setf (hitl-question-id ,q) ,v))

(defsubst agent-shell-ask-question-prompt (q)
  (hitl-question-prompt q))
(gv-define-setter agent-shell-ask-question-prompt (v q)
  `(setf (hitl-question-prompt ,q) ,v))

(defsubst agent-shell-ask-question-kind (q)
  (let ((k (hitl-question-kind q)))
    (if (keywordp k)
        (intern (string-remove-prefix ":" (symbol-name k)))
      k)))
(gv-define-setter agent-shell-ask-question-kind (v q)
  `(setf (hitl-question-kind ,q)
         (if (keywordp ,v)
             ,v
           (intern (format ":%s" (string-remove-prefix ":" (symbol-name ,v)))))))

(defsubst agent-shell-ask-question-options (q)
  (hitl-question-options q))
(gv-define-setter agent-shell-ask-question-options (v q)
  `(setf (hitl-question-options ,q) ,v))

(defsubst agent-shell-ask-question-default-value (q)
  (hitl-question-default-value q))
(gv-define-setter agent-shell-ask-question-default-value (v q)
  `(setf (hitl-question-default-value ,q) ,v))

(defsubst agent-shell-ask-question-status (q)
  (hitl-question-status q))
(gv-define-setter agent-shell-ask-question-status (v q)
  `(setf (hitl-question-status ,q) ,v))

(defsubst agent-shell-ask-question-response (q)
  (hitl-question-response q))
(gv-define-setter agent-shell-ask-question-response (v q)
  `(setf (hitl-question-response ,q) ,v))

(defsubst agent-shell-ask-question-target-shell (q)
  (hitl-question-target q))
(gv-define-setter agent-shell-ask-question-target-shell (v q)
  `(setf (hitl-question-target ,q) ,v))

(defsubst agent-shell-ask-question-directory (q)
  (hitl-question-directory q))
(gv-define-setter agent-shell-ask-question-directory (v q)
  `(setf (hitl-question-directory ,q) ,v))

(defsubst agent-shell-ask-question-created (q)
  (hitl-question-created-at q))
(gv-define-setter agent-shell-ask-question-created (v q)
  `(setf (hitl-question-created-at ,q) ,v))

(defsubst agent-shell-ask-question-answered-at (q)
  (hitl-question-answered-at q))
(gv-define-setter agent-shell-ask-question-answered-at (v q)
  `(setf (hitl-question-answered-at ,q) ,v))

(defsubst agent-shell-ask-question-timeout (q)
  (hitl-question-timeout q))
(gv-define-setter agent-shell-ask-question-timeout (v q)
  `(setf (hitl-question-timeout ,q) ,v))

(defsubst agent-shell-ask-question-metadata (q)
  (hitl-question-metadata q))
(gv-define-setter agent-shell-ask-question-metadata (v q)
  `(setf (hitl-question-metadata ,q) ,v))

(defsubst agent-shell-ask-question-followup-action (q)
  (plist-get (hitl-question-metadata q) :followup-action))
(gv-define-setter agent-shell-ask-question-followup-action (v q)
  `(setf (hitl-question-metadata ,q)
         (plist-put (copy-sequence (hitl-question-metadata ,q))
                    :followup-action ,v)))

(cl-defun agent-shell-ask-question--make
    (&key id prompt kind options default-value status response target-shell
          directory created answered-at timeout followup-action metadata)
  "Construct a question backed by `hitl-question'."
  (let ((meta (if followup-action
                  (plist-put (copy-sequence metadata) :followup-action followup-action)
                metadata)))
    (hitl-question--make
     :id (or id (hitl-generate-id))
     :prompt prompt
     :kind (if (keywordp kind) kind (intern (format ":%s" (string-remove-prefix ":" (symbol-name kind)))))
     :options options
     :default-value default-value
     :status (or status 'pending)
     :response response
     :target target-shell
     :directory directory
     :created-at (or created (float-time))
     :answered-at answered-at
     :timeout timeout
     :metadata meta)))

;;; Question Lifecycle API

(defalias 'agent-shell-ask-generate-id #'hitl-generate-id)

(cl-defun agent-shell-ask-create
    (&key prompt (kind 'single-choice) options default-value target-shell
          directory timeout followup-action metadata id)
  "Create and register a new question delegating to `hitl-ask'."
  (let* ((shell-name (when target-shell
                       (if (bufferp target-shell)
                           (buffer-name target-shell)
                         target-shell)))
         (dir (or directory
                  (when target-shell
                    (ignore-errors
                      (with-current-buffer target-shell default-directory)))
                  default-directory))
         (meta (if followup-action
                   (plist-put (copy-sequence metadata) :followup-action followup-action)
                 metadata))
         (q (hitl-ask
             :id id
             :prompt prompt
             :kind kind
             :options options
             :default-value default-value
             :target shell-name
             :directory dir
             :timeout timeout
             :metadata meta)))
    q))

(defalias 'agent-shell-ask-get #'hitl-get)

(defun agent-shell-ask-list-pending (&optional target-shell)
  "List pending questions, optionally filtered by TARGET-SHELL."
  (hitl-list-pending target-shell))

(defalias 'agent-shell-ask-list-all #'hitl-list-all)

;;; Cursor Iteration

(defun agent-shell-ask-cursor-next (&optional cursor-id target-shell)
  "Return next pending question for CURSOR-ID delegating to `hitl-cursor-next'."
  (hitl-cursor-next cursor-id target-shell))

(defalias 'agent-shell-ask-cursor-reset #'hitl-cursor-reset)

;;; Shell Resurrection & Follow-up Execution

(defun agent-shell-queue--resurrect-shell (shell-name &optional default-dir)
  "Return a live buffer for SHELL-NAME, resurrecting it via `agent-shell' if dead.
DEFAULT-DIR, when provided, sets `default-directory' in the spawned shell."
  (let ((buf (when shell-name (get-buffer shell-name))))
    (if (and buf (buffer-live-p buf))
        buf
      (when (fboundp 'agent-shell-new-shell)
        (let ((default-directory (or default-dir default-directory)))
          (agent-shell-new-shell))))))

(defun agent-shell-ask-execute-followup (q response)
  "Execute post-answer follow-up action for question Q with RESPONSE."
  (when-let* ((action (agent-shell-ask-question-followup-action q))
              (type (plist-get action :type)))
    (pcase type
      (:function
       (when-let* ((fn (plist-get action :function)))
         (funcall fn response q)))
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
             (comint-send-input))))))))

(defun agent-shell-ask-answer (id response)
  "Mark question ID as answered with RESPONSE payload and trigger follow-up action."
  (let ((q (hitl-answer id response)))
    (agent-shell-ask-execute-followup q response)
    q))

(defalias 'agent-shell-ask-cancel #'hitl-cancel)

;;; Minibuffer & Interactive UI Widgets

(defalias 'agent-shell-ask-prompt-question #'hitl-prompt-question)
(defalias 'agent-shell-ask-prompt #'hitl-prompt)

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
  (hitl-clear-store)
  (dolist (item data)
    (let ((q (agent-shell-ask-question-from-plist item)))
      (hitl-ask :id (agent-shell-ask-question-id q)
                :prompt (agent-shell-ask-question-prompt q)
                :kind (agent-shell-ask-question-kind q)
                :options (agent-shell-ask-question-options q)
                :default-value (agent-shell-ask-question-default-value q)
                :target (agent-shell-ask-question-target-shell q)
                :directory (agent-shell-ask-question-directory q)
                :timeout (agent-shell-ask-question-timeout q)
                :metadata (agent-shell-ask-question-metadata q)))))

(provide 'agent-shell-ask)

;;; agent-shell-ask.el ends here
