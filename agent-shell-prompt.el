;;; agent-shell-prompt.el --- Reusable prompt library for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (agent-shell-queue "0.1"))

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

;; `agent-shell-prompt' registers reusable, parameterized prompt
;; workflows: an optional deterministic pre-operation gathers context
;; (CI logs, diffs, PR comments) before an agent turn; the prompt
;; template renders that context into text; an optional post-operation
;; reacts to turn completion (verification, cleanup, chaining).
;;
;; Workflows dispatch either directly to a live or new `agent-shell'
;; session, or as a queued item in `agent-shell-queue'.  See
;; `agent-shell-prompt-def' to register a workflow and
;; `agent-shell-prompt-dispatch' to run one.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'agent-shell)
(require 'agent-shell-queue)

;; Data model

(cl-defstruct (agent-shell-prompt-spec
               (:constructor agent-shell-prompt-spec--make)
               (:copier nil))
  "A registered reusable prompt workflow.
ID: symbol — unique registry key.
DOC: string — one-line description shown in menus.
CATEGORY: string — grouping label shown in menus.
ARGS: list of (NAME :prompt STRING :type TYPE :optional BOOL) specs.
PRE-OP: function called as (PRE-OP ctx) or (PRE-OP ctx callback) — see
  `agent-shell-prompt-exec-pre'.
TEMPLATE: string with {{key}} placeholders resolved against ctx.
SUBMIT: non-nil to submit the rendered prompt immediately on insertion.
TARGET: one of `:session-reuse', `:session-new', `:queue', `:ask'.
POST-OP: function called as (POST-OP shell-buffer ctx response-text)
  on turn completion; see `agent-shell-prompt-exec-post'."
  id doc category args pre-op template submit target post-op)

(defvar agent-shell-prompt-registry (make-hash-table :test #'eq)
  "Hash table of symbol id to `agent-shell-prompt-spec'.
Populate via `agent-shell-prompt-def'.")

(defun agent-shell-prompt-get (id)
  "Return the `agent-shell-prompt-spec' registered under ID, or nil."
  (gethash id agent-shell-prompt-registry))

(defun agent-shell-prompt-list ()
  "Return all registered `agent-shell-prompt-spec' values."
  (map-values agent-shell-prompt-registry))

(cl-defun agent-shell-prompt-register (&key id doc category args pre-op template submit target post-op)
  "Register a prompt spec built from ID, DOC, CATEGORY, ARGS, PRE-OP.
TEMPLATE, SUBMIT, TARGET, and POST-OP.  Re-registering an existing ID
replaces the entry."
  (unless id
    (error "Agent-shell-prompt: `:id' is required"))
  (unless template
    (error "Agent-shell-prompt %s: `:template' is required" id))
  (puthash id
           (agent-shell-prompt-spec--make
            :id id :doc doc :category (or category "General") :args args
            :pre-op pre-op :template template :submit submit
            :target (or target :ask) :post-op post-op)
           agent-shell-prompt-registry))

(defmacro agent-shell-prompt-def (id &rest keys)
  "Define and register a prompt workflow named ID.
KEYS is a plist accepting the same keys as `agent-shell-prompt-register'
\(:doc :category :args :pre-op :template :submit :target :post-op).
:args is data (an arg-spec list), not code, and is quoted automatically."
  (declare (indent 1))
  (let ((keys (if (plist-member keys :args)
                  (plist-put (copy-sequence keys) :args (list 'quote (plist-get keys :args)))
                keys)))
    `(agent-shell-prompt-register :id ',id ,@keys)))

;; Argument collection

(defun agent-shell-prompt--arg-key (name)
  "Normalize an arg-spec NAME (bare symbol or keyword) to a plist keyword key."
  (if (keywordp name) name (intern (format ":%s" name))))

(defun agent-shell-prompt--read-arg (arg-spec)
  "Interactively read one value for ARG-SPEC and return (KEY . VALUE).
ARG-SPEC is (NAME :prompt STRING :type TYPE :optional BOOL); NAME may be
a bare symbol or a keyword — either way KEY is the keyword form used as
the :args plist key."
  (let* ((name (car arg-spec))
         (key (agent-shell-prompt--arg-key name))
         (opts (cdr arg-spec))
         (prompt (or (plist-get opts :prompt) (format "%s: " name)))
         (type (or (plist-get opts :type) 'string))
         (optional (plist-get opts :optional))
         (raw (pcase type
                ('integer (read-number prompt))
                ('symbol (intern (completing-read prompt nil)))
                (_ (read-string prompt)))))
    (when (and (not optional) (equal raw ""))
      (user-error "agent-shell-prompt: %s is required" name))
    (cons key raw)))

(defun agent-shell-prompt--collect-args (spec provided)
  "Return a complete args plist for SPEC, reading any keys missing from PROVIDED.
PROVIDED is a plist of already-known argument values, keyed by keyword."
  (seq-reduce
   (lambda (acc arg-spec)
     (let ((key (agent-shell-prompt--arg-key (car arg-spec))))
       (if (plist-member acc key)
           acc
         (let ((pair (agent-shell-prompt--read-arg arg-spec)))
           (plist-put acc (car pair) (cdr pair))))))
   (agent-shell-prompt-spec-args spec)
   (copy-sequence provided)))

;; Template rendering

(defun agent-shell-prompt--stringify (value)
  "Return a display string for VALUE, a template substitution result."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun agent-shell-prompt-render (template ctx)
  "Render TEMPLATE, substituting {{key}} placeholders from CTX.
CTX is a plist; {{args.KEY}} looks inside the :args sub-plist, any other
{{key}} looks up the top-level :key entry."
  (replace-regexp-in-string
   "{{\\([-a-zA-Z0-9.]+\\)}}"
   (lambda (matched)
     ;; `split-string' below uses regexp matching internally, which would
     ;; otherwise clobber the match-data `replace-regexp-in-string' needs
     ;; once this function returns.
     (save-match-data
       (let* ((key (substring matched 2 -2))
              (segments (split-string key "\\."))
              (value (if (and (> (length segments) 1) (string= (car segments) "args"))
                        (plist-get (plist-get ctx :args) (intern (concat ":" (cadr segments))))
                      (plist-get ctx (intern (concat ":" key))))))
         (agent-shell-prompt--stringify value))))
   template t t))

;; Pre/post operation execution

(defun agent-shell-prompt-exec-pre (spec ctx callback)
  "Run SPEC's pre-op against CTX, then call CALLBACK with the updated ctx.
When SPEC has no pre-op, CALLBACK is invoked with CTX unchanged.
A pre-op of arity 1 is treated as synchronous and must return the updated
ctx; a pre-op of arity 2 is treated as asynchronous and must itself call
its callback argument with the updated ctx."
  (let ((pre-op (agent-shell-prompt-spec-pre-op spec)))
    (if (null pre-op)
        (funcall callback ctx)
      (pcase (car (func-arity pre-op))
        (1 (funcall callback (funcall pre-op ctx)))
        (_ (funcall pre-op ctx callback))))))

(defun agent-shell-prompt-exec-post (spec shell-buffer ctx response-text)
  "Run SPEC's post-op with SHELL-BUFFER, CTX, and RESPONSE-TEXT.
Returns the post-op's control-flag result, or `:done' when SPEC has no
post-op.  See `agent-shell-prompt-spec' for the set of recognized flags."
  (if-let* ((post-op (agent-shell-prompt-spec-post-op spec)))
      (funcall post-op shell-buffer ctx response-text)
    :done))

(defun agent-shell-prompt--apply-post-result (result shell-buffer)
  "Act on a post-op RESULT for SHELL-BUFFER.
Handles `:drop-context', `:restart', `:close', and `(:chain ID ARGS)'.
`:done' and any unrecognized value are no-ops."
  (pcase result
    (:drop-context
     (agent-shell-queue-enqueue-clear shell-buffer))
    (:restart
     (when (buffer-live-p shell-buffer)
       (with-current-buffer shell-buffer
         (agent-shell-interrupt))))
    (:close
     (when (buffer-live-p shell-buffer)
       (kill-buffer shell-buffer)))
    (`(:chain ,next-id ,next-args)
     (agent-shell-prompt-dispatch next-id
                                  :args next-args
                                  :target :session-reuse
                                  :submit t))
    (_ nil)))

;; Dispatch routing

(defun agent-shell-prompt--resolve-target (target)
  "Resolve TARGET keyword `:ask' via `completing-read' into a concrete target."
  (if (eq target :ask)
      (intern (completing-read "Dispatch to: "
                                '(":session-reuse" ":session-new" ":queue")
                                nil t))
    target))

(defun agent-shell-prompt--project-buffers (dir)
  "Return live `agent-shell' buffers whose `default-directory' is under DIR.
Mirrors `agent-shell-menu-project-buffers' rather than calling it directly:
agent-shell-prompt-menu.el (which does depend on agent-shell-menu) wires a
transient entry into agent-shell-menu-dispatch, so this file requiring
agent-shell-menu in turn would be circular."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (string-prefix-p (agent-shell-queue--canonicalize-dir dir)
                                    (agent-shell-queue--canonicalize-dir default-directory))))
              (agent-shell-buffers)))

(defun agent-shell-prompt--session-buffer (target)
  "Return a live `agent-shell' buffer for TARGET, creating one if needed.
`:session-reuse' searches buffers scoped to `default-directory' first;
`:session-new' always creates a fresh session."
  (pcase target
    (:session-new (agent-shell-new-shell))
    (_ (or (car (agent-shell-prompt--project-buffers default-directory))
           (agent-shell-new-shell)))))

(defun agent-shell-prompt--subscribe-post (spec shell-buffer ctx)
  "Subscribe to SHELL-BUFFER's turn-complete event to run SPEC's post-op.
CTX is the execution context passed through to the post-op; its
:response-start position (set by `agent-shell-prompt--dispatch-rendered')
scopes the visible-response-text walk."
  (let (token)
    (setq token
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'turn-complete
           :on-event
           (lambda (_event)
             (agent-shell-unsubscribe :subscription token)
             (let* ((response (agent-shell-prompt--last-response-text
                               shell-buffer (plist-get ctx :response-start)))
                    (result (agent-shell-prompt-exec-post spec shell-buffer ctx response)))
               (agent-shell-prompt--apply-post-result result shell-buffer)))))))

(defun agent-shell-prompt--last-response-text (shell-buffer start-pos)
  "Return the visible agent response text in SHELL-BUFFER from START-POS.
Reuses `agent-shell-queue--collect-visible-response-text' — the shell-maker
boundary/invisible-block walk already implemented there — rather than
reimplementing it."
  (when (and (buffer-live-p shell-buffer) start-pos)
    (agent-shell-queue--collect-visible-response-text shell-buffer start-pos)))

(defun agent-shell-prompt--dispatch-rendered (spec ctx)
  "Deliver SPEC's rendered prompt in CTX to its resolved dispatch target."
  (let* ((target (agent-shell-prompt--resolve-target (plist-get ctx :target)))
         (rendered (plist-get ctx :rendered-prompt))
         (submit (plist-get ctx :submit)))
    (pcase target
      (:queue
       (agent-shell-queue--enqueue-args
        (prin1-to-string (list :prompt-id (agent-shell-prompt-spec-id spec)
                               :rendered rendered))
        'prompt-library nil))
      (_
       (let* ((shell-buffer (agent-shell-prompt--session-buffer target))
              (insertion (agent-shell-insert :text rendered :submit submit
                                             :no-focus nil :shell-buffer shell-buffer)))
         (when (agent-shell-prompt-spec-post-op spec)
           (let ((ctx (plist-put (plist-put (copy-sequence ctx) :shell-buffer shell-buffer)
                                 :response-start (map-elt insertion :end))))
             (agent-shell-prompt--subscribe-post spec shell-buffer ctx))))))))

;;;###autoload
(cl-defun agent-shell-prompt-dispatch (id &key args target (submit nil submit-supplied-p))
  "Run the prompt workflow registered as ID.
ARGS is a plist of pre-known argument values; any argument the spec
declares but ARGS omits is collected interactively.  TARGET and SUBMIT
override the spec's defaults when supplied."
  (let* ((spec (or (agent-shell-prompt-get id)
                   (error "Agent-shell-prompt: unknown prompt %s" id)))
         (full-args (agent-shell-prompt--collect-args spec args))
         (ctx (list :prompt-id id
                    :args full-args
                    :directory default-directory
                    :target (or target (agent-shell-prompt-spec-target spec))
                    :submit (if submit-supplied-p
                                submit
                              (agent-shell-prompt-spec-submit spec)))))
    (agent-shell-prompt-exec-pre
     spec ctx
     (lambda (updated-ctx)
       (let* ((rendered (agent-shell-prompt-render
                         (agent-shell-prompt-spec-template spec) updated-ctx))
              (final-ctx (plist-put (copy-sequence updated-ctx) :rendered-prompt rendered)))
         (agent-shell-prompt--dispatch-rendered spec final-ctx))))))

;; Queue integration

(defun agent-shell-prompt--dispatch-queue-item (item buf-name)
  "Dispatch a queued prompt-library ITEM to BUF-NAME.
ITEM's args is a printed plist with :prompt-id and :rendered."
  (let* ((payload (read (agent-shell-queue-item-args item)))
         (rendered (plist-get payload :rendered)))
    (agent-shell-insert :text rendered :submit t :no-focus t
                        :shell-buffer (get-buffer buf-name))))

(agent-shell-queue-register-item-type
 :kind 'prompt-library
 :label "prompt-library"
 :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
 :dispatch-fn #'agent-shell-prompt--dispatch-queue-item
 :input-spec '(:kind none))

(provide 'agent-shell-prompt)

;;; agent-shell-prompt.el ends here
