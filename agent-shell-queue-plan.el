;;; agent-shell-queue-plan.el --- Queue hitl-plan executions in agent-shell-queue -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, convenience, agent, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file bridges `hitl-plan' (resumable bulk action plans) and
;; `agent-shell-queue' by registering the `hitl-plan' queue item type.
;; Enqueueing a plan validates that every operation is `:queueable', saves
;; the plan, asks the plan's domain to lock its batch
;; (`hitl-plan-domain-lock-batch', called with BUFFER and PLAN-ID), and
;; queues an item that later runs the plan through `hitl-plan-execute'.  When
;; the last queued plan for a buffer finishes, the domain's `unlock-batch'
;; (called with BUFFER) runs.
;;
;; A plan may be stored outside `hitl-plan-directory', because each plan
;; carries its own directory.  The queued item records that directory so
;; dispatch can find the plan again, and the interactive plan picker also
;; searches each domain's `:directory' store.
;;
;; A buffer is a valid target when a registered domain accepts the buffer.
;; The checks run in this order: a domain's `:accept-buffer' predicate
;; returns non-nil for the buffer, the buffer-local
;; `agent-shell-queue-plan-domain' names a registered domain, or a function
;; in `agent-shell-queue-plan-buffer-accept-functions' returns the name of a
;; registered domain.
;;
;; A plan created over MCP (`hitl-plan-origin-mcp-p') is untrusted, and the
;; queue runs it only when `hitl-plan-approved-p' is true.  That holds when
;; the user has set `hitl-plan-agent-may-apply', or when the user has queued
;; the plan through the interactive command (`agent-shell-queue-plan--input',
;; or `agent-shell-queue-plan-enqueue' called with APPROVE), which records
;; `:approved t' in the plan metadata.  Dispatch consumes that marker, so one
;; approval buys one run, and dispatch requires the plan to be draft or
;; paused.  The item type's reenqueue guard applies the same rules to
;; `agent-shell-queue-reenqueue' of a `hitl-plan' item.  MCP code never sets
;; the marker.
;;
;; This module soft-depends on `hitl-plan'.  Without the package, loading is
;; a no-op (`require' with NOERROR returns nil) and enqueueing signals a
;; clear error.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-queue-core)
(require 'hitl-plan nil t)

(declare-function hitl-plan-get "hitl-plan")
(declare-function hitl-plan-status "hitl-plan")
(declare-function hitl-plan-directory "hitl-plan")
(declare-function hitl-plan-domain-name "hitl-plan")
(declare-function hitl-plan-save "hitl-plan")
(declare-function hitl-plan-execute "hitl-plan")
(declare-function hitl-plan-id "hitl-plan")
(declare-function hitl-plan-domain "hitl-plan")
(declare-function hitl-plan-get-domain "hitl-plan")
(declare-function hitl-plan-registered-domains "hitl-plan")
(declare-function hitl-plan-check-unattended "hitl-plan")
(declare-function hitl-plan-list "hitl-plan")
(declare-function hitl-plan-domain-store "hitl-plan")
(declare-function hitl-plan-domain-lock-batch "hitl-plan")
(declare-function hitl-plan-domain-accept-buffer "hitl-plan")
(declare-function hitl-plan-domain-unlock-batch "hitl-plan")
(declare-function hitl-plan-ui-read-from "hitl-plan-ui")
(declare-function hitl-plan-approved-p "hitl-plan")
(declare-function hitl-plan-origin-mcp-p "hitl-plan")
(declare-function hitl-plan-metadata "hitl-plan")
(declare-function hitl-plan-ui-clean-string "hitl-plan-ui")

(defvar agent-shell-queue--store)
(defvar read-eval)

(defcustom agent-shell-queue-plan-max-items 5
  "Maximum number of active or running `hitl-plan' items per buffer.
nil, or a non-positive number, means unlimited."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'agent-shell-queue)

(defvar-local agent-shell-queue-plan-domain nil
  "Name of the `hitl-plan' domain this buffer belongs to, or nil.")

(defvar agent-shell-queue-plan-buffer-accept-functions nil
  "Abnormal hook; each function receives a buffer.
A function accepts the buffer by returning the name (symbol or string) of a
registered `hitl-plan' domain, which then owns it (its batch is locked and
its plans are offered).  Any other result, including a bare non-nil value,
does not accept.  The first accepting function wins.  Consulted after the
domains' :accept-buffer predicates and `agent-shell-queue-plan-domain'.")

(defun agent-shell-queue-plan--require ()
  "Signal a `user-error' unless `hitl-plan' is available."
  (unless (featurep 'hitl-plan)
    (unless (require 'hitl-plan nil t)
      (user-error "The `hitl' package (hitl-plan) is required by agent-shell-queue-plan"))))

;;; Buffer Predicate

(defun agent-shell-queue-plan--domain-for-buffer (buf)
  "Return the registered `hitl-plan-domain' that accepts BUF, or nil.
Domains' `:accept-buffer' predicates are consulted first (in registration
order), then the buffer-local domain name, then the accept-functions hook,
whose functions must return a registered domain name to accept."
  (when (and (featurep 'hitl-plan) (buffer-live-p buf))
    (let ((name (buffer-local-value 'agent-shell-queue-plan-domain buf)))
      (or (seq-find (lambda (d)
                      (when-let* ((pred (hitl-plan-domain-accept-buffer d)))
                        (funcall pred buf)))
                    (hitl-plan-registered-domains))
          (and name (hitl-plan-get-domain name))
          (run-hook-wrapped
           'agent-shell-queue-plan-buffer-accept-functions
           (lambda (fn)
             (let ((result (funcall fn buf)))
               (and result (or (symbolp result) (stringp result))
                    (hitl-plan-get-domain result)))))))))

(defun agent-shell-queue-plan-buffer-p (buf)
  "Return non-nil if any registered `hitl-plan' domain accepts BUF."
  (and (agent-shell-queue-plan--domain-for-buffer buf) t))

;;; Queue Inspection

(defun agent-shell-queue-plan--pending-count (buf-name)
  "Return the number of active or running `hitl-plan' items for BUF-NAME."
  (if (not (and (boundp 'agent-shell-queue--store) agent-shell-queue--store))
      0
    (seq-count
     (lambda (item)
       (and (eq (agent-shell-queue-item-kind item) 'hitl-plan)
            (memq (agent-shell-queue-item-status item) '(active running))))
     (cdr (assoc buf-name (agent-shell-queue-store-items agent-shell-queue--store))))))

;;; Dispatch

(defun agent-shell-queue-plan--queued-p (plan-id)
  "Return non-nil if an active or running `hitl-plan' item for PLAN-ID exists."
  (and (boundp 'agent-shell-queue--store) agent-shell-queue--store
       (seq-some
        (lambda (entry)
          (seq-some
           (lambda (item)
             (and (eq (agent-shell-queue-item-kind item) 'hitl-plan)
                  (memq (agent-shell-queue-item-status item) '(active running))
                  (equal plan-id
                         (plist-get (agent-shell-queue-plan--parse-args
                                     (agent-shell-queue-item-args item))
                                    :plan-id))))
           (cdr entry)))
        (agent-shell-queue-store-items agent-shell-queue--store))))

(defun agent-shell-queue-plan--find (id)
  "Return the plan with ID, searching the default and every domain store."
  (or (hitl-plan-get id)
      (seq-some (lambda (dir) (hitl-plan-get id dir))
                (delq nil (mapcar #'hitl-plan-domain-store
                                  (hitl-plan-registered-domains))))))

(defun agent-shell-queue-plan--parse-args (raw)
  "Parse RAW item args (plist or string) into a plist with :plan-id.
A string is a printed plist (read with `read-eval' off, so it cannot run
code) or else a bare plan ID.  Malformed or non-plist text is taken as the
ID itself."
  (cond
   ((and (consp raw) (plistp raw)) raw)
   ((stringp raw)
    (let* ((s (string-trim raw))
           (form (and (string-prefix-p "(" s)
                      (condition-case nil
                          (let ((read-eval nil)) (read s))
                        (error nil)))))
      (if (and (consp form) (plistp form))
          form
        (list :plan-id s))))))

(defun agent-shell-queue-plan--unlock (buf plan)
  "Unlock BUF's batch via the domain of PLAN (or BUF's accepting domain)."
  (when-let* ((domain (or (and plan (hitl-plan-get-domain (hitl-plan-domain plan)))
                          (agent-shell-queue-plan--domain-for-buffer buf)))
              (unlock (hitl-plan-domain-unlock-batch domain)))
    (funcall unlock buf)))

(defun agent-shell-queue-plan--check-runnable (plan)
  "Signal a `user-error' unless PLAN may be run from the queue now.
PLAN must be draft or paused, approved (`hitl-plan-approved-p') and
unattended-safe."
  (unless (memq (hitl-plan-status plan) '(draft paused))
    (user-error "Plan %s is %s; only draft or paused plans can run"
                (hitl-plan-id plan) (hitl-plan-status plan)))
  (unless (hitl-plan-approved-p plan)
    (user-error "Plan %s was created over MCP and has not been approved; queue it interactively"
                (hitl-plan-id plan)))
  (hitl-plan-check-unattended plan))

(defun agent-shell-queue-plan--consume-approval (plan)
  "Remove the one-shot `:approved' marker from PLAN's metadata."
  (let ((meta (copy-sequence (hitl-plan-metadata plan))))
    (when (plist-member meta :approved)
      (setf (hitl-plan-metadata plan)
            (cl-loop for (k v) on meta by #'cddr
                     unless (eq k :approved) append (list k v))))))

(defun agent-shell-queue-plan--reenqueue-guard (item _buf)
  "Refuse to re-enqueue `hitl-plan' ITEM unless its plan could be queued now.
This is the item type's reenqueue guard: the plan must exist, be draft or
paused, not already be queued, and pass
`agent-shell-queue-plan--check-runnable'."
  (agent-shell-queue-plan--require)
  (let* ((spec (agent-shell-queue-plan--parse-args (agent-shell-queue-item-args item)))
         (plan (and (plist-get spec :plan-id)
                    (hitl-plan-get (plist-get spec :plan-id) (plist-get spec :directory)))))
    (unless plan
      (user-error "Plan %S no longer exists; cannot re-enqueue" (plist-get spec :plan-id)))
    (when (agent-shell-queue-plan--queued-p (hitl-plan-id plan))
      (user-error "Plan %s is already queued" (hitl-plan-id plan)))
    (agent-shell-queue-plan--check-runnable plan)))

(defun agent-shell-queue-plan-dispatch (item buf-name)
  "Dispatch queued `hitl-plan' ITEM against buffer BUF-NAME.
Executes the plan with `hitl-plan-execute', completes ITEM, and unlocks the
batch once no other `hitl-plan' items remain queued for the buffer."
  (let* ((spec (agent-shell-queue-plan--parse-args
                (agent-shell-queue-item-args item)))
         (plan-id (plist-get spec :plan-id))
         (buf (get-buffer buf-name))
         (plan (and (featurep 'hitl-plan) plan-id
                    (hitl-plan-get plan-id (plist-get spec :directory)))))
    (unwind-protect
        (progn
          (agent-shell-queue-plan--require)
          (unless plan
            (error "Plan %S not found for buffer %s" plan-id buf-name))
          (agent-shell-queue-plan--check-runnable plan)
          (agent-shell-queue-plan--consume-approval plan)
          (hitl-plan-execute plan))
      (agent-shell-queue-complete-item item buf-name)
      (when (and (buffer-live-p buf)
                 (= (agent-shell-queue-plan--pending-count buf-name) 0))
        (agent-shell-queue-plan--unlock buf plan)))))

;;; Enqueue

(defun agent-shell-queue-plan--set-delay-after (item delay)
  "Set post-completion DELAY in seconds on queue ITEM."
  (setf (agent-shell-queue-item-delay-after item) delay))

;;;###autoload
(defun agent-shell-queue-plan-enqueue (buf plan &optional delay-after approve)
  "Enqueue `hitl-plan' PLAN (or plan ID) on buffer BUF.
Validates that every operation is `:queueable', saves PLAN, locks BUF's
batch through the domain's :lock-batch, and queues an item running PLAN
via `agent-shell-queue-plan-dispatch'.  Rejects plans
that are not draft or paused, or already queued.  Enforces
`agent-shell-queue-plan-max-items' per buffer.  DELAY-AFTER, when positive,
is the pause in seconds after the item completes.  APPROVE, non-nil only
when the user chose to queue PLAN (the interactive picker), records the
`:approved' marker that lets a plan created over MCP run unattended; MCP
code must never pass it.  Returns PLAN."
  (agent-shell-queue-plan--require)
  (let* ((buf (if (stringp buf) (get-buffer buf) buf))
         (plan (if (stringp plan) (or (agent-shell-queue-plan--find plan)
                                      (user-error "Unknown plan %s" plan))
                 plan)))
    (unless (buffer-live-p buf)
      (user-error "Target buffer must be live"))
    (unless (agent-shell-queue-plan-buffer-p buf)
      (user-error "No hitl-plan domain accepts buffer %s" (buffer-name buf)))
    (unless (memq (hitl-plan-status plan) '(draft paused))
      (user-error "Plan %s is %s; only draft or paused plans can be queued"
                  (hitl-plan-id plan) (hitl-plan-status plan)))
    (when (agent-shell-queue-plan--queued-p (hitl-plan-id plan))
      (user-error "Plan %s is already queued" (hitl-plan-id plan)))
    (let* ((buf-name (buffer-name buf))
           (pending (agent-shell-queue-plan--pending-count buf-name)))
      (when (and (numberp agent-shell-queue-plan-max-items)
                 (> agent-shell-queue-plan-max-items 0)
                 (>= pending agent-shell-queue-plan-max-items))
        (user-error "Buffer %s already has %d queued hitl-plan items (max %d)"
                    buf-name pending agent-shell-queue-plan-max-items))
      (hitl-plan-check-unattended plan)
      (when (and approve (hitl-plan-origin-mcp-p plan))
        (setf (hitl-plan-metadata plan)
              (plist-put (copy-sequence (hitl-plan-metadata plan)) :approved t)))
      (hitl-plan-save plan)
      (let* ((domain (or (hitl-plan-get-domain (hitl-plan-domain plan))
                         (agent-shell-queue-plan--domain-for-buffer buf)))
             (lock (and domain (hitl-plan-domain-lock-batch domain)))
             (item nil))
        (when lock (funcall lock buf (hitl-plan-id plan)))
        (unwind-protect
            (setq item (agent-shell-queue--enqueue-args
                        (prin1-to-string (append (list :plan-id (hitl-plan-id plan))
                                                 (when-let* ((dir (hitl-plan-directory plan)))
                                                   (list :directory dir))))
                        'hitl-plan buf))
          (unless item
            (agent-shell-queue-plan--unlock buf plan)))
        (when (and (numberp delay-after) (> delay-after 0))
          (agent-shell-queue-plan--set-delay-after item delay-after))
        plan))))

(defun agent-shell-queue-plan--candidates (buf)
  "Return the draft or paused plans that can be queued on BUF.
Looks in `hitl-plan-directory', the loaded plans and the `:directory'
store of each registered domain.  When a domain accepts BUF only
that domain's plans are offered."
  (let* ((domain (agent-shell-queue-plan--domain-for-buffer buf))
         (dirs (cons nil (delq nil (mapcar #'hitl-plan-domain-store
                                           (if domain
                                               (list domain)
                                             (hitl-plan-registered-domains)))))))
    (seq-filter
     (lambda (p) (or (null domain)
                     (eq (hitl-plan-domain p) (hitl-plan-domain-name domain))))
     (seq-uniq (seq-mapcat (lambda (dir) (hitl-plan-list '(draft paused) dir)) dirs)
               #'eq))))

(defun agent-shell-queue-plan--input (buf)
  "Interactive input handler for BUF: prompt for a saved plan and enqueue it.
The picker is `hitl-plan-ui-read-from', annotating each plan with its
status, progress, age and description."
  (agent-shell-queue-plan--require)
  (require 'hitl-plan-ui)
  (let ((plans (agent-shell-queue-plan--candidates buf)))
    (unless plans (user-error "No draft or paused hitl plans"))
    (let* ((id (hitl-plan-ui-read-from plans "Plan: "))
           (plan (seq-find (lambda (p) (equal (hitl-plan-id p) id)) plans)))
      (when (and (hitl-plan-origin-mcp-p plan)
                 (not (yes-or-no-p
                       (format "Plan %s was created by an agent over MCP.  Queue it to run unattended? "
                               (hitl-plan-ui-clean-string id 60)))))
        (user-error "Plan %s not queued" id))
      ;; This is the user's own decision, so it approves an mcp-origin plan.
      (agent-shell-queue-plan-enqueue buf plan nil t))))

;;; Registration

(defun agent-shell-queue-plan-register ()
  "Register the `hitl-plan' item type with `agent-shell-queue'."
  (agent-shell-queue-register-item-type
   :kind 'hitl-plan
   :label "hitl-plan"
   :buffer-pred #'agent-shell-queue-plan-buffer-p
   :dispatch-fn #'agent-shell-queue-plan-dispatch
   :reenqueue-guard #'agent-shell-queue-plan--reenqueue-guard
   :input-spec (list :kind 'special :fn #'agent-shell-queue-plan--input)))

(agent-shell-queue-plan-register)

(provide 'agent-shell-queue-plan)
;;; agent-shell-queue-plan.el ends here
