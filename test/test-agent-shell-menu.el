;;; test-agent-shell-menu.el --- ERT tests for agent-shell-menu -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-menu/")

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'agent-shell-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--block-category

(ert-deftest agent-shell-menu/block-category-thinking ()
  (should (equal "thinking"
                 (agent-shell-menu--block-category "msg_abc123_agent_thought_chunk"))))

(ert-deftest agent-shell-menu/block-category-agent-message ()
  (should (equal "agent message"
                 (agent-shell-menu--block-category "turn_1_agent_message_chunk"))))

(ert-deftest agent-shell-menu/block-category-user-message ()
  (should (equal "user message"
                 (agent-shell-menu--block-category "turn_1_user_message_chunk"))))

(ert-deftest agent-shell-menu/block-category-plan ()
  (should (equal "plan"
                 (agent-shell-menu--block-category "session-plan"))))

(ert-deftest agent-shell-menu/block-category-plan-any-prefix ()
  (should (equal "plan"
                 (agent-shell-menu--block-category "some-other-thing-plan"))))

(ert-deftest agent-shell-menu/block-category-session-info ()
  (should (equal "session info"
                 (agent-shell-menu--block-category "bootstrapping-intro"))))

(ert-deftest agent-shell-menu/block-category-tool-call-default ()
  (should (equal "tool call"
                 (agent-shell-menu--block-category "tool_use_xyz"))))

(ert-deftest agent-shell-menu/block-category-tool-call-unknown-id ()
  (should (equal "tool call"
                 (agent-shell-menu--block-category "some-random-id-123"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--permission-buttons

(ert-deftest agent-shell-menu/permission-buttons-empty-buffer ()
  (with-temp-buffer
    (should (null (agent-shell-menu--permission-buttons)))))

(ert-deftest agent-shell-menu/permission-buttons-no-permission-property ()
  (with-temp-buffer
    (insert "some text without button property")
    (should (null (agent-shell-menu--permission-buttons)))))

(ert-deftest agent-shell-menu/permission-buttons-finds-single-button ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell-menu--permission-buttons)))
      (should (= 1 (length buttons)))
      (should (equal "Allow" (caar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-trims-brackets-and-whitespace ()
  (with-temp-buffer
    (insert "[  Deny  ]")
    (put-text-property 1 11 'button 'permission)
    (let ((buttons (agent-shell-menu--permission-buttons)))
      (should (equal "Deny" (caar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-returns-position ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell-menu--permission-buttons)))
      (should (= 1 (cdar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-finds-multiple-buttons ()
  (with-temp-buffer
    (insert "[ Allow ]\n[ Deny ]")
    (put-text-property 1 10 'button 'permission)
    (put-text-property 11 19 'button 'permission)
    (let ((buttons (agent-shell-menu--permission-buttons)))
      (should (= 2 (length buttons)))
      (should (equal "Allow" (caar buttons)))
      (should (equal "Deny" (caadr buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-ignores-non-permission-button ()
  (with-temp-buffer
    (insert "[ Other ]")
    (put-text-property 1 10 'button 'other-value)
    (should (null (agent-shell-menu--permission-buttons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--permission-button-action

(ert-deftest agent-shell-menu/permission-button-action-invokes-command ()
  "The lambda returned by permission-button-action calls the button's RET command."
  (let* ((invoked nil)
         (cmd (lambda () (interactive) (setq invoked t))))
    (with-temp-buffer
      (insert "x")
      (put-text-property 1 2 'keymap
                         (let ((m (make-sparse-keymap)))
                           (define-key m (kbd "RET") cmd)
                           m))
      (let ((action (agent-shell-menu--permission-button-action 1)))
        (call-interactively action)))
    (should invoked)))

(ert-deftest agent-shell-menu/permission-button-action-ignores-non-command ()
  "Regression: when lookup-key returns a non-commandp value (e.g. \"\"), the
lambda must silently do nothing instead of signalling
Wrong type argument: commandp, \"\"."
  (with-temp-buffer
    (insert "x")
    (put-text-property 1 2 'keymap
                       (let ((m (make-sparse-keymap)))
                         (define-key m (kbd "RET") "")
                         m))
    (let ((action (agent-shell-menu--permission-button-action 1)))
      (should-not (condition-case err
                      (progn (call-interactively action) nil)
                    (error err))))))

(ert-deftest agent-shell-menu/resolve-permission-uses-buttons-for-lookup ()
  "Regression: agent-shell-menu-resolve-permission must bind buttons before using it
in assoc — previously buttons was a free variable causing pos to always be nil."
  (let (invoked-pos)
    (with-temp-buffer
      (insert (make-string 50 ?x))
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'agent-shell--permission-pending-p) (lambda () t))
                ((symbol-function 'agent-shell-menu--permission-buttons)
                 (lambda () (list (cons "Allow" 1))))
                ((symbol-function 'annotated-completing-read)
                 (lambda (_table &rest _) "Allow"))
                ((symbol-function 'agent-shell-menu--permission-action-at)
                 (lambda (pos) (setq invoked-pos pos) #'ignore))
                ((symbol-function 'call-interactively) #'ignore))
        (agent-shell-menu-resolve-permission)))
    (should (equal 1 invoked-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu-select-action

(ert-deftest agent-shell-menu/select-action-calls-command-not-docstring ()
  "Regression: selecting a permission option must invoke the command, not the
doc-string.  The old pipeline converted (label . cmd) to (label . doc-string)
before the assoc lookup, so `call-interactively' received \"\" and signalled
Wrong type argument: commandp, \"\"."
  (let* ((invoked nil)
	 (action (lambda () (interactive) (setq invoked t)))
	 (captured-table nil))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell--permission-pending-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell-menu--permission-buttons)
	       (lambda () (list (cons "Allow" 1))))
	      ((symbol-function 'agent-shell-menu--permission-button-action)
	       (lambda (_pos) action))
	      ((symbol-function 'annotated-completing-read)
	       (lambda (table &rest _)
		 (setq captured-table table)
		 "permission: Allow")))
      (agent-shell-menu-select-action))
    (should invoked)
    (should (equal "" (cdr (assoc "permission: Allow" captured-table))))))

(ert-deftest agent-shell-menu/select-action-permission-entries-before-actions ()
  "Permission entries appear before regular action entries in the display table."
  (let (captured-table)
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell--permission-pending-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell-menu--permission-buttons)
	       (lambda () (list (cons "Allow" 1))))
	      ((symbol-function 'agent-shell-menu--permission-button-action)
	       (lambda (_pos) (lambda () (interactive))))
	      ((symbol-function 'annotated-completing-read)
	       (lambda (table &rest _)
		 (setq captured-table table)
		 (caar table))))
      (agent-shell-menu-select-action))
    (should (string-prefix-p "permission:" (caar captured-table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--pick-buffer

(ert-deftest agent-shell-menu/pick-buffer-passes-alist-not-buffer-list ()
  "Regression: passing (agent-shell-buffers) raw to annotated-completing-read
produced 'Each alist entry must be a cons cell; got: #<buffer ...>'."
  (let* ((mock-buf (generate-new-buffer "*mock-agent-pick*"))
         (captured nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list mock-buf)))
                  ((symbol-function 'agent-shell-menu--buffer-annotation) (lambda (_) "ann"))
                  ((symbol-function 'annotated-completing-read)
                   (lambda (table &rest _)
                     (setq captured table)
                     (buffer-name mock-buf))))
          (agent-shell-menu--pick-buffer "test: ")
          (should (listp captured))
          (should (= 1 (length captured)))
          (should (consp (car captured)))
          (should (equal (buffer-name mock-buf) (caar captured)))
          (should (equal "ann" (cdar captured))))
      (kill-buffer mock-buf))))

(ert-deftest agent-shell-menu/pick-buffer-errors-when-no-buffers ()
  (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
    (should-error (agent-shell-menu--pick-buffer "test: ") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu-project-buffers

(ert-deftest agent-shell-menu/same-project-buffers-empty-when-no-agent-buffers ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
      (should (null (agent-shell-menu-project-buffers))))))

(ert-deftest agent-shell-menu/same-project-buffers-excludes-current-buffer ()
  (with-temp-buffer
    (let ((cur (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list cur))))
        (should (null (agent-shell-menu-project-buffers)))))))

(ert-deftest agent-shell-menu/same-project-buffers-includes-same-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/test-proj/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/test-proj/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (memq other (agent-shell-menu-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-menu/same-project-buffers-excludes-different-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/proj-a/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/proj-b/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (null (agent-shell-menu-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-menu/same-project-buffers-returns-only-matching ()
  (let ((match (generate-new-buffer "*mock-match*"))
        (other (generate-new-buffer "*mock-other*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/my-proj/")
          (with-current-buffer match
            (setq-local default-directory "/tmp/my-proj/"))
          (with-current-buffer other
            (setq-local default-directory "/tmp/different/"))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                     (lambda () (list match other))))
            (let ((result (agent-shell-menu-project-buffers)))
              (should (memq match result))
              (should-not (memq other result)))))
      (kill-buffer match)
      (kill-buffer other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--in-session-p

(ert-deftest agent-shell-menu/session-permission-action-runs-in-shell-buffer ()
  "The action lambda activates the button in the shell buffer regardless of
which buffer is current when the action is invoked."
  (let* ((shell (generate-new-buffer " *mock-shell-3*"))
         (invoked-in nil))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (insert "x")
            (put-text-property 1 2
                               'keymap
                               (let ((m (make-sparse-keymap)))
                                 (define-key m (kbd "RET")
                                   (lambda () (interactive)
                                     (setq invoked-in (current-buffer))))
                                 m)))
          (let ((action (agent-shell-menu--session-permission-button-action shell 1)))
            (with-temp-buffer
              (call-interactively action)))
          (should (eq shell invoked-in)))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/session-permission-pending-p-uses-shell-buffer ()
  "The predicate checks the shell buffer, not the current buffer."
  (let ((shell (generate-new-buffer " *mock-shell-4*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell--permission-pending-p)
                   (lambda (&rest _) t)))
          (should (agent-shell-menu--session-permission-pending-p)))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/in-session-p-nil-when-no-session ()
  "Returns nil when agent-shell-menu--session-shell-buffer returns nil."
  (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer) (lambda () nil)))
    (should-not (agent-shell-menu--in-session-p))))

(ert-deftest agent-shell-menu/in-session-p-non-nil-when-session ()
  "Returns non-nil when agent-shell-menu--session-shell-buffer returns a buffer."
  (let ((shell (generate-new-buffer " *mock-in-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                   (lambda () shell)))
          (should (agent-shell-menu--in-session-p)))
      (kill-buffer shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--goto-last-interaction

(ert-deftest agent-shell-menu/goto-last-interaction-is-command ()
  "Regression: agent-shell-goto-last-interaction is not interactive; the wrapper
must be a command so it passes the commandp check in select-action and can be
used as a transient suffix."
  (should (commandp #'agent-shell-menu--goto-last-interaction)))

(ert-deftest agent-shell-menu/goto-last-interaction-calls-underlying ()
  "The wrapper delegates to agent-shell-goto-last-interaction."
  (let (called)
    (cl-letf (((symbol-function 'agent-shell-goto-last-interaction)
               (lambda () (setq called t))))
      (call-interactively #'agent-shell-menu--goto-last-interaction))
    (should called)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--permission-suffixes

(defun agent-shell-test/suffix-plist (suffix)
  "Return properties of transient SUFFIX as a plain plist.
`transient-parse-suffix' returns (transient-suffix :key K :description D ...)
so this strips the leading type tag via `cdr'."
  (cdr suffix))

(ert-deftest agent-shell-menu/permission-suffixes-empty-when-no-pending ()
  "No suffixes generated when no permission is pending."
  (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer) (lambda () nil)))
    (should (null (agent-shell-menu--permission-suffixes nil)))))

(ert-deftest agent-shell-menu/permission-suffixes-one-per-button ()
  "One transient suffix is produced per pending permission button."
  (let ((shell (generate-new-buffer " *mock-perm-shell*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell-menu--permission-buttons)
                   (lambda () (list (cons "Allow" 10) (cons "Deny" 20))))
                  ((symbol-function 'agent-shell-menu--session-permission-button-action)
                   (lambda (_shell _pos) #'ignore)))
          (let ((suffixes (agent-shell-menu--permission-suffixes nil)))
            (should (= 2 (length suffixes)))
            (should (equal "1" (plist-get (agent-shell-test/suffix-plist (nth 0 suffixes)) :key)))
            (should (equal "2" (plist-get (agent-shell-test/suffix-plist (nth 1 suffixes)) :key)))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/permission-suffixes-labels-include-button-text ()
  "Each suffix description includes the permission button label."
  (let ((shell (generate-new-buffer " *mock-perm-shell-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell-menu--permission-buttons)
                   (lambda () (list (cons "Allow for session" 10))))
                  ((symbol-function 'agent-shell-menu--session-permission-button-action)
                   (lambda (_shell _pos) #'ignore)))
          (let* ((suffix (car (agent-shell-menu--permission-suffixes nil)))
                 (desc (plist-get (agent-shell-test/suffix-plist suffix) :description)))
            (should (string-match-p "Allow for session" desc))))
      (kill-buffer shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--action-entry-command

(ert-deftest agent-shell-menu/action-entry-command-plain-symbol ()
  "Returns the cdr directly when the entry value is a plain command symbol."
  (should (eq 'my-cmd
              (agent-shell-menu--action-entry-command '("label" . my-cmd)))))

(ert-deftest agent-shell-menu/action-entry-command-cons-entry ()
  "Returns car of cdr cons when the entry value is (CMD . PREDICATE)."
  (should (eq 'my-cmd
              (agent-shell-menu--action-entry-command '("label" . (my-cmd . my-pred))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--action-entry-visible-p

(ert-deftest agent-shell-menu/action-entry-visible-p-plain-always-visible ()
  "Plain symbol entries are always visible."
  (should (agent-shell-menu--action-entry-visible-p '("label" . my-cmd))))

(ert-deftest agent-shell-menu/action-entry-visible-p-pred-true ()
  "Cons entries are visible when the predicate returns non-nil."
  (cl-letf (((symbol-function 'my-test-pred) (lambda () t)))
    (should (agent-shell-menu--action-entry-visible-p '("label" . (my-cmd . my-test-pred))))))

(ert-deftest agent-shell-menu/action-entry-visible-p-pred-nil ()
  "Cons entries are hidden when the predicate returns nil."
  (cl-letf (((symbol-function 'my-test-pred) (lambda () nil)))
    (should-not (agent-shell-menu--action-entry-visible-p '("label" . (my-cmd . my-test-pred))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu-select-action predicate filtering

(ert-deftest agent-shell-menu/select-action-excludes-hidden-entries ()
  "Entries whose predicate returns nil are absent from the action menu."
  (let (captured-table
        (visible-cmd (lambda () (interactive)))
        (hidden-cmd (lambda () (interactive))))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'agent-shell--permission-pending-p) (lambda () nil))
              ((symbol-function 'agent-shell-menu--visible-pred) (lambda () t))
              ((symbol-function 'agent-shell-menu--hidden-pred) (lambda () nil))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _)
                 (setq captured-table table)
                 (caar table)))
              (agent-shell-menu-action-alist
               (list (cons "visible-entry" (cons visible-cmd 'agent-shell-menu--visible-pred))
                     (cons "hidden-entry" (cons hidden-cmd 'agent-shell-menu--hidden-pred)))))
      (agent-shell-menu-select-action))
    (should (assoc "visible-entry" captured-table))
    (should-not (assoc "hidden-entry" captured-table))))

(ert-deftest agent-shell-menu/select-action-includes-plain-symbol-entries ()
  "Entries without a predicate (plain COMMAND symbol) are always included."
  (let (captured-table
        (plain-cmd (lambda () (interactive))))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'agent-shell--permission-pending-p) (lambda () nil))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _)
                 (setq captured-table table)
                 (caar table)))
              (agent-shell-menu-action-alist
               (list (cons "plain-entry" plain-cmd))))
      (agent-shell-menu-select-action))
    (should (assoc "plain-entry" captured-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu--in-shell-p

(ert-deftest agent-shell-menu/in-shell-p-true-in-agent-shell-mode ()
  "Returns non-nil when derived-mode-p reports agent-shell-mode."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (mode) (eq mode 'agent-shell-mode))))
    (should (agent-shell-menu--in-shell-p))))

(ert-deftest agent-shell-menu/in-shell-p-nil-in-other-mode ()
  "Returns nil when not in agent-shell-mode."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
    (should-not (agent-shell-menu--in-shell-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu-switch-project-session (regression)

(ert-deftest agent-shell-menu/switch-project-session-passes-alist ()
  "Regression: annotated-completing-read must receive an alist of (name . annotation),
not a raw buffer list or a single annotation string."
  (let* ((other (generate-new-buffer "*mock-proj-session*"))
         captured-table)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-menu-project-buffers)
                   (lambda () (list other)))
                  ((symbol-function 'agent-shell-menu--buffer-annotation) (lambda (_) "ann"))
                  ((symbol-function 'annotated-completing-read)
                   (lambda (table &rest _)
                     (setq captured-table table)
                     (buffer-name other)))
                  ((symbol-function 'get-buffer) #'identity)
                  ((symbol-function 'switch-to-buffer) #'ignore))
          (agent-shell-menu-switch-project-session)
          (should (listp captured-table))
          (should (consp (car captured-table)))
          (should (equal (buffer-name other) (caar captured-table)))
          (should (equal "ann" (cdar captured-table))))
      (kill-buffer other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-menu-select-session-mode (target/value migration)

(ert-deftest agent-shell-menu/select-session-mode-table-carries-id-as-target ()
  "The table passed to `annotated-completing-read' carries each mode's :id
as a triple-form target, so the selected mode-id needs no further lookup."
  (let ((modes '((:id "code" :name "Code" :description "write code")
                 (:id "plan" :name "Plan" :description "make a plan")))
        captured-table)
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'agent-shell--state) (lambda () 'fake-state))
              ((symbol-function 'map-nested-elt) (lambda (&rest _) "session-id"))
              ((symbol-function 'agent-shell--get-available-modes) (lambda (&rest _) modes))
              ((symbol-function 'agent-shell--current-mode-id) (lambda (&rest _) "code"))
              ((symbol-function 'agent-shell--resolve-session-mode-name)
               (lambda (id available-modes)
                 (map-elt (seq-find (lambda (m) (equal (map-elt m :id) id)) available-modes) :name)))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _) (setq captured-table table) "plan"))
              ((symbol-function 'agent-shell--config-option-set-mode-id) #'ignore))
      (agent-shell-menu-select-session-mode))
    (should (equal "plan" (cddr (assoc "Plan" captured-table))))
    (should (equal "code" (cddr (assoc "Code" captured-table))))))

(ert-deftest agent-shell-menu/select-session-mode-sets-mode-by-id ()
  "The mode-id returned by `annotated-completing-read' (via target resolution)
is passed straight through to `agent-shell--config-option-set-mode-id',
with no intermediate name-to-id lookup."
  (let ((modes '((:id "code" :name "Code" :description "write code")
                 (:id "plan" :name "Plan" :description "make a plan")))
        captured-mode-id)
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'agent-shell--state) (lambda () 'fake-state))
              ((symbol-function 'map-nested-elt) (lambda (&rest _) "session-id"))
              ((symbol-function 'agent-shell--get-available-modes) (lambda (&rest _) modes))
              ((symbol-function 'agent-shell--current-mode-id) (lambda (&rest _) "code"))
              ((symbol-function 'agent-shell--resolve-session-mode-name)
               (lambda (id available-modes)
                 (map-elt (seq-find (lambda (m) (equal (map-elt m :id) id)) available-modes) :name)))
              ((symbol-function 'annotated-completing-read) (lambda (&rest _) "plan"))
              ((symbol-function 'agent-shell--config-option-set-mode-id)
               (lambda (&rest args) (setq captured-mode-id (plist-get args :mode-id)))))
      (agent-shell-menu-select-session-mode))
    (should (equal "plan" captured-mode-id))))

(ert-deftest agent-shell-menu/select-session-mode-errors-when-already-active ()
  "Selecting the mode that is already active signals an error."
  (let ((modes '((:id "code" :name "Code" :description "write code"))))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'agent-shell--state) (lambda () 'fake-state))
              ((symbol-function 'map-nested-elt) (lambda (&rest _) "session-id"))
              ((symbol-function 'agent-shell--get-available-modes) (lambda (&rest _) modes))
              ((symbol-function 'agent-shell--current-mode-id) (lambda (&rest _) "code"))
              ((symbol-function 'agent-shell--resolve-session-mode-name)
               (lambda (id available-modes)
                 (map-elt (seq-find (lambda (m) (equal (map-elt m :id) id)) available-modes) :name)))
              ((symbol-function 'annotated-completing-read) (lambda (&rest _) "code")))
      (should-error (agent-shell-menu-select-session-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-intercept-p

(ert-deftest agent-shell-menu/queue-intercept-p-nil-when-no-session ()
  "Returns nil when no shell session is reachable."
  (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer) (lambda () nil)))
    (should-not (agent-shell-queue-intercept-p))))

(ert-deftest agent-shell-menu/queue-intercept-p-nil-when-mode-default ()
  "Returns nil when input mode is default in the shell buffer."
  (let ((shell (generate-new-buffer " *mock-intercept-off*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'default))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should-not (agent-shell-queue-intercept-p))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/queue-intercept-p-nil-when-mode-queue-only ()
  "Returns nil when input mode is queue-only (not queue-intercept)."
  (let ((shell (generate-new-buffer " *mock-intercept-only*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'queue-only))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should-not (agent-shell-queue-intercept-p))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/queue-intercept-p-non-nil-when-mode-on ()
  "Returns non-nil when input mode is queue-intercept in the shell buffer."
  (let ((shell (generate-new-buffer " *mock-intercept-on*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'queue-intercept))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should (agent-shell-queue-intercept-p))))
      (kill-buffer shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-queue-only-p

(ert-deftest agent-shell-menu/queue-only-p-nil-when-no-session ()
  "Returns nil when no shell session is reachable."
  (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer) (lambda () nil)))
    (should-not (agent-shell-queue-only-p))))

(ert-deftest agent-shell-menu/queue-only-p-nil-when-mode-default ()
  "Returns nil when input mode is default."
  (let ((shell (generate-new-buffer " *mock-only-off*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'default))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should-not (agent-shell-queue-only-p))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/queue-only-p-nil-when-mode-intercept ()
  "Returns nil when input mode is queue-intercept (not queue-only)."
  (let ((shell (generate-new-buffer " *mock-only-intercept*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'queue-intercept))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should-not (agent-shell-queue-only-p))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/queue-only-p-non-nil-when-mode-queue-only ()
  "Returns non-nil when input mode is queue-only."
  (let ((shell (generate-new-buffer " *mock-only-on*")))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (setq-local agent-shell-queue-input-mode 'queue-only))
          (cl-letf (((symbol-function 'agent-shell-menu--session-shell-buffer)
                     (lambda () shell)))
            (should (agent-shell-queue-only-p))))
      (kill-buffer shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-menu/dispatch-no-key-prefix-conflicts ()
  "No key in agent-shell-menu-dispatch is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-menu-dispatch))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-menu/dispatch-no-duplicate-keys ()
  "No key appears more than once in agent-shell-menu-dispatch."
  (let* ((keys (transient-test/collect-keys 'agent-shell-menu-dispatch))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

;;;; agent-shell-menu-session-info (with-help-window migration)

(ert-deftest agent-shell-menu/info-map-inherits-help-mode-map ()
  "agent-shell-menu-info-map has help-mode-map as an ancestor."
  (require 'help-mode)
  (let ((map agent-shell-menu-info-map))
    (while (and map (not (eq map help-mode-map)))
      (setq map (keymap-parent map)))
    (should (eq map help-mode-map))))


(ert-deftest agent-shell-menu/select-collapse-annotations-state-and-effect ()
  "Annotations in agent-shell-menu-select-collapse explicitly show current state and effect."
  (let ((buf (generate-new-buffer "*mock-agent-shell-collapse*"))
        (agent-shell-thought-process-expand-by-default t)
        (agent-shell-tool-use-expand-by-default nil)
        (captured-table nil))
    (unwind-protect
        (with-current-buffer buf
          (setq major-mode 'agent-shell-mode)
          (cl-letf (((symbol-function 'annotated-completing-read)
                     (lambda (table &rest _args)
                       (setq captured-table table)
                       "+ expand all")))
            (agent-shell-menu-select-collapse)
            (should (equal "currently expanded by default → select to collapse"
                           (gethash "~ thinking: expand-by-default" captured-table)))
            (should (equal "currently collapsed by default → select to expand"
                           (gethash "~ tool call: expand-by-default" captured-table)))
            (should (equal "select to expand every collapseable block"
                           (gethash "+ expand all" captured-table)))
            (should (equal "select to collapse every collapseable block"
                           (gethash "+ collapse all" captured-table)))))
      (kill-buffer buf))))
(provide 'test-agent-shell-menu)
