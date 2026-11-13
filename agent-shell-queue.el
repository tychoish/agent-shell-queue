;;; agent-shell-queue.el --- Persistent prompt queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell "0.1") (alert "1.2") (annotated-completing-read "0.1") (hitl "0.1.0") (nerd-icons "0.1"))

;; This file is not part of GNU Emacs

;;; Commentary:

;; Implements a persistent prompt queue for agent-shell sessions, supporting
;; multi-session dispatch with pause, resume, and archive lifecycle management.
;; Queue state is serialized to plist, JSON, or YAML for session persistence
;; across Emacs restarts.  Interactive capture, edit, and item-view buffers
;; allow queue manipulation without leaving Emacs.  Fork operations split a
;; queue across multiple sessions for parallel workloads.

;;; Code:

(require 'agent-shell-queue-core)
(require 'agent-shell-queue-ui)
(require 'agent-shell-queue-plan)

(autoload 'agent-shell-queue-overload-mode "agent-shell-queue-overload"
  "Global minor mode routing inbuilt agent-shell queue to ASQ." t)

(autoload 'agent-shell-menu-dispatch "agent-shell-menu"
  "agent-shell operations — navigate, act, send, queue, and session management." t)


(defvar agent-shell-queue-global--managed-settings nil
  "Alist of (SYMBOL . VALUE) settings managed globally by `agent-shell-queue-global-mode'.")

(defvar agent-shell-queue-global--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling `agent-shell-queue-global-mode'.")

(defun agent-shell-queue-global--apply-settings ()
  "Save previous values and apply `agent-shell-queue-global--managed-settings'."
  (setq agent-shell-queue-global--saved-settings nil)
  (dolist (pair agent-shell-queue-global--managed-settings)
    (let ((sym (car pair))
          (val (cdr pair)))
      (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
            agent-shell-queue-global--saved-settings)
      (set sym val))))

(defun agent-shell-queue-global--revert-settings ()
  "Restore variables to their state prior to `agent-shell-queue-global-mode'."
  (dolist (entry agent-shell-queue-global--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set sym (cdr status))
        (makunbound sym))))
  (setq agent-shell-queue-global--saved-settings nil))

(defun agent-shell-queue--enable-integrations ()
  "Enable `agent-shell-queue' advice, hooks, and integrations."
  (agent-shell-queue-global--apply-settings)
  (add-hook 'kill-emacs-hook #'agent-shell-queue--save-on-exit)
  (advice-add 'agent-shell-interrupt :before #'agent-shell-queue--on-interrupt)
  (advice-add 'agent-shell--send-command :before #'agent-shell-queue--on-send-command)
  (advice-add 'agent-shell-send-file-to :around #'agent-shell-queue--ad-agent-shell-send-file-to)
  (advice-add 'shell-maker-submit :before #'agent-shell-queue--on-submit-intercept)
  (agent-shell-menu-install)
  (advice-add 'agent-shell-switch-buffer :override #'agent-shell-menu--switch-buffer)
  (advice-add 'agent-shell-rename-buffer :override #'agent-shell-menu-name-buffer)
  (keymap-set agent-shell-mode-map "<remap> <rename-buffer>" #'agent-shell-menu-name-buffer)
  (keymap-set agent-shell-mode-map "<remap> <shell-maker-rename-buffer>" #'agent-shell-menu-name-buffer)
  (agent-shell-queue-ui-install-nerd-icons))

(defun agent-shell-queue--disable-integrations ()
  "Disable `agent-shell-queue' advice, hooks, and integrations."
  (remove-hook 'kill-emacs-hook #'agent-shell-queue--save-on-exit)
  (advice-remove 'agent-shell-interrupt #'agent-shell-queue--on-interrupt)
  (advice-remove 'agent-shell--send-command #'agent-shell-queue--on-send-command)
  (advice-remove 'agent-shell-send-file-to #'agent-shell-queue--ad-agent-shell-send-file-to)
  (advice-remove 'shell-maker-submit #'agent-shell-queue--on-submit-intercept)
  (advice-remove 'agent-shell-switch-buffer #'agent-shell-menu--switch-buffer)
  (advice-remove 'agent-shell-rename-buffer #'agent-shell-menu-name-buffer)
  (keymap-unset agent-shell-mode-map "<remap> <rename-buffer>" t)
  (keymap-unset agent-shell-mode-map "<remap> <shell-maker-rename-buffer>" t)
  (agent-shell-queue-ui-uninstall-nerd-icons)
  (agent-shell-queue-global--revert-settings))

;;;###autoload
(define-minor-mode agent-shell-queue-global-mode
  "Global minor mode managing `agent-shell-queue' lifecycle, hooks, and advice."
  :global t
  :group 'agent-shell-queue
  (if agent-shell-queue-global-mode
      (agent-shell-queue--enable-integrations)
    (agent-shell-queue--disable-integrations)))

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
