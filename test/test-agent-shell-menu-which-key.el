;;; test-agent-shell-menu-which-key.el --- Menu label tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for Which Key label lifecycle.
;;; Code:
(require 'ert)
(require 'agent-shell-menu)
(ert-deftest agent-shell-menu/which-key-owned-lifecycle ()
  (let ((which-key-replacement-alist nil)
        (agent-shell-menu--which-key-owned nil)
        (agent-shell-menu-global-navigation-mode t))
    (agent-shell-menu--sync-which-key)
    (should (member agent-shell-menu--which-key-replacement which-key-replacement-alist))
    (setq agent-shell-menu-global-navigation-mode nil)
    (agent-shell-menu--sync-which-key)
    (should-not which-key-replacement-alist)))
(ert-deftest agent-shell-menu/which-key-preserves-existing ()
  (let ((which-key-replacement-alist (list agent-shell-menu--which-key-replacement))
        (agent-shell-menu--which-key-owned nil)
        (agent-shell-menu-global-navigation-mode t))
    (agent-shell-menu--sync-which-key)
    (setq agent-shell-menu-global-navigation-mode nil)
    (agent-shell-menu--sync-which-key)
    (should which-key-replacement-alist)))
(provide 'test-agent-shell-menu-which-key)
;;; test-agent-shell-menu-which-key.el ends here
