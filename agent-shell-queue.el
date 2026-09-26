;;; agent-shell-queue.el --- Persistent prompt queue for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-queue
;; Package-Requires: ((emacs "29.1") (transient "0.4") (agent-shell "0.1") (alert "1.2") (annotated-completing-read "0.1"))

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

(provide 'agent-shell-queue)

;;; agent-shell-queue.el ends here
