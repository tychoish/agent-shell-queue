#!/usr/bin/env -S emacs --script

;;; run-tests --- Run the agent-shell-queue ERT test suite -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Standalone test runner: no Cask, no ert-runner. GNU ELPA/MELPA/NonGNU ELPA
;; dependencies (transient, agent-shell, annotated-completing-read, alert,
;; and their own transitive dependencies) are installed on demand via package.el
;; into .deps/elpa. This directory is gitignored and reused across runs, so
;; repeat invocations do not pay the network cost again.
;;
;; Usage:
;;   ./run-tests
;;   emacs --script run-tests

;;; Code:

(require 'seq)
(require 'ert)
(require 'package)

(defconst asq-test-runner--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory this script lives in -- the agent-shell-queue package root.")

(defconst asq-test-runner--deps-dir
  (expand-file-name ".deps" asq-test-runner--root)
  "Directory holding fetched dependencies, gitignored and cached across runs.")

(defconst asq-test-runner--melpa-packages
  '(transient agent-shell annotated-completing-read alert)
  "Packages fetched from GNU ELPA/MELPA/NonGNU ELPA via package.el.")

(defun asq-test-runner--ensure-melpa-packages ()
  "Install any missing MELPA-PACKAGES into `package-user-dir', then load them."
  (setq package-user-dir (expand-file-name "elpa" asq-test-runner--deps-dir))
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (setq package-check-signature nil)
  (package-initialize)
  (let ((missing (seq-remove #'package-installed-p asq-test-runner--melpa-packages)))
    (when missing
      (package-refresh-contents)
      (seq-do #'package-install missing)
      (package-initialize))))

(message "Setting up dependencies...")
(asq-test-runner--ensure-melpa-packages)

;; Add root and test directory to load-path
(add-to-list 'load-path asq-test-runner--root)
(add-to-list 'load-path (expand-file-name "test" asq-test-runner--root))

(message "Loading test files...")
(seq-do (lambda (file) (load file nil t))
        (directory-files (file-name-concat asq-test-runner--root "test")
                          t "\\`test-.*\\.el\\'"))

(message "Running test suite...")
(ert-run-tests-batch-and-exit "^agent-shell")

;;; run-tests ends here
