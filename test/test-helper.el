;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds the package root to load-path for batch test runs and defines
;; transient key-integrity helpers used by the menu tests.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root))

(defun agent-shell-test/suffix-plist (suffix)
  "Extract the plist from a parsed transient suffix spec SUFFIX.
Handles both the (CLASS :key ...) cons format and the
(LEVEL CLASS (:key ...)) tuple format produced by different transient versions."
  (if (integerp (car suffix))
      (nth 2 suffix)
    (cdr suffix)))

;;; Transient key introspection helpers

(defun transient-test/collect-keys (prefix-sym)
  "Return a list of all :key strings in PREFIX-SYM's transient layout."
  (let ((queue (list (get prefix-sym 'transient--layout)))
        keys)
    (while queue
      (let ((node (pop queue)))
        (cond
          ((and (vectorp node) (not (byte-code-function-p node)))
           (setq queue (append (seq-into node 'list) queue)))
          ((and (consp node) (eq (car node) 'transient-suffix))
           (when-let* ((key (plist-get (cdr node) :key)))
             (push key keys)))
          ((consp node)
           (setq queue (append node queue))))))
    (nreverse keys)))

(defun transient-test/key-sequence-prefix-p (short long)
  "Return non-nil if SHORT key string is a strict prefix of LONG."
  (when (not (equal short long))
    (let ((sv (kbd short))
          (lv (kbd long)))
      (and (< (length sv) (length lv))
           (equal sv (substring lv 0 (length sv)))))))

(defun transient-test/key-prefix-conflicts (keys)
  "Return a list of (SHORT LONG) pairs where SHORT is a strict prefix of LONG."
  (let (conflicts)
    (dolist (short keys)
      (dolist (long keys)
        (when (transient-test/key-sequence-prefix-p short long)
          (push (list short long) conflicts))))
    conflicts))

(defun transient-test/duplicate-keys (keys)
  "Return a list of keys that appear more than once in KEYS."
  (let (seen dups)
    (dolist (k keys)
      (if (member k seen)
          (unless (member k dups)
            (push k dups))
        (push k seen)))
    dups))

(provide 'test-helper)
;;; test-helper.el ends here
