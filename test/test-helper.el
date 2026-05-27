;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds the package root to load-path for batch test runs.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root))

(provide 'test-helper)
;;; test-helper.el ends here
