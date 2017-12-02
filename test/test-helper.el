;;; test-helper.el --- Helpers for malinka-test.el

;;; Commentary:

;; Utilities for running malinka tests.

;;; Code:
(require 'ert)

(require 'f)
(let ((malinka-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path malinka-dir))

(require 'malinka)

;;; test-helper.el ends here
