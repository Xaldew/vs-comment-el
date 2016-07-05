;;; vs-comment-init.el --- Test initialization code.

;;; Commentary:
;; Test setup code.

;;; Code:

(require 'undercover)
(undercover "vs-comment-mode.el")

(require 'f)
(require 'ert)

(defvar vs-comment-test/test-path
  (f-dirname (f-this-file)))

(defvar vs-comment-test/root-path
  (f-parent vs-comment-test/test-path))

(load (f-expand "vs-comment-mode" vs-comment-test/root-path))

(provide 'vs-comment-init)

;;; vs-comment-init.el ends here
