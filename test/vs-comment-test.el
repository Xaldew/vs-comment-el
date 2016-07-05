;;; vs-comment-test.el --- Face changing unit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Gustaf Waldemarson

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Maintainer: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Unittest code for `vs-comment-mode'.

;;; Code:

(require 'vs-comment-init)
(require 'faceup)


(defun vs-comment-font-lock-test-facit (file mode)
  "Test that FILE is fontified as the `file'.faceup file describes.

MODE is the major mode to use for the test."
  (faceup-test-font-lock-file mode
                              (concat
                               (file-name-as-directory
                                vs-comment-test/test-path)
                               file)))
(faceup-defexplainer vs-comment-font-lock-test-facit)


(ert-deftest vs-comment-font-lock-file-test ()
  (add-hook 'c++-mode-hook    #'vs-comment-mode)
  (add-hook 'python-mode-hook #'vs-comment-mode)
  (should (vs-comment-font-lock-test-facit "main.py"  'python-mode))
  (should (vs-comment-font-lock-test-facit "main.cpp" 'c++-mode)))


(provide 'vs-comment-test)

;;; vs-comment-test.el ends here
