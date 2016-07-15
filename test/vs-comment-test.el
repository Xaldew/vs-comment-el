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
(require 'font-lock)
(require 'cl)


(defun vs-comment--face-p (pos faces)
  "Return non-nil if any of the faces at POS is present in FACES."
  (let ((f (get-text-property pos 'face)))
    (cl-intersection f (list faces))))


(defun vs-comment--location-test (file mode locations faces)
  "Test if the nth point in FILE with major MODE LOCATIONS contain FACES.

The nth point in LOCATION must contain the face at the nth face
in FACES for this test to pass."
  (with-temp-buffer
    (insert-file-contents file)
    (funcall mode)
    (font-lock-mode)
    (font-lock-fontify-buffer)
    (cl-every #'identity (cl-mapcar #'vs-comment--face-p locations faces))))


(ert-deftest vs-comment--python-test ()
  (add-hook 'python-mode-hook #'vs-comment-mode)
  (let* ((dir (file-name-as-directory vs-comment-test/test-path))
         (file (concat dir "main.py"))
         (locations '(81 108 194 260))
         (faces '(vs-comment-important
                  vs-comment-strike-through
                  vs-comment-question
                  vs-comment-todo)))
    (should (vs-comment--location-test file #'python-mode locations faces))))


(ert-deftest vs-comment--c++-test ()
  (add-hook 'c++-mode-hook #'vs-comment-mode)
  (let* ((dir (file-name-as-directory vs-comment-test/test-path))
         (file (concat dir "main.cpp"))
         (locations '(48 90 150 187 264 301 331 354))
         (faces '(vs-comment-important
                  vs-comment-important
                  vs-comment-question
                  vs-comment-question
                  vs-comment-todo
                  vs-comment-todo
                  vs-comment-strike-through
                  vs-comment-strike-through)))
    (should (vs-comment--location-test file #'c++-mode locations faces))))


(provide 'vs-comment-test)

;;; vs-comment-test.el ends here
