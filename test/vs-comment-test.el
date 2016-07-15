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
    (cl-intersection
     (if (not (listp f))
         (list f)
       f)
     (list faces))))


(defun vs-comment--location-test (locations faces)
  "Test if the nth point in LOCATIONS contain the nth face in FACES.

The nth point in LOCATION must contain the face at the nth face
in FACES for this test to pass."
  (cl-every #'identity (cl-mapcar #'vs-comment--face-p locations faces)))


(defun vs-comment--test-mode (file mode locations faces)
  "Test enabling and disabling `vs-comment-mode' in FILE with major MODE.

The nth point in LOCATIONS is supposed to be matched with the nth
face in FACES when enabled.  When disabled, it checks that the
faces have been removed."
  (with-temp-buffer
    (insert-file-contents file)
    (let (ret0 ret1)
      (global-font-lock-mode t)
      (font-lock-mode t)
      (funcall mode)
      ;; Enable `vs-comment-mode'.
      (vs-comment-mode t)
      (font-lock-fontify-buffer)
      (setq ret0 (vs-comment--location-test locations faces))
      ;; Disable `vs-comment-mode'.
      (vs-comment-mode -1)
      (font-lock-fontify-buffer)
      (setq ret1 (not (vs-comment--location-test locations faces)))
      (and ret0 ret1))))


(ert-deftest vs-comment--python-test ()
  (let* ((dir (file-name-as-directory vs-comment-test/test-path))
         (file (concat dir "main.py"))
         (locations '(81 108 194 260))
         (faces '(vs-comment-important
                  vs-comment-strike-through
                  vs-comment-question
                  vs-comment-todo)))
    (should (vs-comment--test-mode file #'python-mode locations faces))))


(ert-deftest vs-comment--c++-test ()
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
    (should (vs-comment--test-mode file #'c++-mode locations faces))))


(provide 'vs-comment-test)

;;; vs-comment-test.el ends here
