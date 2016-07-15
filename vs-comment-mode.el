;;; vs-comment-mode.el --- VS comment highlights -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Keywords: convenience, highlighting
;; Created: 2016-07-03
;; Version: 0.0.1

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

;; This package allows you annotate your comments similar to Visual Studio.
;;
;; To use: (vs-comment-mode)

;;; Code:


(require 'newcomment)
(require 'font-lock)


(defface vs-comment-strike-through
  '((t (:strike-through t)))
  "Strike-through comment highlights."
  :group 'faces)

(defface vs-comment-important
  '((t (:foreground "#ff0000")))
  "Important comment highlighting."
  :group 'faces)

(defface vs-comment-question
  '((t (:foreground "#00ff00")))
  "Question comment highlighting."
  :group 'faces)

(defface vs-comment-todo
  '((t (:foreground "#f4ef10")))
  "TODO comment highlighting."
  :group 'faces)


(defcustom vs-comment-keyword-list
  `(("x"    'vs-comment-strike-through)
    ("\\?"  'vs-comment-question)
    ("!"    'vs-comment-important)
    ("TODO" 'vs-comment-todo))
  "List of comment modifying keywords."
  :group 'comments)


(defvar vs-comment-keywords nil
  "List of dynamic font-lock keywords such as multi-character comments.")

(make-variable-buffer-local 'vs-comment-keywords)


(defun vs-comment--create-keywords (modifier face)
  "Create font-locking keyword entries from MODIFIER and FACE."
  (list (list (concat
               "\\s<+[[:space:]]?"
               modifier
               "[[:space:]]*\\(?1:.*?\\)[[:space:]]*\\s>")
              1 face 'prepend)
        (list (concat
               comment-start-skip
               modifier
               "[[:space:]]*\\(?2:.*\\)[[:space:]]*"
               comment-end)
              2 face 'prepend)))


(defun vs-comment--create-keyword-list ()
  "Create the font-locking keyword list."
  (let (entry keywords)
    (dolist (pair vs-comment-keyword-list keywords)
      (setq entry (vs-comment--create-keywords (nth 0 pair)
                                               (nth 1 pair)))
      (setq keywords (append keywords entry)))))


(defun vs-comment--font-lock-flush ()
  "Compatibility layer around `font-lock-flush'."
  (if (fboundp 'font-lock-flush)
      (progn
        (font-lock-flush)
        (font-lock-ensure))
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


(defun vs-comment--turn-on ()
  "Turn on `vs-comment-mode'."
  (setq vs-comment-keywords (vs-comment--create-keyword-list))
  (font-lock-add-keywords nil vs-comment-keywords)
  (vs-comment--font-lock-flush))


(defun vs-comment--turn-off ()
  "Turn off `vs-comment-mode'."
  (font-lock-remove-keywords nil vs-comment-keywords)
  (vs-comment--font-lock-flush))


(define-minor-mode vs-comment-mode
  "Minor mode to highlight comments similar to visual studio."
  :global nil
  :init-value nil
  :lighter ""
  (if vs-comment-mode
      (vs-comment--turn-on)
    (vs-comment--turn-off)))


(provide 'vs-comment-mode)

;;; vs-comment-mode.el ends here
