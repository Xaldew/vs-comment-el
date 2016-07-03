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


(defface vs-comment-strike-through
  '((t (:strike-through t)))
  "Strike-through comment highlights."
  :group 'faces)

(defface vs-comment-important
  '((t (:foreground "#00ff00")))
  "Important comment highlighting."
  :group 'faces)

(defface vs-comment-question
  '((t (:foreground "#ff0000")))
  "Question comment highlighting."
  :group 'faces)

(defface vs-comment-todo
  '((t (:foreground "#f4ef10")))
  "TODO comment highlighting."
  :group 'faces)


(defcustom vs-comment-keyword-list
  `(("x"    'vs-comment-strike-through)
    ("\\?"  'vs-comment-important)
    ("!"    'vs-comment-question)
    ("TODO" 'vs-comment-todo))
  "List of comment modifying keywords."
  :group 'comments)


(defvar-local vs-comment-keywords nil
  "List of dynamic font-lock keywords such as multi-character comments.")


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


(defun vs-comment--turn-on ()
  "Turn on `vs-comment-mode'."
  (setq vs-comment-keywords (vs-comment--create-keyword-list))
  (font-lock-add-keywords nil vs-comment-keywords)
  (font-lock-flush (point-min) (point-max))
  (font-lock-ensure (point-min) (point-max)))


(defun vs-comment--turn-off ()
  "Turn off `vs-comment-mode'."
  (font-lock-remove-keywords nil vs-comment-keywords)
  (font-lock-flush (point-min) (point-max))
  (font-lock-ensure (point-min) (point-max)))


(define-minor-mode vs-comment-mode
  :global nil
  :init-value nil
  :lighter ""
  "Minor mode to highlight comments similar to visual studio."
  (if vs-comment-mode
      (vs-comment--turn-on)
    (vs-comment--turn-off)))


(provide 'vs-comment-mode)

;;; vs-comment-mode.el ends here