;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (whitespace "13") (parsec))
;; Keywords: extensions

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

(defun conllu--spaces ()
  (parsec-many (parsec-ch ? )))

(defun conllu--symbol (cp)
  (parsec-optional*
    (conllu--spaces))
  (funcall cp))

(defun conllu--empty-field ()
  ;; doesn't work
  (conllu--symbol (function (parsec-ch ?_))))

(defun conllu--field ()
  (conllu--symbol '(parsec-none-of ?\t ?\n)))

(defun conllu--maybe-empty-field ()
  (parsec-or (conllu--empty-field) (conllu--field)))

(defun conllu--index ()
  (string-to-int (conllu--field)))

(defun conllu--token ()
  (parsec-collect
   (conllu--index) ; might not be empty
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)
   (conllu--index)
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)
   (conllu--maybe-empty-field)))

(defun conllu-token-head ()
  (when (conllu-not-looking-at-token)
    (user-error "%s" "Error: not on token line"))
  (beginning-of-line)
  (destructuring-bind (ix _ _ _ _ _ h _ _ _) (parsec-parse (conllu--token))
    (forward-line (- h ix))))

(provide 'conllu-parse)
