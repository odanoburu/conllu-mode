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
  (parsec-optional*
   (parsec-many
    (parsec-ch ?\s))))

(defun conllu--empty-field ()
  (conllu--spaces)
  (parsec-ch ?_))

(defun conllu--field ()
  (conllu--spaces)
  (parsec-many-as-string
   (parsec-none-of ?\t ?\s ?\n)))

(defun conllu--maybe-empty-field ()
  (parsec-or
   (conllu--empty-field)
   (conllu--field)))

(defun conllu--index ()
  (conllu--spaces)
  (string-to-int
   (parsec-many-as-string (parsec-digit))))

(defun conllu--tab ()
  (conllu--spaces)
  (parsec-ch ?\t) nil)

(defun conllu--eol ()
  (conllu--spaces)
  (parsec-newline) nil)

(defun conllu--token ()
  (parsec-collect*
   (conllu--index) ; might not be empty
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--index)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--tab)
   (conllu--maybe-empty-field)
   (conllu--eol)))

(provide 'conllu-parse)

;;; conllu-parse.el ends here
