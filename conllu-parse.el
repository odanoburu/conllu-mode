;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (whitespace "13") (parsec) (cl-lib))
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

(defun conllu--symbol (parser &rest args)
  (parsec-and
   (conllu--spaces)
   (apply parser args)))

(defun conllu--empty-field ()
  (conllu--symbol #'parsec-ch ?_))

(defun conllu--no-space-field ()
  (conllu--symbol
   (lambda ()
     (parsec-many-as-string
      (parsec-none-of ?\t ?\s ?\n)))))

(defun conllu--maybe-empty (parser &rest args)
  (parsec-or
   (conllu--empty-field)
   (apply parser args)))

(defun conllu--index ()
  (conllu--symbol
   (lambda ()
     (string-to-int
      (parsec-many-as-string (parsec-digit))))))

(defun conllu--tab ()
  (conllu--symbol #'parsec-ch ?\t)
  nil)

(defun conllu--eol ()
  (conllu--symbol #'parsec-newline)
  nil)

(defun conllu--token ()
  (parsec-collect*
   (conllu--index) ; might not be empty
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--index) ; TODO: can be empty
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--eol)))

(provide 'conllu-parse)

;;; conllu-parse.el ends here
