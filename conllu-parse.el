;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (parsec "0.1") (cl-lib "0.5"))
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

;;; Commentary:
;; this mode provides simple utilities for editing and viewing CoNLL-U files.

;; it offers the following features, and more:

;; - highlighting comments, and upostag and deprel fields
;; - truncate lines by default
;; - show newline and tab characters using whitespace.el
;; - aligning and unaligning column fields
;; - jumping to next or previous sentence
;; - in a token line, jump to its head

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
     (string-to-number
      (parsec-many-as-string (parsec-digit))))))

(defun conllu--meta-separator ()
  (parsec-optional-maybe
   (parsec-one-of ?- ?.)))

(defun conllu--other-index ()
  (parsec-optional-maybe
   (parsec-many-as-string (parsec-digit))))

(defun conllu--tab ()
  (conllu--symbol #'parsec-ch ?\t)
  nil)

(defun conllu--eol ()
  (conllu--symbol #'parsec-newline)
  nil)

(defun conllu--token ()
  (parsec-collect*
   (conllu--index) ; might not be empty
   (conllu--meta-separator)
   (conllu--other-index)
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
   (conllu--maybe-empty #'conllu--index)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--tab)
   (conllu--maybe-empty #'conllu--no-space-field)
   (conllu--eol)))

(provide 'conllu-parse)

;;; conllu-parse.el ends here
