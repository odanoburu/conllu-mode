;;; conllu-move.el --- movement code for conllu-mode -*- lexical-binding: t; -*-
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

(require 'conllu-parse)

(require 'cl-lib)

;;;
;; token

;; looking at functions
(defsubst conllu--not-looking-at-token ()
  "Return t if looking at blank or comment line, nil otherwise.
assumes point is at beginning of line."
  (looking-at (concat " *$" "\\|" "#")))

(defsubst conllu--looking-at-end-of-field ()
  "Skip forward over one field."
  (skip-chars-forward "^[\t\n]"))

;; tokens are divided in simple, multi and empty tokens.
(defsubst conllu--looking-at-stoken ()
  "return t if looking at a simple token line, nil
otherwise. assumes point is at beginning of line."
  (looking-at "[0-9]*[^-.]\t"))

(defsubst conllu--looking-at-mtoken ()
  "return t if looking at a multi-token line, nil
otherwise. assumes point is at beginning of line."
  (looking-at "[0-9]*-[0-9]*\t"))

(defsubst conllu--looking-at-etoken ()
  "return t if looking at an empty token line, nil
otherwise. assumes point is at beginning of line."
  (looking-at "[0-9]*\\.[0-9]*\t"))

(defun conllu-move-to-head ()
  "moves point to the head token of the present token (if it has
one). if root, moves to beginning of sentence"
  (interactive)
  (beginning-of-line)
  (when (conllu--not-looking-at-token)
    (user-error "%s" "Error: not on token line"))
  (destructuring-bind (ix _ _ _ _ _ _ _ h _ _ _)
      (parsec-parse (conllu--token))
    (forward-line -1) ;; back to parsed line
    (when (member h (list "_" 0))
      (user-error "%s" "Error: token has no head"))
    (conllu--move-to-existing-head ix h)))

(defun conllu--move-to-existing-head (ix head)
  (if (> ix head)
      (conllu--move-forward-to-head head -1)
    (conllu--move-forward-to-head head 1)))

(defun conllu--move-forward-to-head (head n)
  (beginning-of-line)
  (unless (looking-at (concat (int-to-string head) "\t"))
    (progn (forward-line n)
           (conllu--move-forward-to-head head n))))

;;;
;; sentence
(defun conllu-forward-to-token-line ()
  "move to next token line."
  (conllu--move-to-token-line 1))

(defun conllu-backward-to-token-line ()
  "move to previous token line."
  (conllu--move-to-token-line -1))

(defun conllu--move-to-token-line (n)
  "call with 1 or -1."
  (when (conllu-not-looking-at-token)
    (forward-line n)
    (conllu--move-to-token-line n)))

(defun conllu-forward-sentence ()
  "jump to end of sentence, which in CoNLL-U files is actually
the next blank line."
  (interactive)
  (forward-sentence)
  (forward-line))

(defun conllu-next-sentence ()
  "unalign sentence at point, jump to next sentence and align it."
  (interactive)
  (conllu-unalign-fields (sentence-begin-point) (sentence-end-point))
  (conllu-forward-sentence)
  (conllu-forward-to-token-line)
  (conllu-align-fields (sentence-begin-point) (sentence-end-point)))

(defun conllu-previous-sentence ()
  "unalign sentence at point, jump to next sentence and align it."
  (interactive)
  (conllu-unalign-fields (sentence-begin-point) (sentence-end-point))
  (backward-sentence)
  (conllu-backward-to-token-line)
  (conllu-align-fields (sentence-begin-point) (sentence-end-point)))

(provide 'conllu-move)

;;; conllu-move.el ends here
