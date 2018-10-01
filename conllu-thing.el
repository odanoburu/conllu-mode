;;; conllu-thing.el --- utilities for CoNLL-U files  -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.1.6
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (s "1.0"))
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
;; this mode provides simple utilities for editing and viewing CoNLL-U
;; files.

;; it offers the following features, and more:

;; - highlighting comments, and upostag and deprel fields
;; - truncate lines by default
;; - show newline and tab characters using whitespace.el
;; - aligning and unaligning column fields
;; - jumping to next or previous sentence
;; - in a token line, jump to its head

;;; code:

;;;
;; dependencies
(require 'cl-lib)
(require 's)

;;;
;; token
;< looking at functions
(defsubst conllu--looking-at (regexp)
  "Call `looking-at' at the beginning-of-line and return its result.
Return point to original position."
  (save-excursion (beginning-of-line)
                  (looking-at regexp)))

(defsubst conllu--looking-at-empty-line ()
  "Return t if looking at blank line."
  (conllu--looking-at "^ *$"))

(defsubst conllu--not-looking-at-token ()
  "Return t if looking at blank or comment line, nil otherwise."
  (conllu--looking-at " *$\\|#"))

;; tokens are divided in simple, multi and empty tokens.
(defsubst conllu--looking-at-stoken ()
  "Return t if looking at a simple token line, nil otherwise."
  (conllu--looking-at "[0-9]+[^-.]"))

(defsubst conllu--looking-at-mtoken ()
  "Return t if looking at a multi-token line, nil otherwise."
  (conllu--looking-at "[0-9]+-[0-9]"))

(defsubst conllu--looking-at-etoken ()
  "Return t if looking at an empty token line, nil otherwise."
  (conllu--looking-at "[0-9]+\\.[0-9]"))
;>

(defun conllu--barf-if-not-at-token-line (&optional message)
  "Displays error MESSAGE if not at token line."
  (when (conllu--not-looking-at-token)
    (user-error "%s" (or message "Error: not at token line"))))

(defun conllu--id> (id id2)
  "Return t if CoNLL-U field ID is greater than ID2."
  (pcase (cons id id2)
    (`((,_ ,beg ,_) . (,_ ,beg2 ,_))
     ; todo: does this ever happen? if so it's incorrect
     (> beg beg2))
    (`((,_ ,beg ,_) . ,n)
     (> beg n))
    (`(,n . (,_ ,beg ,_))
     (> n beg))
    (`(,n . ,n2)
     (> n n2))))

(defun conllu--sentence-begin-point ()
  "Return point of the beginning of current sentence."
  (save-excursion (backward-sentence) (point)))

(defun conllu--sentence-tokens-begin-point()
  "Return point of the beginning of the first token line."
  (save-excursion (backward-sentence)
                  (conllu-forward-to-token-line)
                  (point)))

(defun conllu--sentence-end-point ()
  "Return point of the end of current sentence."
  (save-excursion (forward-sentence) (point)))

(defun conllu--sentence-points ()
  "Return points that delimit current sentence."
  (list (conllu--sentence-begin-point)
        (conllu--sentence-end-point)))

(defun conllu--sentence-tokens-points ()
  "Return points that delimit the token lines of the sentence at point."
  (list (conllu--sentence-tokens-begin-point)
        (conllu--sentence-end-point)))

(provide 'conllu-thing)

;;; conllu-thing.el ends here
