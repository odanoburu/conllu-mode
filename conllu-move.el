;;; conllu-move.el --- movement code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.1.5
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

(require 'conllu-parse)
(require 'conllu-thing)

(eval-when-compile
  (require 'cl-lib))

;;; Code:

;;;
;; fields
(defsubst conllu--skip-backward-to-end-of-field ()
  "Skip backward over one field."
  (skip-chars-backward "^\t^\n"))

(defsubst conllu--skip-forward-to-end-of-field ()
  "Skip forward over one field."
  (skip-chars-forward "^\t^\n"))

(defun conllu-field-forward ()
  "Move to next field.
if at end of sentence, go to next line."
  (interactive)
  (conllu--skip-forward-to-end-of-field)
  (forward-char))

(defun conllu--move-to-field-number (n)
  "Move to field number N.
N must be inbouds, i.e., 0 < N <= 10."
  (beginning-of-line)
  (conllu--barf-if-not-at-token-line)
  (dotimes (_t (1- n) t)
    (conllu-field-forward)))

(defun conllu-move-to-field-number-1 ()
  "Move point to field ID."
  (interactive)
  (conllu--move-to-field-number 1))

(defun conllu-move-to-field-number-2 ()
  "Move point to field FORM."
  (interactive)
  (conllu--move-to-field-number 2))

(defun conllu-move-to-field-number-3 ()
  "Move point to field LEMMA."
  (interactive)
  (conllu--move-to-field-number 3))

(defun conllu-move-to-field-number-4 ()
  "Move point to field UPOSTAG."
  (interactive)
  (conllu--move-to-field-number 4))

(defun conllu-move-to-field-number-5 ()
  "Move point to field XPOSTAG."
  (interactive)
  (conllu--move-to-field-number 5))

(defun conllu-move-to-field-number-6 ()
  "Move point to field FEATS."
  (interactive)
  (conllu--move-to-field-number 6))

(defun conllu-move-to-field-number-7 ()
  "Move point to field HEAD."
  (interactive)
  (conllu--move-to-field-number 7))

(defun conllu-move-to-field-number-8 ()
  "Move point to field DEPREL."
  (interactive)
  (conllu--move-to-field-number 8))

(defun conllu-move-to-field-number-9 ()
  "Move point to field DEPS."
  (interactive)
  (conllu--move-to-field-number 9))

(defun conllu-move-to-field-number-10 ()
  "Move point to field MISC."
  (interactive)
  (conllu--move-to-field-number 10))

(defun conllu-field-backward ()
  "Move to previous field.
if at beginning of sentence, go to previous line"
  (interactive)
  (skip-chars-backward "^[\t\n]")
  (forward-char -1)
  (skip-chars-backward "^[\t\n]"))

;< move to token head
(defun conllu-move-to-head ()
  "Move point to the head token of the present token (if it has one).
if root, moves to beginning of sentence."
  (interactive)
  (beginning-of-line)
  (conllu--barf-if-not-at-token-line)
  (let* ((token (conllu--line->token (thing-at-point 'line t)))
         (h (conllu-token-head token)))
    (cond
     ((conllu--meta-token-p token)
      (user-error "%s" "Error: meta token has no HEAD"))
     ((equal h nil)
      (user-error "%s" "Error: token has no head"))
     ((equal h 0)
      (user-error "%s" "Error: ROOT")))
    (conllu--move-to-head h)))

(defun conllu--move-to-head (head)
  "Decide if token HEAD is forward or backward and move point there."
  (conllu--barf-if-not-at-token-line)
  (let ((token (conllu--line->maybe-token (thing-at-point 'line t))))
    (unless token
      (user-error "%s" "Error: malformed token line"))
    (let ((id (conllu-token-id token)))
      (cond
       ((conllu--id> id head)
        (progn (forward-line -1)
               (conllu--move-to-head head)))
       ((conllu--id> head id)
        (progn (forward-line 1)
               (conllu--move-to-head head)))
       (t (beginning-of-line))))))
;>

;;;
;; sentence
(defun conllu-forward-to-token-line ()
  "Move to next token line."
  (conllu--move-to-token-line 1))

(defun conllu-backward-to-token-line ()
  "Move to previous token line."
  (conllu--move-to-token-line -1))

(defun conllu--move-to-token-line (n)
  "Move to a token line.
Argument N is either 1 or -1, specifying which direction to go."
  (when (conllu--not-looking-at-token)
    (forward-line n)
    (conllu--move-to-token-line n)))

(defun conllu-forward-sentence ()
  "Jump to end of sentence, which in CoNLL-U files is actually the next blank line."
  (interactive)
  (forward-sentence)
  (forward-line))

(defun conllu-next-sentence ()
  "Unalign sentence at point, jump to next sentence and align it."
  (interactive)
  (conllu-unalign-fields
   (conllu--sentence-begin-point)
   (conllu--sentence-end-point))
  (conllu-forward-sentence)
  (conllu-forward-to-token-line)
  (conllu-align-fields
   (conllu--sentence-begin-point)
   (conllu--sentence-end-point)))

(defun conllu-previous-sentence ()
  "Unalign sentence at point, jump to next sentence and align
it."
  (interactive)
  (conllu-unalign-fields
   (conllu--sentence-begin-point)
   (conllu--sentence-end-point))
  (backward-sentence)
  (conllu-backward-to-token-line)
  (conllu-align-fields
   (conllu--sentence-begin-point)
   (conllu--sentence-end-point)))

(provide 'conllu-move)

;;; conllu-move.el ends here
