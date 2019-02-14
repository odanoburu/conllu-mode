;;; conllu-move.el --- movement code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.5.0
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (flycheck "30") (hydra "0.13.0") (s "1.0"))
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

;;; Code:

(require 'conllu-parse)
(require 'conllu-thing)

(eval-when-compile
  (require 'cl-lib))

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
  (conllu--barf-unless-at-token-line)
  (beginning-of-line)
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
  (conllu--barf-unless-at-token-line)
  (beginning-of-line)
  (let* ((token (conllu--line->token (thing-at-point 'line t)))
         (h (conllu-token-head token)))
    (cond
     ((conllu--meta-token-p token)
      (user-error "Meta token has no HEAD"))
     ((equal h nil)
      (user-error "Token has no HEAD"))
     ((equal h 0)
      (user-error "ROOT node has no HEAD")))
    (conllu--move-to-head h)))

(defun conllu--move-to-head (head)
  "Decide if token HEAD is forward or backward and move point there."
  (conllu--barf-unless-at-token-line)
  (let ((token (conllu--line->maybe-token (thing-at-point 'line t))))
    (unless token
      (user-error "Malformed token line at point"))
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

(defun conllu--forward-sentence (n)
  "Jump to end of sentence.
With negative N, move backward to start of sentence."
  (interactive)
  (forward-sentence n)
  (forward-char n))

(defun conllu--next-sentence (n)
  "Jump to next sentence.
If sentence was aligned, unalign it and align the next sentence.
With negative N, move backward."
  (conllu--with-sentence-alignment
     (conllu--forward-sentence n)
     (conllu--move-to-token-line n)))

(defun conllu-next-sentence ()
  "Jump to next sentence.
If previous sentence was aligned, unalign it and align the next
sentence."
  (interactive)
  (conllu--next-sentence 1))

(defun conllu-previous-sentence ()
  "Jump to previous sentence.
If sentence was aligned, unalign it and align the previous
sentence."
  (interactive)
  (conllu--next-sentence -1))

;;; points

(defun conllu--field-points ()
  "Return points that delimit the field at point."
  (save-excursion
    (let ((start (progn (conllu--skip-backward-to-end-of-field)
                        (point)))
          (end (progn (conllu--skip-forward-to-end-of-field)
                      (point))))
      (list start end))))

(defun conllu--sentence-begin-point ()
  "Return point of the beginning of current sentence."
  (save-excursion (backward-sentence) (point)))

(defun conllu--sentence-tokens-begin-point ()
  "Return point of the beginning of the first token line."
  (save-excursion (backward-sentence)
                  (conllu-forward-to-token-line)
                  (point)))

(defun conllu--sentence-end-point ()
  "Return point of the end of current sentence."
  (save-excursion (forward-sentence) (point)))

(defun conllu--sentence-points ()
  "Return points that delimit current sentence in a list."
  (cl-labels
      ;; can't use (forward-sentence) because that only works in
      ;; conllu mode
      ((to-empty-point (n)
                       (save-excursion
                         (forward-line n)
                         (forward-line 0)
                         (while (not (or (conllu--looking-at-empty-line)
                                         (bobp)
                                         (eobp)))
                           (forward-line n))
                         (point))))
    ;; can't use conllu--sentence-begin-point because they save
    ;; excursion
    (let ((bp (to-empty-point -1))
          (ep (to-empty-point 1)))
      (list bp ep))))

(defun conllu--sentence-tokens-points ()
  "Return points that delimit the token lines of the sentence at point."
  (save-excursion
    (let ((bp (progn (backward-sentence)
                     (conllu-forward-to-token-line)
                     (point)))
          (ep (progn (forward-sentence)
                     (point))))
      (list bp ep))))


(provide 'conllu-move)

;;; conllu-move.el ends here
