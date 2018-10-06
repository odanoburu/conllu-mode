;;; conllu-edit.el --- editing CoNLL-U files  -*- lexical-binding: t; -*-
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
(eval-when-compile (require 'cl-lib))
(require 'conllu-move)
(require 'conllu-thing)

(defun conllu--clear-field ()
  "Extract text from field at point and prompt for its replacement.
If REPLACE is non-nil, display the original text as default
string in the minibuffer, else display the empty field as default string."
  (save-excursion (beginning-of-line) (conllu--barf-if-not-at-token-line))
  (let* ((start (progn (conllu--skip-backward-to-end-of-field)
                       (point)))
         (end (progn (conllu--skip-forward-to-end-of-field)
                     (point))))
    (delete-and-extract-region start end)))

(defun conllu-clear-field ()
  "Empty the field at point."
  (interactive)
  (conllu--clear-field)
  (insert "_"))

(defun conllu-edit-field ()
  "Interactively edit the field at point."
  (interactive)
  (let ((original-str (conllu--clear-field)))
    (minibuffer-with-setup-hook (lambda () (insert original-str))
      (call-interactively #'conllu--prompt-for-field-string))))

(defun conllu--prompt-for-field-string (str)
  "Prompt for string in the minibuffer and insert it at point."
  (interactive "sString to place in current field:")
  (insert str))

(defun conllu-insert-token-line (&optional n)
  "Insert empty token line at point if at beginning of line.
Else do it in the next line. If called with a prefix argument, insert N lines."
  (interactive "p")
  (unless (bolp)
    (forward-line))
  (dotimes (_ n)
    (insert "_\t_\t_\t_\t_\t_\t_\t_\t_\t_\n")))

(defun conllu-merge-sentences ()
  "Merge the sentence at point with the next one.
Manual adjustment of metadata is needed.";;todo: offsets deps field too
  (interactive)
  (let* ((s1-ps (prog1 (conllu--sentence-points)
                  (backward-sentence)
                  (forward-sentence)
                  (forward-sentence)))
         (s2-ps (conllu--sentence-points))

         (s1-str (apply #'buffer-substring-no-properties s1-ps))
         (s1 (conllu--string->sent s1-str))
         (s2-str (apply #'buffer-substring-no-properties s2-ps))
         (s2 (conllu--string->sent s2-str))
         (index-last-tk-s1 (conllu--id->index (conllu-token-id (car (last (conllu-sent-tokens s1))))))
         (s2- (conllu--map-sent-tokens (apply-partially #'conllu--offset-indices index-last-tk-s1) s2))
         (s (conllu--sent-make (append (conllu-sent-comments s1)
                                       (conllu-sent-comments s2-))
                               (append (conllu-sent-tokens s1)
                                       (conllu-sent-tokens s2-)))))
    (delete-region (car s1-ps) (cadr s2-ps))
    (insert (conllu--sent->string s))))

(defun conllu--offset-indices (inc tk)
  "Offset the TK's id and head fields by n.";todo: should offset deps too.
  (let ((tk- tk))
    (cl-labels ((offset-word (n) (+ n inc))
                (offset-empty (n n2) `(multi ,(+ n inc) ,(+ n2 inc)))
                (offset-multi (n n2) `(empty ,(+ n inc) ,n2))
                (offset-index (id) (conllu--do-token-id id
                                                        #'offset-word
                                                        #'offset-empty
                                                        #'offset-multi)))
      (setf (conllu-token-id tk-) (funcall #'offset-index (conllu-token-id tk-)))
      (let ((h (conllu-token-head tk-)))
        (when h
          (setf (conllu-token-head tk-) (funcall #'offset-index h)))
        tk-))))

(provide 'conllu-edit)

;;; conllu-edit.el ends here
