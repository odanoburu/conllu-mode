;;; conllu-edit.el --- editing CoNLL-U files  -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.4.1
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

;;; code:

;;;
;; dependencies
(eval-when-compile (require 'cl-lib))
(require 'flycheck)
(require 'hydra)
(require 's)
(require 'conllu-align)
(require 'conllu-move)
(require 'conllu-thing)

(defun conllu--extract-field ()
  "Extract text from field at point and return it.
Assumes point is at token line."
  (let* ((start (progn (conllu--skip-backward-to-end-of-field)
                       (point)))
         (end (progn (conllu--skip-forward-to-end-of-field)
                     (point))))
    (delete-and-extract-region start end)))

(defun conllu-clear-field ()
  "Empty the field at point."
  (interactive)
  (conllu--barf-unless-at-token-line)
  (conllu--extract-field)
  (insert "_")
  (conllu--sentence-realign-if-aligned))

(defun conllu--edit-field ()
  "Edit the field at point in the minibuffer.
Assumes point is at token line. If field value is empty ('_'),
put the empty string in the minibuffer instead of the original
string."
  (let* ((original-str (conllu--extract-field))
         (str (if (s-equals? original-str "_") "" original-str)))
    (minibuffer-with-setup-hook (lambda () (insert str))
      (call-interactively #'conllu--prompt-for-field-string))))

(defun conllu-edit-field ()
  "Interactively edit the field at point."
  (interactive)
  (conllu--barf-unless-at-token-line)
  (conllu--edit-field)
  (conllu--sentence-realign-if-aligned))

(defun conllu--prompt-for-field-string (str)
  "Prompt for string in the minibuffer and insert it at point.
If string is blank, insert the empty field ('_')."
  (interactive "sString to place in current field:")
  (if (s-blank? str)
      (insert "_")
    (insert str)))

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
          (setf (conllu-token-head tk-) (funcall #'offset-index h))))
      (let ((ds (conllu-token-deps tk-)))
        (when ds
          (setf (conllu-token-deps tk-) (mapcar (lambda (dep) (cl-list* (offset-index (car dep))
                                                                     (cdr dep)))
                                                ds)))
        tk-))))

(defun conllu--flycheck-error-display-toggler ()
  "Return function that captures current `flycheck-display-errors-function' and
toggles it on and off when called."
  (let ((original flycheck-display-errors-function))
    (lambda (to-on?)
      (cond
       ((and to-on? flycheck-mode)
        (setq-local flycheck-display-errors-function nil))
       (flycheck-mode (setq-local flycheck-display-errors-function original))))))

(defun conllu--flycheck-display-errors-at-point-in-hydra ()
  (or (when flycheck-mode
        (let ((errors (flycheck-overlay-errors-at (point))))
          (when errors
            (s-concat "Flycheck errors:\n"
                      (s-join "\n" (mapcar #'flycheck-error-format-message-and-id errors))))))
      ""))


(cl-labels ((move-tok-and-edit (n)
                               (forward-line n)
                               (when (conllu--not-looking-at-token)
                                 (forward-line (- n))
                                 (conllu--next-sentence n)))
            (move-sent-and-edit (n)
                                (conllu--next-sentence n))
            (move-and-edit-field (n)
                                 (conllu--move-to-field-number n)
                                 (conllu--edit-field))
            (if-flycheck-next-error (n)
                                    (if flycheck-mode
                                        (flycheck-next-error n)
                                      (user-error "Flycheck not setup."))))
  (let ((error-display-toggler (conllu--flycheck-error-display-toggler)))
    (defhydra conllu-edit-hydra (:pre
                                 (progn
                                   (conllu--barf-unless-at-token-line)
                                   (funcall error-display-toggler t))
                                 :post
                                 (progn
                                   (conllu--sentence-realign-if-aligned)
                                   (funcall error-display-toggler nil)))
      "
^ ^  Navigate    |     Edit
----------------------------------------
_↑_: prev tok    |  _1_: ID     %s(nth 0 (conllu--line->fields (thing-at-point 'line t)))
_↓_: next tok    |  _2_: FORM   %s(nth 1 (conllu--line->fields (thing-at-point 'line t)))
_←_: prev snt    |  _3_: LEMMA  %s(nth 2 (conllu--line->fields (thing-at-point 'line t)))
_→_: next snt    |  _4_: UPOS   %s(nth 3 (conllu--line->fields (thing-at-point 'line t)))
^ ^              |  _5_: XPOS   %s(nth 4 (conllu--line->fields (thing-at-point 'line t)))
_<_: next error  |  _6_: FEATS  %s(nth 5 (conllu--line->fields (thing-at-point 'line t)))
_>_: prev error  |  _7_: HEAD   %s(nth 6 (conllu--line->fields (thing-at-point 'line t)))
^ ^              |  _8_: DEPREL %s(nth 7 (conllu--line->fields (thing-at-point 'line t)))
_q_: quit        |  _9_: DEPS   %s(nth 8 (conllu--line->fields (thing-at-point 'line t)))
^ ^              |  _0_: MISC   %s(nth 9 (conllu--line->fields (thing-at-point 'line t)))
%s(conllu--flycheck-display-errors-at-point-in-hydra)"
      ;; nav commands
      ("<up>"    (move-tok-and-edit -1)  nil)
      ("<down>"  (move-tok-and-edit 1)   nil)
      ("<left>"  (move-sent-and-edit -1) nil)
      ("<right>" (move-sent-and-edit 1)  nil)
      ;; flycheck error navigation
      ("<" (if-flycheck-next-error -1) nil)
      (">" (if-flycheck-next-error 1) nil)
      ;; editing commands
      ("1" (move-and-edit-field 1)  nil)
      ("2" (move-and-edit-field 2)  nil)
      ("3" (move-and-edit-field 3)  nil)
      ("4" (move-and-edit-field 4)  nil)
      ("5" (move-and-edit-field 5)  nil)
      ("6" (move-and-edit-field 6)  nil)
      ("7" (move-and-edit-field 7)  nil)
      ("8" (move-and-edit-field 8)  nil)
      ("9" (move-and-edit-field 9)  nil)
      ("0" (move-and-edit-field 10) nil)
      ;; exit
      ("q" nil nil))))

(provide 'conllu-edit)

;;; conllu-edit.el ends here
