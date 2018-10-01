;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
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

;;; Code:
(require 'cl-lib)
(require 's)

(cl-defstruct (conllu-sent (:constructor nil)
                           (:constructor conllu--make-sent (comments tokens))
                           (:copier nil))
  comments tokens)

(cl-defstruct (conllu-token (:constructor nil)
                            (:constructor conllu--token-create (id form lemma upos xpos feats head deprel deps misc))
                            (:copier nil))
  id form lemma upos xpos feats head deprel deps misc)

;;; all fields are strings, except for ID and HEAD, which are either
;;; integers or lists (nil if empty)
(defun conllu--make-token (id fo le up xp fe he dr ds m)
  "Turn CoNLL-U line field strings into a token."
  (conllu--token-create (conllu--string->token-id id) fo le up xp fe
                        (conllu--make-head he) dr ds m))

(defun conllu--string->token-id (id)
  "Turn ID string into integer or list representation.
 IDs of regular tokens are integers, and those of meta tokens are
 represented as lists of three elements: the first element is the
 kind of meta-token ('empty or 'multi), and the rest are
 integers."
  (pcase (s-slice-at "[\.-]" id)
    (`(,n) (string-to-number n))
    (`(,n ,sep-n2)
     (let ((n (string-to-number n))
           (sep (substring sep-n2 0 1))
           (n2 (string-to-number (substring sep-n2 1))))
       (pcase sep
           ("." (list 'empty n n2))
           ("-" (list 'multi n n2)))))
    (_ (user-error "Error: invalid CoNLL-U ID %s" id))))

(defun conllu--do-token-id (id w-fn e-fn m-fn)
  "Apply either W-FN E-FN or M-FN on ID, depending on its type."
  (pcase id
    (`(empty ,n ,n2) (funcall e-fn n n2))
    (`(multi ,n ,n2) (funcall m-fn n n2))
    (n (funcall w-fn n))))

(defun conllu--token-id->string (id)
  "Return the string representation of CoNLL-U ID."
  (conllu--do-token-id id
                       #'number-to-string
                       (lambda (n n2) (format "%d.%d" n n2))
                       (lambda (n n2) (format "%d-%d" n n2))))

(defun conllu--token-head->string (head)
  "Return the string representation of CoNLL-U HEAD."
  (if head
      (conllu--token-id->string head)
    "_"))

(defun conllu--meta-token-p (tk)
  "Return t if TK is a meta CoNLL-U token (either a multiword token or an empty token."
  (consp (conllu-token-id tk)))

(defun conllu--make-head (h)
  "Turn H into list representation, or nil if head field is empty."
  (if (string-equal h "_")
      nil
    (conllu--string->token-id h)))

(defun conllu--line->fields (line)
  "Split a string into a list of field strings at TAB separator."
  (mapcar #'s-trim (s-split "\t" line)))

(defun conllu--line->maybe-token (line)
  "Turn a well-formed CoNLL-U LINE string into a token, else return nil."
  (let ((raw-fields (conllu--line->fields line)))
    (pcase raw-fields
      (`(,id ,fo ,le ,up ,xp ,fe ,he ,dr ,ds ,m)
       (conllu--make-token id fo le up xp fe he dr ds m))
      (_ nil)))) ;; good to be explicit about this

(defun conllu--line->token (line)
  "Turn a well-formed CoNLL-U LINE string into a token, else report user error."
  (let ((tk (conllu--line->maybe-token line)))
    (if tk
        tk
      (user-error "%s" "Error: malformed token line"))))

(defun conllu--comment-line? (str)
  "Return t if STR is a CoNLL-U comment line."
  (string-equal "# " (seq-take (s-trim str) 2)))

(defun conllu--string->sent (sent)
  "Turn a well-formed CoNLL-U SENT string into a sentence."
  (let* ((ls (s-split "\n" sent t))
         (ls-by (seq-group-by #'conllu--comment-line? ls))
         (cs (rest (assoc t ls-by)))
         (tks (mapcar #'conllu--line->token (rest (assoc nil ls-by)))))
    (conllu--make-sent cs tks)))

(defun conllu--token->line (token)
  "Print conllu TOKEN to a string."
  (s-join "\t" (list
                (conllu--token-id->string (conllu-token-id token))
                (conllu-token-form token)
                (conllu-token-lemma token)
                (conllu-token-upos token)
                (conllu-token-xpos token)
                (conllu-token-feats token)
                (conllu--token-head->string (conllu-token-head token))
                (conllu-token-deprel token)
                (conllu-token-deps token)
                (conllu-token-misc token))))

(defun conllu--map-sent-tokens (sent f)
  "Call F on each token of SENT."
  (setf sent (conllu-sent-tokens sent) (mapcar f (conllu-sent-tokens sent)))
  sent)

(provide 'conllu-parse)

;;; conllu-parse.el ends here
