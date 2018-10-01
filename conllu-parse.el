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

(cl-defstruct (conllu-token (:constructor nil)
                            (:constructor conllu--token-create (id form lemma upos xpos feats head deprel deps misc))
                            (:copier nil))
  id form lemma upos xpos feats head deprel deps misc)

;;; all fields are strings, except for ID and HEAD, which are either
;;; integers or lists (nil if empty)
(defun conllu--make-token (id fo le up xp fe he dr ds m)
  "Turn CoNLL-U line field strings into a token."
  (conllu--token-create (conllu--make-id id) fo le up xp fe
                        (conllu--make-head he) dr ds m))

(defun conllu--make-id (id)
  "Turn ID string into integer or list representation.
 IDs of regular tokens are integers, and those of meta tokens are
 represented as lists of three elements: the first element is the
 kind of meta-token ('empty or 'multi), and the rest are
 integers."
  (pcase (s-slice-at "[\.-]" id)
    (`(,n) (string-to-number n))
    (`(,n ,sep-n2)
     (let ((sep (substring sep-n2 0 1))
           (n2 (substring sep-n2 1)))
       (pcase sep
           ("." (list 'empty n n2))
           ("-" (list 'multi n n2)))))
    (_ (user-error "Error: invalid CoNLL-U ID %s" id))))

(defun conllu--meta-token-p (tk)
  "Return t if TK is a meta CoNLL-U token (either a multiword token or an empty token."
  (consp (conllu-token-id tk)))

(defun conllu--make-head (h)
  "Turn H into list representation, or nil if head field is empty."
  (if (string-equal h "_")
      nil
    (conllu--make-id h)))

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

(provide 'conllu-parse)

;;; conllu-parse.el ends here
