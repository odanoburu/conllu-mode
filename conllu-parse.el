;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.1.1
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

(eval-when-compile
  (require 'cl-lib))

(require 's)

(cl-defstruct conllu-token (id form lemma upos xpos feats head deprel deps misc))

(defun conllu-make-id (id)
  "Turn ID string into list representation."
  (pcase (s-slice-at "[\.-]" id)
    (`(,n) (string-to-int n))
    (`(n sep-n2)
     (let ((sep (substring sep-n2 0 1))
           (n2 (substring sep-n2 1)))
       (pcase
           ("." (list 'empty n n2))
           ("-" (list 'multi n n2)))))
    (_ (user-error "Error: invalid CoNLL-U ID %s" id))))

(defalias 'conllu--make-head 'conllu-make-id "Turn ID into list representation")

(defun conllu--make-token (id fo le up xp fe he dr ds m)
  "Turn CoNLL-U line field strings into a token."
  (conllu-make-token (conllu--make-id id) fo le up xp fe
                     (conllu--make-head he) dr ds m))

(defun conllu--line->fields (line)
  "Split a string into a list of field strings at TAB separator."
  (mapcar s-trim (s-split "\t" line)))

(defun conllu--line->token (line)
  "Turn CoNLL-U line string into a token."
  (let ((raw-fields (conllu--line->fields line)))
    (pcase raw-fields
      (`(,id ,fo ,le ,up ,xp ,fe ,he ,dr ,ds ,m)
       (conllu-make-token id fo le up xp fe he dr ds m))
      (_ (user-error "Error: wrong number of fields in token line. %s should have 9 tabs.")))))

(provide 'conllu-parse)

;;; conllu-parse.el ends here
