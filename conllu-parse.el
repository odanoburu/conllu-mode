;;; conllu-parse.el --- parse code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.2.0
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (s "1.0") (flycheck "30"))
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

;;; dependencies
(require 'conllu-thing)

(require 's)


;;; parse token line
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

;;; parse sent
(defun conllu--string->sent (sent)
  "Turn a well-formed CoNLL-U SENT string into a sentence."
  (let* ((ls (s-split "\n" sent t))
         (ls-by (seq-group-by #'conllu--comment-line? ls))
         (cs (cdr (assoc t ls-by)))
         (tks (mapcar #'conllu--line->token (cdr (assoc nil ls-by)))))
    (conllu--sent-make cs tks)))

;;; print token
(defun conllu--token->string (token)
  "Print CoNLL-U TOKEN to a string."
  (s-join "\t" (list
                (conllu--token-id->string (conllu-token-id token))
                (conllu-token-form token)
                (conllu-token-lemma token)
                (conllu-token-upos token)
                (conllu-token-xpos token)
                (conllu-token-feats token)
                (conllu--token-head->string (conllu-token-head token))
                (conllu-token-deprel token)
                (conllu--token-deps->string (conllu-token-deps token))
                (conllu-token-misc token))))

;;; print sent
(defun conllu--sent->string (sent)
  "Print CoNLL-U SENT to a string."
  (s-join "\n" (append
                (conllu-sent-comments sent)
                (mapcar #'conllu--token->string (conllu-sent-tokens sent)))))

(provide 'conllu-parse)

;;; conllu-parse.el ends here
