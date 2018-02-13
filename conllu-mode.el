;;; conllu-mode.el --- editing mode for CoNLL-U files  -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.3
;; Package-Requires: ((emacs "25") (parsec "0.1") (cl-lib "0.5"))
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
(require 'conllu-align)
(require 'conllu-move)
(require 'conllu-parse)

(require 'whitespace)
(require 'cl-lib)
(require 'parsec)

;;;
;; misc
(defvar conllu-tab-width 2
  "Width of a tab in ‘conllu-mode’.")

;;;
;; keymap
(defvar conllu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'conllu-align-fields)
    (define-key map [(control ?c) (control ?u)] 'conllu-unalign-fields)
    (define-key map [(control ?c) (control ?h)] 'conllu-move-to-head)
    (define-key map [(control ?c) ?f] 'conllu-field-forward)
    (define-key map [(control ?c) ?b] 'conllu-field-backward)
    (define-key map [(meta ?e)] 'conllu-forward-sentence)
    (define-key map [(meta ?n)] 'conllu-next-sentence)
    (define-key map [(meta ?p)] 'conllu-previous-sentence)
    map)
  "Keymap for ‘conllu-mode’.")

;;;
;; syntax table
(defvar conllu-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st) ; _ is (part of) word, not whitespace
    (modify-syntax-entry ?, "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?# "<" st) ; begins comment
    (modify-syntax-entry ?\n ">" st) ; ends comment
    st)
  "Syntax table for ‘conllu-mode’.")

;;;
;; fonts
(defvar conllu-keywords
  '("ADJ" "ADP" "ADV" "AUX" "CCONJ" "DET" "INTJ" "NOUN" "NUM" "PART" "PRON" "PROPN" "PUNCT" "SCONJ" "SYM" "VERB" "X")
  "List possible upostag values.")

(defvar conllu-constants
  '("acl" "advcl" "advmod" "amod" "appos" "aux" "case" "cc" "ccomp" "clf" "compound" "conj" "cop" "csubj" "dep" "det" "discourse" "dislocated" "expl" "fixed" "flat" "goeswith" "iobj" "list" "mark" "nmod" "nsubj" "nummod" "obj" "obl" "orphan" "parataxis" "punct" "reparandum" "root" "vocative" "xcomp")
  "List possible deprel values.")

(defvar conllu-font-lock-defaults
  `((( ,(regexp-opt conllu-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt conllu-constants 'words) . font-lock-constant-face)))
  "Default font locks for ‘conllu-mode’.")

;;;
;; derive mode
(define-derived-mode conllu-mode fundamental-mode "CoNLL-U"
  "conllu-mode is a major mode for editing CoNLL-U files."
  :syntax-table conllu-mode-syntax-table
  (setq-local font-lock-defaults conllu-font-lock-defaults)
  (setq-local indent-tabs-mode t) ;; use tabs for indentation
  (when conllu-tab-width ;; allow user to change tab width
    (setq-local tab-width conllu-tab-width))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to
                                  ;; jump
  (setq-local truncate-lines t)
  (setq-local whitespace-style '(face tabs newline newline-mark tab-mark))
  (whitespace-mode))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conllu\\'" . conllu-mode))

(provide 'conllu-mode)

;;; conllu-mode.el ends here
