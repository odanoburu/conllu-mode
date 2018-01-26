;;; conllu-mode.el --- conllu-mode for emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bruno cuconato

;; Author:  <>
;; Maintainer: 
;; URL: https://github.com/
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
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


;; https://www.emacswiki.org/emacs/SampleMode
;; https://www.emacswiki.org/emacs/DerivedMode
;; https://www.emacswiki.org/emacs/ModeTutorial
;; ftp://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_chapter/elisp_23.html

;;; code:

;;;
;; misc
(defvar conllu-tab-width 4 "Width of a tab for CoNLL-U mode")

;;;
;; keymap
(defvar conllu-mode-map
  ;; bogus function, as there's no need for keymaps yet
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-a" 'electric-newline-and-maybe-indent)
    map)
  "Keymap for conllu major mode")

;;;
;; syntax table
(defvar conllu-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st) ; _ is (part of) word, not whitespace
    (modify-syntax-entry ?# "<" st) ; begins comment
    (modify-syntax-entry ?\n ">" st) ; ends comment
    st)
  "Syntax table for conllu-mode")

;;;
;; fonts
(defvar conllu-keywords
  '("ADJ" "ADP" "ADV" "AUX" "CCONJ" "DET" "INTJ" "NOUN" "NUM" "PART" "PRON" "PROPN" "PUNCT" "SCONJ" "SYM" "VERB" "X"))

(defvar conllu-constants
  '("acl" "advcl" "advmod" "amod" "appos" "aux" "case" "cc" "ccomp" "clf" "compound" "conj" "cop" "csubj" "dep" "det" "discourse" "dislocated" "expl" "fixed" "flat" "goeswith" "iobj" "list" "mark" "nmod" "nsubj" "nummod" "obj" "obl" "orphan" "parataxis" "punct" "reparandum" "root" "vocative" "xcomp"))

(defvar conllu-font-lock-defaults
  `((( ,(regexp-opt conllu-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt conllu-constants 'words) . font-lock-constant-face))))

;;;
;; derive mode
(define-derived-mode conllu-mode fundamental-mode "CoNLL-U"
  "CoNLL-U mode is a major mode for editing CoNLL-U files"
  :syntax-table conllu-mode-syntax-table
  (setq-local font-lock-defaults conllu-font-lock-defaults)
  ;;(font-lock-add-keywords nil conllu-symbols) ;; highlight tabs
  (setq-local indent-tabs-mode t) ;; use tabs for indentation
  (when conllu-tab-width ;; allow user to change tab width
    (setq-local tab-width conllu-tab-width))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local truncate-lines t)
  (setq-local whitespace-style '(face tabs newline tab-mark newline-mark))
  (setq-local whitespace-display-mappings '((newline-mark ?\n [?$ ?\n]) (tab-mark ?\t [?§ ?\t]))) ;; original is \u00BB
  (whitespace-mode))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conllu\\'" . conllu-mode))

(provide 'conllu-mode)
