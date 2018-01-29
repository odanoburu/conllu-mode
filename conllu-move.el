;;; conllu-move.el --- movement code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (whitespace "13") (parsec))
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

(require 'conllu-align)

;;;
;; sentence
(defun conllu-forward-to-token-line ()
  (conllu-move-to-token-line 1))

(defun conllu-backward-to-token-line ()
  (conllu-move-to-token-line -1))

(defun conllu-move-to-token-line (n)
  (when (conllu-not-looking-at-token)
    (forward-line n)
    (conllu-move-to-token-line n)))

(defun conllu-forward-sentence ()
  "jump to end of sentence, which in CoNLL-U files is actually
the next blank line."
  (interactive)
  (forward-sentence)
  (forward-line))

(defun conllu-next-sentence ()
  "unalign sentence at point, jump to next sentence and align it."
  (interactive)
  (conllu-unalign-fields (sentence-begin-point) (sentence-end-point))
  (conllu-forward-sentence)
  (conllu-forward-to-token-line)
  (conllu-align-fields (sentence-begin-point) (sentence-end-point)))

(defun conllu-previous-sentence ()
  "unalign sentence at point, jump to next sentence and align it."
  (interactive)
  (conllu-unalign-fields (sentence-begin-point) (sentence-end-point))
  (backward-sentence)
  (conllu-backward-to-token-line)
  (conllu-align-fields (sentence-begin-point) (sentence-end-point)))

;;;
;; token
;(defun conllu-move-to-head)

(provide 'conllu-move)
