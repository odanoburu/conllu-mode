;;; conllu-flycheck.el --- flycheck for  files -*- lexical-binding: t; -*-
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
(require 's)
(require 'rx)
(require 'flycheck)


(defgroup conllu-flycheck nil
  "Support for flychecking CoNLL-U buffers."
  :group 'conllu)

(defcustom conllu-flycheck-lang
  nil
  "2-character language code corresponding to the language of the current buffer.
Meant to be bound as a file- or direcory-local variable"
  :type '(choice string nil)
  :group 'conllu-flycheck)

(defcustom conllu-flycheck-default-lang
  "ud"
  "2-character language code to default to.
See ‘conllu--flycheck-lang’."
  :type 'string
  :group 'conllu-flycheck)


;; TODO: highlight only the column (is this desirable? what if the
;; column is out of view because of truncate lines?)
(flycheck-define-checker conllu-validate-python3
  "A CoNLL-U syntax checker using the validate python 3 script.

If you don't have the script you should obtain it from URL
`www.github.com/universaldependencies/tools' and then add it to
your PATH variable."
  :command ("validate.py"
            "--lang" (eval conllu-flycheck-lang)
            "--max-err" (eval (number-to-string conllu-flycheck-error-threshold))
            "--level" (eval conllu-flycheck-validation-level)
            source)
  :error-patterns
  ((error line-start "[Line" (one-or-more space) line (zero-or-more (not (any ?\]))) "]: "
          (message) line-end))
  :modes conllu-mode
  :predicate (lambda () (buffer-file-name)))


(defcustom conllu-flycheck-checkers
  nil
  "CoNLL-U checkers to be used by Flycheck."
  :type '(set (function-item :doc "Python validate script" conllu-validate-python3))
  :group 'conllu)


(defcustom conllu-flycheck-error-threshold
  30
  "Maximum number of errors to be shown by flycheck.

Argument passed to checker.  If this number is too high, Emacs
might slow down when displaying the errors."
  :type  'integer
  :group 'conllu)


(defcustom conllu-flycheck-validation-level
  "5"
  "Set validation level of the flycheck checker.

The value will be ignored if the checker does not support it."
  :group 'conllu
  :type '(radio (const :tag "1")
                (const :tag "2")
                (const :tag "3")
                (const :tag "4")
                (const :tag "5")))


(defcustom conllu-flycheck-on?
  'ask
  "Should flycheck-mode be turned on automatically when conllu-mode is invoked?"
  :type '(choice (const :tag "Yes" 'yes)
                 (const :tag "No" 'no)
                 (other :tag "Ask" 'ask))
  :group 'conllu)


(defun conllu--try-derive-lang-code-from-filename ()
  "Return two-letter language code from filename if available, else return nil.

UD treebanks filenames usually start with a two-letter language
code and an underline character, which is what we try to extract
here."
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (when (s-matches-p (rx string-start (group letter letter) ?\_)
                       f)
      (s-left 2 f))))


(defun conllu--read-string-matching (prompt predicate &optional default-value)
  "Read a string that matches PREDICATE from the minibuffer, prompting with PROMPT.

Loops until a satisfactory string is found.  DEFAULT-VALUE
behaves as in `read-string'"
  (cl-loop
   for x = (read-string prompt nil nil default-value)
   while (not (funcall predicate x))
   finally (return x)))


(defun conllu--flycheck-lang ()
  "Try to derive 2-character language code from filename, else ask user."
  (or conllu-flycheck-lang
      (conllu--try-derive-lang-code-from-filename)
      (conllu--read-string-matching "2-character language code or nothing for simple UD validation:"
                                    (lambda (in)
                                      (= 2 (length (s-trim in)))))
      conllu-flycheck-default-lang))

(defun conllu-flycheck ()
  "Invoke flycheck when checkers are available."
  (interactive)
  (if conllu-flycheck-checkers
      (progn
        (conllu--add-checkers)
        (setq-local conllu-flycheck-lang (conllu--flycheck-lang))
        (flycheck-mode 1))
    (user-error "No checker configured.  Customize variable `conllu-flycheck-checkers'")))


(defun conllu--invoke-flycheck-if ()
  "Invoke `flycheck-mode' if a checker is available and according to `conllu-flycheck-on?'."
  (when conllu-flycheck-checkers
    (cl-case conllu-flycheck-on?
      (yes (conllu-flycheck))
      (ask (when (y-or-n-p "Enable flycheck for this buffer?")
             (conllu-flycheck))))))


(defun conllu--flycheck-next-error (n)
  "Move to next flycheck error preserving sentence alignment.

N behaves as in `flycheck-next-error'."
  (conllu--with-sentence-alignment
   (flycheck-next-error n)))


(defun conllu--add-checkers ()
  "Add conllu checkers in `conllu-flycheck-checkers' to `flycheck-checkers'."
  (mapcar (lambda (checker)
            (add-to-list 'flycheck-checkers checker))
          conllu-flycheck-checkers))

(provide 'conllu-flycheck)

;;; conllu-flycheck.el ends here
