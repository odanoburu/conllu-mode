;;; conllu-flycheck.el --- flycheck for  files -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.4.2
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

(defun conllu--set-valid-path-variable (sym val)
  (when val
    (let ((fp (file-truename val)))
      (if (file-exists-p fp)
          (set-default sym fp)
        (user-error "File ~s does not exist" fp)))))

(defcustom conllu-flycheck-validate-python2-path
  nil
  "Set the path to the validate python 2 script."
  :type '(choice file (const nil))
  :group 'conllu
  :set #'conllu--set-valid-path-variable)

(defcustom conllu-flycheck-validate-python3-path
  nil
  "Set the path to the validate python 3 script."
  :type '(choice file (const nil))
  :group 'conllu
  :set #'conllu--set-valid-path-variable)

(defcustom conllu-flycheck-error-threshold
  30
  "Maximum number of errors to be shown.

Argument passed to validate script. If this number is too high,
emacs might slow down when displaying the errors."
  :type 'integer
  :group 'conllu)

(defcustom conllu-flycheck-validation-level
  "5"
  "Set validation level of the validate script."
  :type 'string
  :group 'conllu
  :set (lambda (sym val)
         (if (member val '("1" "2" "3" "4" "5"))
             (set-default sym val)
           (user-error "~s: not a valid validation level. Must be integer between 1-5."))))

(defun conllu-invoke-flycheck-if-checker-available ()
  "Invoke `flycheck-mode' if a checker is available.

You must set `conllu-flycheck-validate-python2-path' or
`conllu-flycheck-validate-python3-path' are to a non-nil value."
  (when (or conllu-flycheck-validate-python3-path
            conllu-flycheck-validate-python2-path)
    (flycheck-mode)))

;; TODO: is it better to ask lang whenever a new file is opened?
(defun conllu--derive-lang-code-from-filename ()
  "Return two-letter language code from filename if available, else return nil.

UD treebanks filenames usually start with a two-letter language
code and an underline character, which is what we try to extract
here. If not available, we return 'ud', which is the code for
unspecified language."
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (if (s-matches-p (rx string-start (group letter letter) ?\_)
                     f)
        (s-left 2 f)
      "ud")))

;; TODO: highlight only the column (is this desirable? what if the
;; column is out of view because of truncate lines?)
(flycheck-define-checker conllu-validate-python2
  "A CoNLL-U syntax checker using the validate python 2 script.

If you don't have the script you should obtain it from URL
`www.github.com/universaldependencies/tools' and then customize
`conllu-flycheck-validate-python2-path' with its path."
  :command ("python2" (eval conllu-flycheck-validate-python2-path)
            "--lang" (eval (conllu--derive-lang-code-from-filename))
            "--max-err" (eval (number-to-string conllu-flycheck-error-threshold))
            source)
  :error-patterns
  ((error line-start "[Line" (one-or-more space) line "]: " (message) line-end))
  :modes conllu-mode
  :predicate (lambda () (buffer-file-name)))

(flycheck-define-checker conllu-validate-python3
  "A CoNLL-U syntax checker using the validate python 3 script.

If you don't have the script you should obtain it from URL
`www.github.com/universaldependencies/tools' and then customize
`conllu-flycheck-validate-python3-path' with its path."
  :command ("python3" (eval conllu-flycheck-validate-python3-path)
            "--lang" (eval (conllu--derive-lang-code-from-filename))
            "--max-err" (eval (number-to-string conllu-flycheck-error-threshold))
            "--level" (eval conllu-flycheck-validation-level)
            source)
  :error-patterns
  ((error line-start "[Line" (one-or-more space) line (zero-or-more (not (any ?\]))) "]: " (message) line-end))
  :modes conllu-mode
  :predicate (lambda () (buffer-file-name)))

(add-to-list 'flycheck-checkers 'conllu-validate-python3)

(provide 'conllu-flycheck)

;;; conllu-flycheck.el ends here
