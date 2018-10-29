;;; conllu-flycheck.el --- flycheck for  files -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.2.1
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
(require 's)
(require 'rx)
(require 'flycheck)


(defcustom conllu-flycheck-validate.py-path
  nil
  "Set the path to the validate.py script."
  :type 'file
  :group 'conllu
  :set (lambda (sym val) (when val (set-default sym (file-truename val)))))

(defun conllu-invoke-flycheck-if-checker-available ()
  "Invoke `flycheck-mode' if `conllu-flycheck-validate.py-path'
is non-nil."
  (when conllu-flycheck-validate.py-path
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
(flycheck-define-checker conllu-validate.py
  "A CoNLL-U syntax checker using the validate.py script.

If you don't have the script you should obtain it from URL
`www.github.com/universaldependencies/tools' and then customize
`conllu-flycheck-validate.py-path' with its path."
  :command ("python" (eval conllu-flycheck-validate.py-path) "--lang" (eval (conllu--derive-lang-code-from-filename)) source)
  :error-patterns
  ((error line-start "[Line" (one-or-more space) line "]: " (message) line-end))
  :modes conllu-mode
  :predicate (lambda () (buffer-file-name)))

(add-to-list 'flycheck-checkers 'conllu-validate.py)

(provide 'conllu-flycheck)

;;; conllu-flycheck.el ends here
