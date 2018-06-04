;;; conllu-flycheck.el --- flycheck for CoNLL-U files  -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.1.1
;; Package-Requires: ((emacs "25") (parsec "0.1") (cl-lib "0.5") (flycheck "0.25"))
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
;; this module provides a flychecker for CoNLL-U files.

;;; Code:

(require 'flycheck)

(defcustom conllu-flycheck? nil
  "Non-nil if CoNLL-U files should be flychecked.
You must have both flycheck and the checker program installed for this to work."
  :type 'boolean
  :group 'conllu)

(flycheck-define-command-checker 'hs-conllu
  "A CoNLL-U syntax checker using the hs-conllu tool.

   See URL `https://www.github.com/odanoburu/hs-conllu'."
  :command '("hs-conllu" "validate" source)
  :error-patterns
  '((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes 'conllu-mode)

(add-to-list 'flycheck-checkers 'hs-conllu)

(provide 'conllu-flycheck)

;;; conllu-flycheck.el ends here
