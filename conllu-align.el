;;; conllu-align.el --- aligning code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.2.1
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (s "1.0") (flycheck "30"))
;; Keywords: extensions
;; Note: this code is a simplified version of the one in csv-mode.el.

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

(require 'conllu-thing)
(require 'conllu-move)

;;; Code:

(defgroup conllu-align-group
  nil
  "Group for conllu-align.el customizations."
  :group 'data)

(defcustom conllu-align-padding 1
  "Aligned field spacing: must be a positive integer.
Number of spaces used by `conllu--align-fields' after
separators."
  :type 'integer
  :group 'conllu-align-group)

(defcustom conllu-align-style 'left
  "Aligned field style: one of `left', `centre', `right' or `auto'.
Alignment style used by `conllu-align-fields'.  Auto-alignment
means left align text and right align numbers."
  :type '(choice (const left) (const centre)
                 (const right) (const auto))
  :group 'conllu-align-group)

(defun conllu--column-widths ()
  (let ((widths '()))
    ;; Construct list of column widths:
    (while (not (eobp))                   ; for each token...
      (or (conllu--not-looking-at-token)
          (let ((w widths)
                (col (current-column))
                x)
            (while (not (eolp))
              (conllu--skip-forward-to-end-of-field)
              (setq x (- (current-column) col)) ; Field width.
              (if w
                  (if (> x (car w)) (setcar w x))
                (setq w (list x)
                      widths (nconc widths w)))
              (or (eolp) (forward-char))  ; Skip separator.
              (setq w (cdr w) col (current-column)))))
      (forward-line))
    widths))

(defun conllu--make-overlay (beg end &optional buffer front-advance rear-advance props)
  (let ((o (make-overlay beg end buffer front-advance rear-advance)))
    (overlay-put o 'conllu t)
    (while props
      (overlay-put o (pop props) (pop props)))
    o))

(defun conllu--delete-overlay (o)
  (and (overlay-get o 'conllu) (delete-overlay o)))

(defun conllu-align-fields (beg end)
  "Align fields in the current region.
BEG and END must be point values."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (conllu--sentence-points))) ; if interactive, by default
                                     ; align sentence
  (setq end (copy-marker end))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (set-marker end nil)
      (goto-char (point-min))
      (let ((widths (conllu--column-widths)))

        ;; Align fields:
        (goto-char (point-min))
        (while (not (eobp))             ; for each token...
          (unless (conllu--not-looking-at-token)
            (let ((w widths))
              (while (and w (not (eolp)))
                (let* ((beg (point))
                       (align-padding (if (bolp) 0
                                        conllu-align-padding))
                       (left-padding 0)
                       (right-padding 0)
                       (field-width
                        (- (- (current-column)
                              (progn (conllu--skip-forward-to-end-of-field) (current-column)))))
                       (column-width (pop w))
                       (x (- column-width field-width))) ; Required padding.
                  (set-marker end (point)) ; End of current field.
                  ;; beg = beginning of current field
                  ;; end = (point) = end of current field

                  ;; Compute required padding:
                  (cond
                   ((eq conllu-align-style 'left)
                    ;; Left align -- pad on the right:
                    (setq left-padding align-padding
                          right-padding x))
                   ((eq conllu-align-style 'right)
                    ;; Right align -- pad on the left:
                    (setq left-padding (+ align-padding x)))
                   ((eq conllu-align-style 'auto)
                    ;; Auto align -- left align text, right align numbers:
                    (if (string-match "\\`[-+.[:digit:]]+\\'"
                                      (buffer-substring beg (point)))
                        ;; Right align -- pad on the left:
                        (setq left-padding (+ align-padding x))
                      ;; Left align -- pad on the right:
                      (setq left-padding align-padding
                            right-padding x)))
                   ((eq conllu-align-style 'centre)
                    ;; Centre -- pad on both left and right:
                    (let ((y (/ x 2)))  ; truncated integer quotient
                      (setq left-padding (+ align-padding y)
                            right-padding (- x y)))))

                  ;; Do not hide separators...
                    (let ((overlay (conllu--make-overlay beg (point) nil nil t)))
                      (when (> left-padding 0) ; Pad on the left.
                        ;; Display spaces before field:
                        (overlay-put overlay 'before-string
                                     (make-string left-padding ?\ )))
                      (unless (eolp)
                        (if (> right-padding 0) ; Pad on the right.
                            ;; Display spaces after field:
                            (overlay-put
                             overlay
                             'after-string (make-string right-padding ?\ )))
                        (forward-char))))))) ;; skip separator
          (forward-line)))))
  (set-marker end nil))

(defun conllu-unalign-fields (beg end)
  "Unalign fields in the current region.
BEG and END must be point values."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (conllu--sentence-points)))
  ;; Remove any soft alignment:
  (mapc #'conllu--delete-overlay (overlays-in beg end))
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(display))))

(provide 'conllu-align)

;;; conllu-align.el ends here
