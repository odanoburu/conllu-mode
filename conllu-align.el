;;; conllu-align.el --- aligning code for conllu-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2018 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/conllu-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (whitespace "13") (parsec) (cl-lib))
;; Keywords: extensions
;; Note: this code is a simplified version of one finds in csv-mode.el.

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

(require 'conllu-move)

(defun conllu--sentence-begin-point ()
  (save-excursion
    (backward-sentence)
    (point)))

(defun conllu--sentence-end-point ()
  (save-excursion
    (forward-sentence)
    (point)))

(defun conllu--field-end-point ()
  "doesn't save excursion"
  (conllu--skip-to-end-of-field)
  (prog1 (point) (forward-char)))

(defun conllu--sentence-points ()
  (let ((start (conllu--sentence-begin-point))
        (end (conllu--sentence-end-point)))
    (list start end)))

(defun conllu--column-widths ()
  (let ((widths '()))
    ;; Construct list of column widths:
    (while (not (eobp))                   ; for each token...
      (or (conllu--not-looking-at-token)
          (let ((w widths)
                (col (current-column))
                x)
            (while (not (eolp))
              (conllu--skip-to-end-of-field)
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
                              (progn (conllu--skip-to-end-of-field) (current-column)))))
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
                        (forward-char))) ; Skip separator.

                   ))))
          (forward-line)))))
  (set-marker end nil))

(defun conllu-unalign-fields (beg end)
  ""
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (conllu--sentence-points)))
  ;; Remove any soft alignment:
  (mapc #'conllu--delete-overlay (overlays-in beg end))
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(display))))

(defgroup conllu-align-group
  nil
  "group for conllu-align.el customizations."
  :group 'data)

(defcustom conllu-align-padding 1
  "Aligned field spacing: must be a positive integer.
Number of spaces used by `conllu--align-fields' after separators."
  :type 'integer
  :group 'conllu-align-group)

(defcustom conllu-align-style 'left
  "Aligned field style: one of `left', `centre', `right' or `auto'.
Alignment style used by `conllu-align-fields'.
Auto-alignment means left align text and right align numbers."
  :type '(choice (const left) (const centre)
                 (const right) (const auto))
  :group 'conllu-align-group)

;;;
;; hide fields

;; TODO can the invisible property be messed up by a lot of heavy
;; editing? if how so, have something reset it

;; TODO make navigation better? docs said point would automagically
;; move from invisible region, but the effect is not exactly what I
;; expected.
(defvar conllu--invisible-property-put nil
  "set to t in the current buffer when `conllu-hide-columns' is
  called the first time. once the invisible property has been put
  on each column, we simply need to toggle the invisibility spec
  for each value, instead of going all over the text again
  changing property values.")

(defun conllu--field-symbols ()
  (list 'conllu-id 'conllu-form 'conllu-lemma 'conllu-upostag
      'conllu-xpostag 'conllu-feats 'conllu-head 'conllu-deprel
      'conllu-deps 'conllu-misc))

(defun conllu--field-strings ()
  (list "id" "form" "lemma" "upostag" "xpostag" "feats" "head" "deprel" "deps" "misc"))

(defun conllu--fields-alist ()
  (mapcar* (lambda (a b) (cons a b))
           (conllu--field-strings) (conllu--field-symbols)))

(defun conllu-show-fields (string-columns)
  (interactive "sFields to show (RET for unhiding all fields):")
  (let (selected-fields)
    (if (string= string-columns "")
        (setq selected-fields (conllu--field-symbols))
      (setq selected-fields (conllu--string-to-fields string-columns)))
    (conllu-rm-from-invisibility-spec selected-fields)))

(defun conllu-hide-fields (string-columns)
  "hides the fields specified (names should be lower-case,
exactly the same as in the UD specification, and separated by
spaces)."
  (interactive "sFields to hide: ")
  (let ((selected-fields (conllu--string-to-fields string-columns)))
    (unless conllu--invisible-property-put
      (conllu--put-property-in-fields
       'invisible (conllu--field-symbols))
      (setq-local conllu--invisible-property-put t))
    (conllu--add-to-invisibility-spec selected-fields)))

(defun conllu--string-to-fields (fields-str)
  (let ((field-sym-alist (conllu--fields-alist))
        (selected-fields-str (split-string (downcase fields-str)))
        selected-fields-sym)
    (dolist (field selected-fields-str)
      (push (cdr (assoc field field-sym-alist)) selected-fields-sym))
    (when (/= (length selected-fields-str) (length (remove-if #'null selected-fields-sym)))
      (user-error "%s" "Error: didn't understand some of the specified fields. Fields must be separated by spaces, as in:\nform feats id"))
    selected-fields-sym))

(defun conllu--put-property-in-fields (prop values)
  "puts property prop in each field of the file. the value of the
property in each field corresponds to an element in values, so
that (= (length values) number-of-fields) must hold."
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (conllu--not-looking-at-token)
            (forward-line)
            (conllu--put-property-in-fields-token-line prop values))))))

(defun conllu--put-property-in-fields-token-line (prop values)
  (dolist (val values)
    (let ((start-field (point))
          (end-field (conllu--field-end-point)))
      (put-text-property start-field end-field prop val))))

(defun conllu--add-to-invisibility-spec (syms)
  (conllu--toggle-invisibility-spec #'add-to-invisibility-spec  syms))

(defun conllu-rm-from-invisibility-spec (syms)
  (conllu--toggle-invisibility-spec
   #'remove-from-invisibility-spec syms))

(defun conllu--toggle-invisibility-spec (add-rm-func syms)
  "transforms the list of symbols in a list of cons cells with t
as cdr (this makes the invisible characters display as an
ellipsis), and then adds/removes them to the invisibility-spec,
according to function passed."
  (let ((syms-list (mapcar (lambda (sym) (cons sym t)) syms)))
    (dolist (sym-pair syms-list)
      (funcall add-rm-func sym-pair))))

(defun conllu--unzip (alist)
  "Return a list of all keys and a list of all values in ALIST.
Returns '(KEYLIST VALUELIST) where KEYLIST and VALUELIST contain all the keys
and values in ALIST in order, including repeats. The original alist can be
reconstructed with
    (asoc-zip KEYLIST VALUELIST)
NOTE: function taken from the asoc.el package by troyp: https://github.com/troyp/asoc.el"
  (let ( keylist
         valuelist
         (rest      (reverse alist)) )
    (while rest
      (let ((pair (car rest)))
        (push (car pair) keylist)
        (push (cdr pair) valuelist)
        (setq rest (cdr rest))))
    (list keylist valuelist)))

(provide 'conllu-align)

;;; conllu-align.el ends here
