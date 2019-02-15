;;; conllu-thing.el --- utilities for CoNLL-U files  -*- lexical-binding: t; -*-
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

;;; code:

;;; dependencies
(eval-when-compile
  (require 'cl-lib))
(require 's)


;;; looking-at functions
(defsubst conllu--looking-at (regexp)
  "Call `looking-at' REGEXP at the `beginning-of-line' and return its result.

Return point to original position."
  (save-excursion (beginning-of-line)
                  (looking-at regexp)))


(defsubst conllu--looking-at-empty-line ()
  "Return t if looking at blank line."
  (conllu--looking-at "^ *$"))


(defsubst conllu--not-looking-at-token ()
  "Return t if looking at blank or comment line, nil otherwise."
  (conllu--looking-at " *$\\|#"))


;; tokens are divided in simple, multi and empty tokens.
(defsubst conllu--looking-at-stoken ()
  "Return t if looking at a simple token line, nil otherwise."
  (conllu--looking-at "[0-9]+[^-.]"))


(defsubst conllu--looking-at-mtoken ()
  "Return t if looking at a multi-token line, nil otherwise."
  (conllu--looking-at "[0-9]+-[0-9]"))


(defsubst conllu--looking-at-etoken ()
  "Return t if looking at an empty token line, nil otherwise."
  (conllu--looking-at "[0-9]+\\.[0-9]"))


;;; structs
(cl-defstruct (conllu-sent (:constructor nil)
                           (:constructor conllu--sent-make (comments tokens))
                           (:copier nil))
  comments tokens)


(defun conllu--map-sent-tokens (f sent)
  "Return SENT with F applied to each one of its tokens."
  (conllu--sent-make (conllu-sent-comments sent)
                     (mapcar f (conllu-sent-tokens sent))))


(cl-defstruct (conllu-token (:constructor nil)
                            (:constructor conllu--token-create (id form lemma upos xpos feats head deprel deps misc))
                            (:copier nil))
  id form lemma upos xpos feats head deprel deps misc)


(defun conllu--token-meta? (tk)
  "Return t if token TK is empty or multiword."
  (consp (conllu-token-id tk)))


;;; all fields are strings, except for ID and HEAD (which are either
;;; integers or lists (nil if empty)), FEATS (list of lists of
;;; string), and DEPS (list of lists of (integer . string ...)
(defun conllu--make-token (id fo le up xp fe he dr ds m)
  "Turn CoNLL-U line field strings into a token.

ID, FO, LE, UP, XP, FE, HE, DR, DS, and M correspond to each
CoNLL-U field respectively."
  (conllu--token-create (conllu--string->token-id id) fo le up xp
                        (conllu--string->token-feats fe)
                        (conllu--string->token-head he) dr
                        (conllu--string->token-deps ds) m))


(defun conllu--string->token-id (id)
  "Turn ID string into integer or list representation.

IDs of regular tokens are integers, and those of meta tokens are
represented as lists of three elements: the first element is the
kind of meta-token ('empty or 'multi), and the rest are
integers."
  (pcase (s-slice-at "[\.-]" id)
    (`(,n) (string-to-number n))
    (`(,n ,sep-n2)
     (let ((n (string-to-number n))
           (sep (substring sep-n2 0 1))
           (n2 (string-to-number (substring sep-n2 1))))
       (pcase sep
         ("." (list 'empty n n2))
         ("-" (list 'multi n n2)))))
    (_ (user-error "Error: invalid CoNLL-U ID %s" id))))


(defun conllu--do-token-id (id w-fn e-fn m-fn)
  "Apply either W-FN E-FN or M-FN on ID, depending on its type."
  (pcase id
    (`(empty ,n ,n2) (funcall e-fn n n2))
    (`(multi ,n ,n2) (funcall m-fn n n2))
    ((pred numberp) (funcall w-fn id))))


(defun conllu--token-id->string (id)
  "Return the string representation of CoNLL-U ID."
  (conllu--do-token-id id
                       #'number-to-string
                       (lambda (n n2) (format "%d.%d" n n2))
                       (lambda (n n2) (format "%d-%d" n n2))))


(defun conllu--string->token-feat (feat)
  (if (equal feat "_")
      ;; shouldn't happen, and helps in `conllu--token-set-key-value'
      nil
    (s-split "=" feat t)))


(defun conllu--token-feat->string (feat)
  (s-join "=" feat))


(defun conllu--string->token-feats (feats)
  "Split FEATS string into list of lists."
  (if (equal feats "_")
      nil
    (let ((pairs (s-split "|" feats t)))
      (mapcar #'conllu--string->token-feat pairs))))

(defun conllu--token-feats->string (feats)
  "Format FEATS to string."
  (if feats
      (let ((pairs (mapcar #'conllu--token-feat->string feats)))
        (s-join "|" pairs))
    "_"))


(defun conllu--token-feat (key tk)
  "Get value of feature KEY from token TK."
  (cl-second (assoc key (conllu-token-feats tk))))


(defun conllu--token-head->string (head)
  "Return the string representation of CoNLL-U HEAD."
  (if head
      (conllu--token-id->string head)
    "_"))


(defun conllu--meta-token-p (tk)
  "Return t if TK is a meta CoNLL-U token (either a multiword token or an empty token."
  (consp (conllu-token-id tk)))


(defun conllu--empty-field? (fs)
  "Return t if FS is the empty field string (_)."
  (when (string-equal fs "_")
    t))

    
(defun conllu--string->token-head (h)
  "Turn H into list representation, or nil if head field is empty."
  (unless (conllu--empty-field? h)
    (conllu--string->token-id h)))


(defun conllu--string->token-deps (deps)
  "Return list representation of DEPS."
  (cl-labels ((string->dep (dep)
                           (pcase (s-split ":" dep)
                             (`(,h ,d . ,dt) (cl-list* (conllu--string->token-id h)
                                                       d
                                                       dt)))))
    (unless (conllu--empty-field? deps)
      (mapcar #'string->dep (s-split "|" deps t)))))


(defun conllu--token-deps->string (deps)
  "Return the string representation CoNLL-U DEPS."
  (cl-labels ((dep->string (dep)
                           (s-join ":" (cons (conllu--token-id->string (car dep))
                                             (cdr dep)))))
    (if deps
        (s-join "|" (mapcar #'dep->string deps))
      "_")))


;;; utils
(defun conllu--comment-line? (str)
  "Return t if STR is a CoNLL-U comment line."
  (string-equal "# " (seq-take (s-trim str) 2)))


(defun conllu--barf-unless-at-token-line (&optional message)
  "Displays error MESSAGE if not at token line."
  (when (save-excursion (beginning-of-line)
                        (conllu--not-looking-at-token))
    (user-error "%s" (or message "Error: not at token line"))))


(defun conllu--id> (id id2)
  "Return t if CoNLL-U field ID is greater than ID2."
  (pcase (cons id id2)
    (`((,_ ,beg ,_) . (,_ ,beg2 ,_))
     ;; todo: does this ever happen? if so it's incorrect
     (> beg beg2))
    (`((,_ ,beg ,_) . ,n)
     (> beg n))
    (`(,n . (,_ ,beg ,_))
     (> n beg))
    (`(,n . ,n2)
     (> n n2))))


(defun conllu--id->index (id)
  "Return the index of the ID.
For word tokens, that is the ID itself, and for meta tokens it's
the first of the two numbers."
  (pcase id
    (`(empty ,n ,_) n)
    (`(multi ,n ,_) n)
    ((pred numberp) id)))


(defun conllu--token-key->get-function (key)
  "Return function that takes token and returns value corresponding to KEY."
  (pcase key
    (:id #'conllu-token-id)
    (:form #'conllu-token-form)
    (:lemma #'conllu-token-lemma)
    (:upos #'conllu-token-upos)
    (:xpos #'conllu-token-xpos)
    (`(:feat ,feat) (apply-partially #'conllu--token-feat feat))
    (:feats #'conllu-token-feats)
    (:head #'conllu-token-head)
    (:deprel #'conllu-token-deprel)
    ;; (`(:dep dep) (apply-partially #'conllu-token-dep dep))
    (:deps #'conllu-token-deps)
    ;; (`(:misc-pair key) (apply-partially #'conllu-token-misc-pair key))
    (:misc #'conllu-token-misc)
    (_ (user-error "Wrong format for key %s" key))))


(defun conllu--token-get-key-value (tk key)
  "Return value corresponding to KEY in token TK."
  (funcall (conllu--token-key->get-function key) tk))


(defun conllu--token-key->string (tk key)
  (pcase key
    (:id (conllu--token-id->string (conllu-token-id tk)))
    (:form (conllu-token-form tk))
    (:lemma (conllu-token-lemma tk))
    (:upos (conllu-token-upos tk))
    (:xpos (conllu-token-xpos tk))
    (`(:feat ,feat) (conllu--token-feat->string
                     (list feat (conllu--token-feat feat tk))))
    (:feats (conllu--token-feats->string (conllu-token-feats tk)))
    (:head (conllu--token-head->string (conllu-token-head tk)))
    (:deprel (conllu-token-deprel tk))
    ;; (`(:dep dep) (apply-partially #'conllu-token-dep dep))
    (:deps (conllu--token-deps->string (conllu-token-deps tk)))
    ;; (`(:misc-pair key) (apply-partially #'conllu-token-misc-pair key))
    (:misc (conllu-token-misc tk))
    (_ (user-error "Wrong format for key %s" key))))


(defun conllu--token-set-key-value (tk key value &optional overwrite?)
  (cl-labels
      ((update (pair pairs)
               (if overwrite?
                   (cons pair 
                         (seq-remove (lambda (p) (equal (cl-first pair) (cl-first p)))
                                     pairs))
                 (cons pair pairs))))

    (pcase key
      (:id (setf (conllu-token-id tk) (conllu--string->token-id value)))
      (:form (setf (conllu-token-form tk) value))
      (:lemma (setf (conllu-token-lemma tk) value))
      (:upos (setf (conllu-token-upos tk) value))
      (:xpos (setf (conllu-token-xpos tk) value))
      (`(:feat ,key) (setf (conllu-token-feats tk)
                           (update (list key value)
                                   (conllu-token-feats tk))))
      (:feats (setf (conllu-token-feats tk) (conllu--string->token-feats value)))
      (:head (setf (conllu-token-head tk) (conllu--string->token-head value)))
      (:deprel (setf (conllu-token-deprel tk) value))
      ;; (`(:dep dep) (apply-partially #'conllu-token-dep dep))
      (:deps (setf (conllu-token-deps tk) (conllu--string->token-deps value)))
      ;; (`(:misc-pair key) (apply-partially #'conllu-token-misc-pair key))
      (:misc (setf (conllu-token-misc tk) value))
      (_ (user-error "Wrong format for key %s" key)))))


(provide 'conllu-thing)

;;; conllu-thing.el ends here
