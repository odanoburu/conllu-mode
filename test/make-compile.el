(add-to-list 'load-path default-directory)

;; compile *.el files
(package-initialize)
(dolist (file (mapcar #'file-truename (file-expand-wildcards "*.el")))
  (unless (byte-compile-file file)
    (kill-emacs 1)))

