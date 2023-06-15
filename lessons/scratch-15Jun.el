;;;;;;;;;;;;;;;;;;;;;;;;
;; Prot's explains... ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-lisp-dirs ()
  "Return my Lisp files in DIRECTORY."
  (directory-files user-emacs-directory :fullpath "bb-"))


(defun bb-lisp-files-sans-extension (directory)
  "Returns a list with all elisp files in DIRECTORY without extension."
  (mapcar
   (lambda (file)
     (file-name-sans-extension file))
   (directory-files directory nil "\.el$")))


(defun my-lisp-files-require (files)
  (dolist (file files)
    (insert file)))


(my-lisp-files-sans-extension "~/.emacs.d")
(my-lisp-files-require (my-lisp-files-sans-extension "~/.emacs.d/lisp"))


;;;;;;;;;;;;;;;
;; Version 2 ;;
;;;;;;;;;;;;;;;

(defun require-files-in-regex-dirs (regex-string)
(let ((dirs-to-load  (directory-files user-emacs-directory :fullpath "bb-" )))
  (while dirs-to-load
    (progn
      (mapcar
       (lambda (file)
         (file-name-sans-extension file))
       (directory-files (car dirs-to-load) nil directory-files-no-dot-files-regexp))
      (setq dirs-to-load (cdr dirs-to-load)))))

;;;;;;;;;;;;;;;
;; Version 3 ;;
;;;;;;;;;;;;;;;

(defun bb-require-lisp-files-in-dir (directory)
  "Requires all elisp files in DIRECTORY."
  (mapcar
   (lambda (file)
     (require (file-name-sans-extension file)))
   (directory-files directory nil "\.el$")))


(defun bb-require-lisp-files-in-dir-matching (parent-dir regex-string)
  "Require all elisp files in directories matched by REGEX-STRING having
PARENT-DIR as a parent directory.

e.g  (bb-require-lisp-files-in-dir-matching user-emacs-directory \"lisp-\")

Will `require' all .el files in ~/.emacs.d/{lisp-init,lisp-utils,lisp-user}"
(let ((dirs-to-load  (directory-files parent-directory :fullpath regex-string)))
  (while dirs-to-load
      (bb-require-lisp-files-in-dir (car dirs-to-load)
      (setq dirs-to-load (cdr dirs-to-load))))))

(bb-require-lisp-files-in-dir-matching user-emacs-directory "bb-")
