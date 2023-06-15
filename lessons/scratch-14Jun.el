;; GNU Emacs 30.0.50 of 2023-06-11
;; Initialization in 0.299s
;; Bruno Boal, be disciplined and maintain focus.

;; Then `dolist' over this to `require' each file.
(mapcar
 (lambda (file)
   (file-name-sans-extension file))
 ;; (split-string
 ;;  (shell-command-to-string "ls emacs/.emacs.d/prot-lisp/")
 ;;  "\n"
 ;;  :omit-nulls)
 (directory-files "emacs/.emacs.d/prot-lisp/" nil directory-files-no-dot-files-regexp))


(defun my-lisp-files (directory)
  "Return my Lisp files in DIRECTORY."
  (directory-files directory nil directory-files-no-dot-files-regexp))

(defun my-list-files-sans-extension (files directory)
  (mapcar
   (lambda (file)
     (file-name-sans-extension file))
   (my-lisp-files directory)))

(defun my-lisp-files-require (files)
  (dolist (file files)
    (require file)))

(my-lisp-files-require (my-lisp-files directory))

;;;;;;;;;;;;;;,

(defun my-mode-to-keyword (mode)
  (intern
   (replace-regexp-in-string
    "-mode"
    ""
    (format ":%s" mode))))

(map-nested-elt
 (with-current-buffer (find-file-noselect "path/to/file")
   (let ((json-object-type 'plist))
     (json-read-from-string (buffer-string))))
 '(:css :snippets :@f+))
