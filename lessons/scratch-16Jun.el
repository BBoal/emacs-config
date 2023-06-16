;; Lesson 16Jun - Friday


;;;;;;;;;;;;;; v2
(defmacro my-project-root (mode file)
  "Define project root check for MODE given FILE.
MODE must be the symbol of the major mode, without a quote.  FILE
is a string."
  (let ((project-find-fn (intern (format "project-find-%s-root" mode)))
        (major-mode-fn (intern (format "bb-%s-project-find-function" mode)))
        (file-symbol (intern file)))
    `(progn
       (defun ,project-find-fn (dir)
         (when-let ((root (locate-dominating-file dir ,file)))
           (cons ',file-symbol root)))

       (cl-defmethod project-root ((project (head ,file-symbol)))
         (cdr project))

       (defun ,(intern (format "bb-%s-project-find-function" mode)) ()
         (add-hook 'project-find-functions #',project-find-fn :depth :local))

       (add-hook ',(intern (format "%s-hook" mode)) #',major-mode-fn))))




;;;;;;;;;;;;; v1
(defmacro my-project-root (name file)
  (let ((fn (intern (format "project-find-%s-root" name))))
    `(progn
       (defun ,fn (dir)
         (when-let ((root (locate-dominating-file dir ,file)))
           (cons 'filename root)))

       (add-hook 'project-find-functions #',fn))))

(my-project-root js "js-file")
