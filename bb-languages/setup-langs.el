;;; setup-langs.el --- General functions for different langs -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `bb-programming-hooks'
(defun bb-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (electric-pair-local-mode t)
  (subword-mode t)
  (indent-guide-mode t)
  (hs-minor-mode t))

;;;; `bb-eglot-arrange-file'
(defun bb-eglot-arrange-file()
  "Imports and formats programming file using Eglot"
  (interactive)
  (if (eq major-mode 'c++-mode)
      (cpp-auto-include))
  (ignore-errors
    (eglot-code-action-organize-imports (point-min)))
  (eglot-format-buffer))


;;;; `prot-find-project-root'
(defmacro prot-find-project-root (mode file)
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




(provide 'setup-langs)
;;; setup-langs.el ends here
