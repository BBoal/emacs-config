;;; setup-langs.el --- General functions for different langs -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


(defconst bb--regex-general-f "[!\"#$%&'()*+,-./:;<=>\?\\@\[\]\^_`{|}~]"
  "General Regex for common usage")

(defconst bb--regex-lisp-f "[()`'\"@,]"
  "Regex for Lisp families programming languages")

(defconst bb--regex-shell-f "[\"'\[\({,;~=+-/%]"
  "Regex for shell families")

;; (provided-mode-derived-p 'python-ts-mode 'python-base-mode)
(defcustom bb-prog-langs-alist
  `((lisp-interaction-mode . ,bb--regex-lisp-f)
    (emacs-lisp-mode       . ,bb--regex-lisp-f)
    (bash-ts-mode          . ,bb--regex-shell-f)
    (sh-mode               . ,bb--regex-shell-f))
  "Alist of characters, language specific, used by `bb-simple-ç-dwim'")




;;;; `package-lint-flymake'
(use-package package-lint-flymake
  :defer 1
  :hook (flymake-diagnostic-functions . package-lint-flymake)
  :config
  (require 'flymake)
  (setq flymake-start-on-flymake-mode t
        flymake-no-changes-timeout nil
        flymake-start-on-save-buffer t))



;;;; `bb-programming-hooks'
(defun bb-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (electric-pair-local-mode t)
  (subword-mode t)
  (indent-bars-mode t)
  (hs-minor-mode t))




;;;; `bb-eglot-arrange-file'
(defun bb-eglot-arrange-file()
  "Imports and formats programming file using Eglot"
  (interactive)
  (cond
   ((or (eq major-mode 'c++-mode)
        (eq major-mode 'c++-ts-mode))
    (cpp-auto-include))
   ((or (eq major-mode 'rust-mode)
        (eq major-mode 'rust-ts-mode))
    (prettify-symbols-mode)))
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
