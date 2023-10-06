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

(defcustom bb-prog-langs-alist
  `((lisp-interaction-mode . ,bb--regex-lisp-f)
    (emacs-lisp-mode       . ,bb--regex-lisp-f)
    (bash-ts-mode          . ,bb--regex-shell-f)
    (sh-mode               . ,bb--regex-shell-f))
  "Alist of characters, language specific, used by `bb-simple-ç-dwim'")


(defun bb-try-jump-args-direction(arg paragraph-boundary)
  " Jump ARG times through user specific chars bounded by PARAGRAPH-BOUNDARY.

According to a specific regexp described in the 'bb-prog-langs-list' obtained
according to the major-mode, the user can \"jump\" to designated chars to quickly
re-edit the current paragraph."
  (interactive "p")
  (let ((regexp (or (alist-get major-mode bb-prog-langs-alist)
                    bb--regex-general-f))
        (bound (save-excursion
                 (funcall paragraph-boundary)
                 (point))))

    (if (> arg 0)
        (re-search-forward regexp bound t arg)
      ;; else  arg < 0
      (unless (or (bobp) (eq (point) (1+ (point-min))))
        (forward-char -1)
        (re-search-forward regexp bound t arg)
        (forward-char 1)))))
;; 2023-08-18 BUG => `start-of-paragraph' is inconsistent with
;; `end-of-paragraph-text'




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
  (if (or (eq major-mode 'c++-mode)
          (eq major-mode 'c++-ts-mode))
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
