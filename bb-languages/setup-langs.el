;;; setup-langs.el --- General functions for different langs -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:

(defconst bb--regex-general-f "[]\[!\"#$%&'()*+,-./:;<=>\?\\@^`{|}~]"
  "General Regex for common usage.")

(defconst bb--regex-lisp-f "[()`'\"@,]"
  "Regex for Lisp families programming languages.")

(defconst bb--regex-shell-f "[\"'\[\({,;~<=>*+-/$@^|%]"
  "Regex for shell families.")

;; (provided-mode-derived-p 'python-ts-mode 'python-base-mode)
(defvar bb-prog-langs-alist
  `((lisp-interaction-mode . ,bb--regex-lisp-f)
    (emacs-lisp-mode       . ,bb--regex-lisp-f)
    (bash-ts-mode          . ,bb--regex-shell-f)
    (sh-mode               . ,bb--regex-shell-f))
  "Alist of characters, language specific, used by `bb-simple-ç-dwim'.")




;;;; `package-lint-flymake'
(use-package package-lint-flymake
  :defer 1
  :defines flymake-mode bb-flymake-assoc-buffer
  :functions bb-flymake-check-focus bb-flymake-cleanup-diag
  bb-flymake--diag-buffer-name flymake-show-buffer-diagnostics
  :hook ((flymake-diagnostic-functions . package-lint-flymake)
         (flymake-mode . bb-manage-flymake-diagnostics))
  ;; (buffer-list-update . bb-flymake-check-focus)

  :config
  (require 'flymake)
  (setopt flymake-start-on-flymake-mode t
          flymake-no-changes-timeout nil
          flymake-start-on-save-buffer t)
  (add-hook 'kill-buffer-hook #'bb-flymake-cleanup-diag -90)

  :init
  (defun bb-flymake--diag-buffer-name (&optional name)
    "Copy of `flymake--diagnostics-buffer-name' with argument NAME."
    (setq name (or name (current-buffer)))
    (format "*Flymake diagnostics for `%s'*" name))

  (defun bb-flymake-check-focus (&optional window)
    (setq window (or window (selected-window)))
    (with-selected-window window
      (if flymake-mode
          (let* ((diag-buffer (bb-flymake--diag-buffer-name))
                 (diag-window (get-buffer-window diag-buffer)))
            (cond
             ((null diag-window)
              (flymake-show-buffer-diagnostics))
             ((not (window-live-p diag-window))
              (display-buffer diag-buffer))
             (t
              (message "True clause"))))
        (mapc (lambda (w)
                (when
                    (string-prefix-p "*Flymake" (buffer-name (window-buffer w)))
                  (delete-window w)))
              (window-list nil :no-minibuf)))))

  (defun bb-flymake-cleanup-diag ()
    ;; (if (memq 'bb-flymake-check-focus window-buffer-change-functions)
    ;;   (setq window-buffer-change-functions
    ;;         (delq 'bb-flymake-check-focus window-buffer-change-functions)))
    (let ((name (current-buffer)))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (when (equal (buffer-name name) bb-flymake-assoc-buffer)
                  (if-let ((win-buf (get-buffer-window buf)))
                           ;; ((window-live-p win-buf)))
                      (quit-window :KILL win-buf)
                    (kill-buffer buf)))))
            (buffer-list))))

  ;; (defun bb-cleanup-diag-buffer ()
  ;;   (let ((name (current-buffer)))
  ;;     ;; (message "Current Buffer is: %s" name)
  ;;     (with-current-buffer name
  ;;       (when-let ((diag-buffer (bb-flymake--diag-buffer-name name))
  ;;                  (diag-window (get-buffer-window diag-buffer)))
  ;;         (message "Buffer is %s and Window is %s" diag-buffer diag-window)
  ;;         (with-selected-window diag-window
  ;;           (quit-window :KILL diag-window))))))
  ;;
  ;; (defun bb-cleanup-diag ()
  ;;   (bb-cleanup-diag-assoc-buffer)
  ;;   (bb-cleanup-diag-buffer))


  (defun bb-manage-flymake-diagnostics ()
    "Manage buffer diagnostics when flymake-mode is toggled."
    (if flymake-mode
        (flymake-show-buffer-diagnostics)
      (bb-flymake-cleanup-diag))))

;; (cl-pushnew #'bb-flymake-check-focus window-buffer-change-functions)
;; (advice-add #'select-window :after #'bb-flymake-check-focus))
;; (advice-remove #'select-window #'bb-flymake-check-focus)




;;;; `bb-programming-hooks'
(declare-function indent-bars-mode "ext:indent-bars")

(defun bb-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (electric-pair-local-mode t)
  (subword-mode t)
  (indent-bars-mode t)
  (hs-minor-mode t))




;;;; `bb-eglot-arrange-file'
(declare-function cpp-auto-include "ext:cpp-auto-include")
(declare-function eglot-code-action-organize-imports "eglot")
(declare-function eglot-format-buffer "eglot")

(defun bb-eglot-arrange-file()
  "Imports and formats programming file using Eglot."
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




;;;; `dape'
(use-package dape
  ;; To use window configuration like gud (gdb-mi)
  :init
  (setopt dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setopt dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  (setopt dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t))))



(provide 'setup-langs)
;;; setup-langs.el ends here
