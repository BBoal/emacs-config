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

(defconst bb--regex-general-f "[!\"#$%&'()*+,-./:;<=>\?\\@\[\]\^_`{|}~]"
  "General Regex for common usage.")

(defconst bb--regex-lisp-f "[()`'\"@,]"
  "Regex for Lisp families programming languages.")

(defconst bb--regex-shell-f "[\"'\[\({,;~=+-/%]"
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
  :defines flymake-mode
  :functions flymake-show-buffer-diagnostics flymake--diagnostics-buffer-name
  :hook ((flymake-diagnostic-functions . package-lint-flymake)
         (flymake-mode . bb-manage-flymake-diagnostics))
  :config
  (require 'flymake)
  (setopt flymake-start-on-flymake-mode t
          flymake-no-changes-timeout nil
          flymake-start-on-save-buffer t)

  (defun bb-manage-flymake-diagnostics ()
    "Manage buffer diagnostics when flymake-mode is toggled."
    (let ((diag-buffer-name (flymake--diagnostics-buffer-name)))
      (if flymake-mode
          (progn
            (flymake-show-buffer-diagnostics)
            (shrink-window-if-larger-than-buffer (get-buffer-window diag-buffer-name)))
        (quit-window :KILL (get-buffer-window diag-buffer-name))))))




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



(provide 'setup-langs)
;;; setup-langs.el ends here
