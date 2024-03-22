;;; init.el --- BB's config -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Still a sapling dreaming of becoming a tree

;;; Code:


;;;;;;;;;;;;;;;;
;;; Settings ;;;
;;;;;;;;;;;;;;;;

;;; Set default font
(if (display-graphic-p) (setq-default line-spacing 0.1))
;; (set-fontset-font t 'symbol (font-spec :family "Symbols Nerd Font" :size 24))
;; (set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 125)
;; (set-face-attribute 'default nil :family "D2CodingLigature Nerd Font" :height 120)
;; (set-face-attribute 'default nil :family "Sudo" :height 150)
;; (set-face-attribute 'default nil :family "CommitMonoZenodotus" :height 120)
;; (set-face-attribute 'default nil :family "Iosevka Zenodotus" :height 130)
;; (set-face-attribute 'default nil :family "JetBrainsMono" :height 110)

;;;;;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-list-file-prefix (concat user-emacs-directory
                                         ".cache/auto-save-list/saves-")
      auto-save-file-name-transforms
      `(("\\`/.*/\\([^/]+\\)\\'"
         ,(concat (locate-user-emacs-file ".tmp/")"\\1") t)))

;;; Welcome message
(setq-default
 initial-scratch-message
 (let ((emacs-version (replace-regexp-in-string "\s\(.*\)\n" "" (emacs-version))))
   (format ";; %s\n;; Initialization in %s\n;; %s, be disciplined and maintain focus.\n\n"
           emacs-version (emacs-init-time "%.3fs") user-full-name)))


(defun bb-maybe-check-parens ()
  "If derived-mode is Lisp data, check for parenthesis correcteness."
  (if (derived-mode-p 'lisp-data-mode) (check-parens)))

(setq bidi-inhibit-bpa t
      show-paren-delay 0
      blink-matching-paren nil
      delete-pair-blink-delay 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      x-stretch-cursor t
      ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-processes nil
      read-process-output-max (* 1024 1024)
      save-interprogram-paste-before-kill t
      kill-read-only-ok t
      revert-without-query '(".*")
      help-window-select t
      help-window-keep-selected t
      kill-whole-line t
      mouse-yank-at-point t
      custom-safe-themes t
      delete-window-choose-selected 'pos
      switch-to-prev-buffer-skip 'this
      enable-recursive-minibuffers t
      write-file-functions '(bb-maybe-check-parens)
      ;; switch-to-buffer-obey-display-actions t
      show-paren-context-when-offscreen 'overlay
      split-height-threshold 20
      auto-save-no-message t
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      window-combination-resize t
      ;; window-combination-limit 'display-buffer
      describe-bindings-outline-rules nil
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      frame-title-format '(multiple-frames
                           "%b" ("" "Emacs - %b ")))

(setq-default fill-column 80
              tab-width 4
              scroll-margin 3
              indent-tabs-mode nil
              comment-fill-column fill-column
              emacs-lisp-docstring-fill-column fill-column
              sentence-end-double-space nil
              eval-expression-print-length nil
              ;; case-fold-search nil
              tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct
              kill-do-not-save-duplicates t
              write-contents-functions '(bb-maybe-check-parens)
              cursor-in-non-selected-windows nil
              bidi-paragraph-direction 'left-to-right
              large-file-warning-threshold (* 30 1024 1024)
              project-vc-extra-root-markers
              '( ".dir-locals.el" "package.json" "Makefile" "Gemfile"
                 "Dockerfile" "CMakeLists.txt" "autogen.sh" "main.*"))


;; No cursor blinking
(blink-cursor-mode -1)

;; Depth indication of recursive minibuffers
(minibuffer-depth-indicate-mode t)

;; Select text is replaced with input
(delete-selection-mode t)

;; Automatically loads the files if they change in another process
(global-auto-revert-mode t)

;; File name shadow mode
(file-name-shadow-mode t)

;; Remember last position on file
(with-eval-after-load 'saveplace
  (setopt save-place-file (concat user-emacs-directory "var/places")))
(save-place-mode t)


;;; No warnings and restrictions
(defvar restricted-functions
  '(erase-buffer
    narrow-to-region
    narrow-to-page
    dired-find-alternate-file
    upcase-region
    downcase-region)
  "List of restricted functions by default that we want to always enable.")

(mapc (lambda (restricted)
        (put restricted 'disabled nil))
      restricted-functions)


;; Allow the following local variables in files
(setq safe-local-variable-values
      '((eval add-to-list 'whitespace-style 'indentation::tab)
        (eval delete 'indentation whitespace-style)
        (display-line-numbers . visual)
        (eval indent-tabs-mode t)
        (eval indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom functions/libraries/modules ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adding "bb-" dirs to load-path
(mapc (lambda (dir)
          (cl-pushnew dir load-path))
      (directory-files user-emacs-directory :fullpath "bb-"))

;; Initialization
(require 'package-init)
(require 'project)
(with-eval-after-load 'project
  (setq project-list-file (concat user-emacs-directory "var/projects"))
  (setcdr project-vc-backend-markers-alist nil))
(require 'setup-langs)
(require 'server)

;; Loads all "bb-" elisp files from "bb-" dirs
(bb-require-lisp-files-in-dir-matching user-emacs-directory "bb-")


(defvar modes-with-autofill
  '(text-mode-hook
    message-mode-hook
    markdown-mode-hook
    adoc-mode-hook
    emacs-lisp-mode-hook
    org-mode-hook)
  "Modes that benefit from auto-fill mode.")

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'org-babel-post-tangle-hook
          #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'emacs-lisp-mode-hook
          (lambda() (setq sentence-end-double-space t) (eldoc-mode 1)))
(add-hook 'activate-mark-hook (lambda() (setq cursor-type 'bar)))
(add-hook 'deactivate-mark-hook (lambda() (setq cursor-type t)))
(add-hook 'xref-after-jump-hook #'pulsar-recenter-top)
(mapc (lambda (mode) (add-hook mode #'turn-on-auto-fill)) modes-with-autofill)
;; access from emacsclient
(add-hook 'after-init-hook (lambda ()
                             (unless (server-running-p) (server-start))))



(provide 'init)
;;; init.el ends here
