;;; init.el --- BB's config -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://github.com/BBoal/emacs-config
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

;;; Commentary: Still a sapling dreaming of becoming a tree

;;; Code:

;;;;;;;;;;;;;;;;
;;; Settings ;;;
;;;;;;;;;;;;;;;;

;;; Set default font
(set-face-attribute 'default nil
                    :family "Iosevka Zenodotus"
                    :height 120)
;; (set-frame-font "FantasqueSansMono Nerd Font Mono 13" nil t t)


;;;;;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-file-name-transforms
      `(("\\`/.*/\\([^/]+\\)\\'"
         ,(concat (locate-user-emacs-file ".tmp/")"\\1") t)))


;;; Welcome message
(setq-default initial-scratch-message
              (let ((emacs-version (replace-regexp-in-string "\s\(.*\)\n" "" (emacs-version))))
                (format ";; %s\n;; Initialization in %s\n;; %s, be disciplined and maintain focus.\n\n"
                        emacs-version (emacs-init-time "%.3fs") user-full-name)))


;;; User preferences
(setq bidi-inhibit-bpa t
      scroll-conservatively 101
      x-stretch-cursor t
      ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-processes nil
      read-process-output-max (* 1024 1024)
      save-interprogram-paste-before-kill t
      kill-read-only-ok t
      revert-without-query '(".*")
      imenu-auto-rescan t
      help-window-select t
      kill-whole-line t
      mouse-yank-at-point t
      calendar-week-start-day 1
      custom-safe-themes t
      dictionary-server "dict.org"
      set-mark-command-repeat-pop t ;; C-u C-SPC once, then C-SPC, C-SPC, ...
      frame-title-format '(multiple-frames "%b"
                                           ("" "Emacs - %b ")))


(setq-default fill-column 100
              tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct
              tab-width 4
              scroll-margin 4
              indent-tabs-mode nil
              kill-do-not-save-duplicates t
              bidi-paragraph-direction 'left-to-right
              large-file-warning-threshold (* 30 1024 1024))


;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'org-babel-post-tangle-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)


;; Select text is replaced with input
(delete-selection-mode t)

;; Automatically loads the files if they change in another process
(global-auto-revert-mode t)

;; File name shadow mode
(file-name-shadow-mode t)


;;; No warnings and restrictions
(dolist (unrestricted '(erase-buffer narrow-to-region narrow-to-page dired-find-alternate-file
                        upcase-region downcase-region))
  (put unrestricted 'disabled nil))

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
(dolist (dirs (directory-files user-emacs-directory :fullpath "bb-"))
  (add-to-list 'load-path dirs))

;; Initialization
(require 'package-init)
(require 'project)
(require 'setup-langs)

;; Loads all "bb-" elisp files from "bb-" dirs
(bb-require-lisp-files-in-dir-matching user-emacs-directory "bb-")
