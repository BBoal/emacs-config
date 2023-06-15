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
;; (set-frame-font "AardvarkFixed Nerd Font Mono 13" nil t t)


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
(setq column-number-mode t
      display-time-24hr-format t
      display-time-mode t
      ispell-dictionary nil
      sentence-end-double-space t
      sentence-end-without-period nil
      colon-double-space nil
      adaptive-fill-mode t
      bidi-inhibit-bpa t
      scroll-conservatively 101
      x-stretch-cursor t
      ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-emacs nil
      confirm-kill-processes nil
      read-process-output-max (* 1024 1024)
      save-interprogram-paste-before-kill t
      kill-read-only-ok t
      mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis)
      revert-without-query '(".*")
      imenu-auto-rescan t
      help-window-select t
      kill-whole-line t
      mouse-yank-at-point t
      calendar-week-start-day 1
      custom-safe-themes t
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
              large-file-warning-threshold (* 30 1048 1048))


;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'org-babel-post-tangle-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook (lambda() (eldoc-mode t)))
(add-hook 'html-mode-hook #'emmet-mode)


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
(require 'bb-package-init)
(require 'project)

(defun bb-require-lisp-files-in-dir (directory)
  "Requires all elisp files in DIRECTORY."
  (mapcar
   (lambda (file)
     (require (intern (file-name-sans-extension file))))
   (directory-files directory nil "\.el$")))

(defun bb-require-lisp-files-in-dir-matching (parent-dir regex-string)
  "Require all elisp files in directories matched by REGEX-STRING having
PARENT-DIR as a parent directory.

e.g  (bb-require-lisp-files-in-dir-matching user-emacs-directory \"lisp-\")

Will `require' all .el files in ~/.emacs.d/{lisp-init,lisp-utils,lisp-user}"
(let ((dirs-to-load  (directory-files parent-dir :fullpath regex-string)))
  (while dirs-to-load
      (bb-require-lisp-files-in-dir (car dirs-to-load))
      (setq dirs-to-load (cdr dirs-to-load)))))

(bb-require-lisp-files-in-dir-matching user-emacs-directory "bb-")
