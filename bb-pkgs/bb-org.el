;;; bb-org.el --- Org related packages -*- lexical-binding: t -*-

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

(require 'package-init)

;;;; `org'
(use-package org
  :config
  ;; Workaround function for vertico completion
  (defun org-enforce-basic-completion (&rest args)
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           (let ((map (make-sparse-keymap)))
             (define-key map [tab] #'minibuffer-complete)
             (use-local-map (make-composed-keymap (list map) (current-local-map))))
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))

  ;; Org Babel Languages setup
  (let ((languages '(emacs-lisp shell sql C racket makefile)))
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (mapcar (lambda (lang) (cons lang t))
                                         languages)))

  (let* ((org-dir "~/org.d/")
         (agenda-dir (concat org-dir "agenda.d/"))
         (notes-file (concat org-dir "notes.org")))
    (dolist (fd `(,agenda-dir ,notes-file))
      (bb-ensure-dir-or-file-exist fd))

    (setopt org-directory org-dir
            org-agenda-files (directory-files-recursively agenda-dir "\\.org\\'")
            org-default-notes-file notes-file
            org-export-html-postamble nil
            org-startup-indented t
            org-src-preserve-indentation t
            org-src-tab-acts-natively nil
            org-confirm-babel-evaluate nil
            org-src-tab-acts-natively t
            org-auto-align-tags nil
            org-tags-column 0
            org-catch-invisible-edits 'show-and-error
            org-special-ctrl-a/e t
            org-insert-heading-respect-content t
            org-edit-src-content-indentation 0
            org-hide-emphasis-markers t
            org-image-actual-width nil
            org-id-locations-file (concat org-directory ".org-id-locations")
            ;; Next 2 options are related with `advice-add' below.
            org-refile-use-outline-path 'file
            org-outline-path-complete-in-steps t
            org-pretty-entities t
            org-ellipsis "…"
            org-time-stamp-custom-formats: '("%a %F" . "%A, %d %b %y %R")
            org-clock-sound (concat org-directory "ding.mp3")
            org-agenda-tags-column 0
            org-agenda-block-separator ?─
            org-agenda-time-grid
            '((daily today require-timed)
              (800 1000 1200 1400 1600 1800 2000)
              " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
            org-agenda-current-time-string
            "◀── now ─────────────────────────────────────────────────"))
  ;; Next advice is needed to avoid problematic completion commands with Vertico
  (advice-add #'org-olpath-completing-read :around
              #'org-enforce-basic-completion)
  (advice-add #'org-make-tags-matcher :around #'org-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'org-enforce-basic-completion))




;;;; `org-modern'
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))




;;;; `ob-racket'
(use-package ob-racket
  :defer 1
  :after (org racket-mode)
  :vc (:url "https://github.com/hasu/emacs-ob-racket")
  :hook (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library))




;;;; `org-cmenu'
;; (use-package org-cmenu
;;   :defer 1
;;   :after org
;;   :vc (:url "https://github.com/misohena/org-cmenu")
;;   :config
;;   (require 'org-cmenu-setup))




;;;; `ox-reveal'
(use-package ox-reveal
  :after  org
  :defer 1)



(provide 'bb-org)
;;; bb-org.el ends here
