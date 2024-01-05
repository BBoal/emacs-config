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
  (let* ((org-dir "~/org.d")
         (agenda-dir (concat org-dir "/agenda.d/"))
         (notes-file (concat org-dir "/notes.org")))
    (dolist (fd `(,agenda-dir ,notes-file))
       (bb-ensure-dir-or-file-exist fd))

    (setopt org-directory org-dir
            org-agenda-files (directory-files-recursively agenda-dir "\\.org\\'")
            org-default-notes-file notes-file
            org-export-html-postamble nil
            org-startup-indented t
            org-src-preserve-indentation t
            org-src-tab-acts-natively nil
            org-auto-align-tags nil
            org-tags-column 0
            org-catch-invisible-edits 'show-and-error
            org-special-ctrl-a/e t
            org-insert-heading-respect-content t
            org-edit-src-content-indentation 0
            org-hide-emphasis-markers t
            org-pretty-entities t
            org-ellipsis "…"
            org-agenda-tags-column 0
            org-agenda-block-separator ?─
            org-agenda-time-grid
            '((daily today require-timed)
              (800 1000 1200 1400 1600 1800 2000)
              " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
            org-agenda-current-time-string
            "◀── now ─────────────────────────────────────────────────")))




;;;; `org-modern'
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))


(provide 'bb-org)
;;; bb-org.el ends here
