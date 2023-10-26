;;; bb-org.el --- Org related packages -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `org'
(use-package org
  :config
  (let* ((org-dir "~/org.d")
         (agenda-dir (concat org-dir "/agenda.d/"))
         (notes-file (concat org-dir "/notes.org")))
    (mapc
     (lambda (fd)
       (bb-ensure-dir-or-file-exist fd))
     `(,agenda-dir ,notes-file))

    (setq org-directory org-dir
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
          "◀── now ─────────────────────────────────────────────────"))
  (require 'bb-org-capture))




;;;; `org-modern'
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))


(provide 'bb-org)
;;; bb-org.el ends here
