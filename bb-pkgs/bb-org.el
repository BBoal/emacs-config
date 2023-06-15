;;; bb-org.el --- Org related packages -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `org'
(use-package org
  :config
  (setq org-directory "~/org.d"
        org-agenda-files (directory-files-recursively
                          (concat org-directory "/agenda.d/") "\\.org\\'")
        org-default-notes-file (concat org-directory "/notes.org")
        org-export-html-postamble nil
        org-startup-indented t
        org-src-preserve-indentation t
        org-src-tab-acts-natively nil
        org-edit-src-content-indentation 0)
  (require 'bb-org-capture))



;;;; `org-modern'
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))





(provide 'bb-org)
;;; bb-org.el ends here
