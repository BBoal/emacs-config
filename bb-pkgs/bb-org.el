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
          org-edit-src-content-indentation 0))
  (require 'bb-org-capture))




;;;; `org-modern'
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))


(provide 'bb-org)
;;; bb-org.el ends here
