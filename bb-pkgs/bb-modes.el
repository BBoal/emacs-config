;;; bb-modes.el --- Various modes -*- lexical-binding: t -*-

;;; Commentary:
;;; Support for external modes

;;; Code:


;;;; `nov'
(use-package nov
  :defer 2
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Iosevka Zenodotus Fixed"
                             :height 130))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))




;;;; `pdf-tools'
(use-package pdf-tools
  :defer 3)




;;;; `csv-mode'
(use-package csv-mode
  :defer 2
  :mode	("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (setq csv-separators '("," ";" "|" " ")))




;;;; `adoc-mode'
(use-package adoc-mode
  :defer 2
  :mode
  ("\\.adoc\\'" . adoc-mode))




;;;; `markdown-mode'
(use-package markdown-mode
  :defer 2
  :mode
  ("\\.md\\.html\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command
        (concat
         (executable-find "pandoc")
         " --from=markdown --to=html5"
         " --standalone --mathjax --highlight-style=pygments"))
  (defun no-delete-whitespace()
    (remove-hook 'before-save-hook #'delete-trailing-whitespace :local))
  (add-hook 'markdown-mode-hook 'no-delete-whitespace))


(provide 'bb-modes)
;;; bb-modes.el ends here
