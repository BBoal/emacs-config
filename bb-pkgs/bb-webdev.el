;;; bb-webdev.el --- Web Development packages -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `emmet-mode'
(use-package emmet-mode
  :defer 2
  :hook (sgml-mode html-mode css-mode html-ts-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))




(provide 'bb-webdev)
;;; bb-webdev.el ends here
