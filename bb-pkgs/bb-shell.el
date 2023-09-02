;;; bb-shell.el --- Terminal shell related -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `pass'
(use-package pass
  :defer 2)




;;;; `vterm'
(use-package vterm
  :defer 2)




;;;; `chezmoi'
;; TODO: Check how to integrate with CLI tool
;; (use-package chezmoi)




;;;; `envrc'
(use-package envrc
  :config
  (envrc-global-mode))




;;;; `editorconfig'
(use-package editorconfig
  :config
  (editorconfig-mode))



(provide 'bb-shell)
;;; bb-shell.el ends here
