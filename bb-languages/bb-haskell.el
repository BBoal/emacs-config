;;; bb-haskell.el --- Haskell setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `haskell-mode'
(use-package haskell-mode
  :hook
  (haskell-mode . (lambda()
                          (eglot-ensure)
                          (bb-programming-hooks)))
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  (haskell-indent-mode t))


;;;; `ormolu'
(use-package ormolu
  :hook (haskell-mode . (ormolu-format-on-save-mode))
  :bind (:map haskell-mode-map
              ("C-c r" . ormolu-format-buffer)))



(provide 'bb-haskell)
;;; bb-haskell.el ends here
