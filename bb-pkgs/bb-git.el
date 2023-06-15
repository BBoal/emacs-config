;;; bb-git.el --- Git related packages -*- lexical-binding: t -*-

;;; Commentary:
;;; Nothing to see, move along...

;;; Code:


;;;; `vc'
(use-package vc
  :demand t
  :config
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)
  ;;  (require 'vc-dir)  ;; 2023-05-26  NOTE => Take a look into it
  (keymap-set vc-git-log-view-mode-map "s" #'vc-log-search)
  (keymap-set vc-git-log-view-mode-map "<tab>" #'log-view-toggle-entry-display)
  (keymap-set vc-git-log-view-mode-map "<return>" #'log-view-find-revision))


;;;; `vundo'
(use-package vundo
  :defer 2
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;;; `magit'
(use-package magit
  :defer 2
  :bind ("C-c m s" . magit-status))


;;;; `magit-annex'
(use-package magit-annex)



(provide 'bb-git)
;;; bb-git.el ends here
