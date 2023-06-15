;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Tweaked dired configs

;;; Code:

;;;; `diredfl'
(use-package diredfl
  :defer 2
  :after dired
  :hook (dired-mode . (lambda()
                        (diredfl-mode)
                        (gnus-dired-mode)))
  :config
  (require 'gnus-dired))




(provide 'bb-dired)
;;; bb-dired.el ends here
