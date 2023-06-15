;;; bb-package-init.el --- Package init -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;; Initializing
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")))

;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities
      '(("melpa" . 2)
        ("elpa" . 1)))

(package-initialize)


;;;; `use-package'
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-defer t)


;;;; `auto-package-update'
(use-package auto-package-update
  :hook ((auto-package-update-after . package-autoremove)
         (auto-package-update-after . package-quickstart-refresh))
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 2)
  :config
  (auto-package-update-maybe))


(provide 'bb-package-init)
;;; bb-package-init.el ends here
