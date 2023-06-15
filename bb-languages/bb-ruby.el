;;; bb-ruby.el --- Ruby setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `inf-ruby'
(use-package inf-ruby
  :bind (:map inf-ruby-minor-mode-map
              ("C-c r c" . inf-ruby-console-auto))
  :hook (compilation-filter . inf-ruby-auto-enter-and-focus))

;;;; `ruby-electric'
(use-package ruby-electric)

;;;; `ruby-end'
(use-package ruby-end)

;;;; `enh-ruby-mode'
(use-package enh-ruby-mode
  :after inf-ruby ruby-electric
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :functions inf-ruby-keys
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (enh-ruby-mode . ruby-electric-mode))
  :config
    (inf-ruby-keys))

;;;; `rspec-mode'
(use-package rspec-mode)

;;;; `rubocopfmt'
(use-package rubocopfmt)



(provide 'bb-ruby)
;;; bb-ruby.el ends here
