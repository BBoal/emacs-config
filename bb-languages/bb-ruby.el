;;; bb-ruby.el --- Ruby setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `inf-ruby'
(use-package inf-ruby
  :defer 3
  :bind (:map inf-ruby-minor-mode-map
              ("C-c r c" . inf-ruby-console-auto))
  :hook (compilation-filter . inf-ruby-auto-enter-and-focus))




;;;; `ruby-electric'
(use-package ruby-electric
  :defer 3)




;;;; `ruby-end'
(use-package ruby-end
  :defer 3)




;;;; `enh-ruby-mode'
(use-package enh-ruby-mode
  :defer 3
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (enh-ruby-mode . ruby-electric-mode)))




;;;; `rspec-mode'
(use-package rspec-mode
  :defer 3)




;;;; `rubocopfmt'
(use-package rubocopfmt
  :defer 3)


(provide 'bb-ruby)
;;; bb-ruby.el ends here
