;;; bb-ruby.el --- Ruby setup -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:


(use-package inf-ruby
  :vc ( :url "https://github.com/BBoal/inf-ruby.git"
        :rev :newest)
  :defines inf-ruby-minor-mode-map
  :bind (:map inf-ruby-minor-mode-map
              ("C-c r c" . inf-ruby-console-auto))
  :hook (compilation-filter . inf-ruby-auto-enter-and-focus))




;;;; `ruby-electric'
(use-package ruby-electric)




;;;; `ruby-end'
(use-package ruby-end)




;;;; `enh-ruby-mode'
(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (enh-ruby-mode . ruby-electric-mode)))




;;;; `rspec-mode'
(use-package rspec-mode)




;;;; `rubocopfmt'
(use-package rubocopfmt)


(provide 'bb-ruby)
;;; bb-ruby.el ends here
