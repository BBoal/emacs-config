;;; bb-haskell.el --- Haskell setup -*- lexical-binding: t -*-

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

(require 'setup-langs)

;;;; `haskell-mode'
(use-package haskell-mode
  :commands haskell-indent-mode
  :hook
  (haskell-mode . (lambda()
                    (eglot-ensure)
                    (bb-programming-hooks)))
  :config
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false))))))  ;; disable
                                                                        ;; stan
  (haskell-indent-mode t))




;;;; `ormolu'
(use-package ormolu
  :defines haskell-mode-map
  :hook (haskell-mode . (ormolu-format-on-save-mode))
  :bind (:map haskell-mode-map
              ("C-c r" . ormolu-format-buffer)))



(provide 'bb-haskell)
;;; bb-haskell.el ends here
