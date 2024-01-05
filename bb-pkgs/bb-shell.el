;;; bb-shell.el --- Terminal shell related -*- lexical-binding: t -*-

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


;;;; `pass'
(use-package pass
  :defer 1)




;;;; `vterm'
(use-package vterm
  :defer 1)




;;;; `chezmoi'
;; TODO: Check how to integrate with CLI tool
;; (use-package chezmoi)




;;;; `envrc'
(use-package envrc
  :commands envrc-global-mode
  :config
  (envrc-global-mode))




;;;; `editorconfig'
(use-package editorconfig
  :commands editorconfig-mode
  :config
  (editorconfig-mode))



(provide 'bb-shell)
;;; bb-shell.el ends here
