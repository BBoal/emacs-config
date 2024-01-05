;;; bb-git.el --- Git related packages -*- lexical-binding: t -*-

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


;;;; `vc'
(use-package vc
  :defines vc-git-log-view-mode-map
  :functions log-view-toggle-entry-display log-view-find-revision
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
  :defer 1
  :defines vundo-unicode-symbols
  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols))




;;;; `magit'
(use-package magit
  :defer 1
  :bind ("C-c m s" . magit-status))




;;;; `magit-annex'
(use-package magit-annex)



(provide 'bb-git)
;;; bb-git.el ends here
