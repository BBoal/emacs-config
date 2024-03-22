;;; bb-webdev.el --- Web Development packages -*- lexical-binding: t -*-

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
;; A short file that says a lot...

;;; Code:


;;;; `deno-bridge'
(use-package deno-bridge
  :vc (:url "https://github.com/manateelazycat/deno-bridge.git")
  :config
  (use-package websocket
    :demand t))




;;;; `emmet2-mode'
(use-package emmet2-mode
  :after deno-bridge
  :hook ((mhtml-mode html-ts-mode sgml-mode css-mode) . emmet2-mode)
  :vc (:url "https://github.com/P233/emmet2-mode.git")
  :config
  (require 'deno-bridge))



(provide 'bb-webdev)
;;; bb-webdev.el ends here
