;;; early-init.el --- Emacs pre-initialisation config -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <bruno.boal@tutanota.com>
;; Author: Bruno Boal <bruno.boal@tutanota.com>
;; URL: https://github.com/BBoal/emacs-config
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      load-prefer-newer         t
      package-native-compile    t) ; native compile packages

(scroll-bar-mode -1)               ; disable scrollbar
(menu-bar-mode -1)                 ; disable menubar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; help text in echo area
(set-fringe-mode 10)               ; give some breathing room

(setq gc-cons-threshold (* 1024 1024 1024))

(if (and (> (string-to-number(format-time-string "%H")) 6 )
         (< (string-to-number(format-time-string "%H")) 18))
    (progn
      (set-face-background 'default "white")
      (setq hour-sets-modus 'modus-operandi))
  (set-face-background 'default "black")
  (setq hour-sets-modus 'modus-vivendi))


(provide 'early-init)
;;; early-init.el ends here
