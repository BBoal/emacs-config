;;; early-init.el --- Emacs pre-initialisation config -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://github.com/BBoal/emacs-config
;; Package-Requires: ((emacs "29.1"))

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

;; Disabling the custom file.
(setq-default custom-file (make-temp-file "emacs-custom-"))

;; Do not show compilation warnings
(setq warning-suppress-types '((use-package) (bytecomp) (comp)))

;; garbage collection setup
(let ((normal-gc-cons-threshold (* 64 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
			  (setq gc-cons-threshold normal-gc-cons-threshold))))


;; User info
(setq user-full-name    "Bruno Boal"
      user-login-name   "bb"
      user-mail-address "egomet@bboal.com")

(setq package-enable-at-startup   	    nil
	  package-quickstart                t
	  use-dialog-box              	    nil
      inhibit-startup-screen     	    t
	  inhibit-startup-buffer-menu       t
      frame-resize-pixelwise            t
	  frame-inhibit-implied-resize      t
      load-prefer-newer                 t
	  garbage-collection-messages       t
      package-native-compile            t)

(eval '(setq inhibit-startup-echo-area-message user-full-name))


;; setting the UI
(scroll-bar-mode -1)     ; disable scrollbar
(menu-bar-mode   -1)     ; disable menubar
(tool-bar-mode   -1)     ; disable toolbar
(tooltip-mode    -1)     ; help text in echo area
(set-fringe-mode 10)     ; give some breathing room

(defun bb-emacs-invisible-dividers (_theme)
  "Make windows dividers for THEME invisible."
  (let ((bg (face-background 'default)))
	(custom-set-faces
	 `(fringe ((t :background ,bg :foreground ,bg)))
	 `(window-divider ((t :background ,bg :foreground ,bg)))
  	 `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
  	 `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'bb-emacs-invisible-dividers)

;; Setting themes and avoid flash of light during startup
(if (and (> (string-to-number(format-time-string "%H")) 6 )
         (< (string-to-number(format-time-string "%H")) 18))
    (progn
      (set-face-background 'default "white")
      (setq hour-sets-modus 'modus-operandi))
  (set-face-background 'default "black")
  (setq hour-sets-modus 'modus-vivendi))


(provide 'early-init)
;;; early-init.el ends here
