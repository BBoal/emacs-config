;;; early-init.el --- Emacs pre-initialisation config -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://github.com/BBoal/emacs-config
;; Package-Requires: ((emacs "30.0"))

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


;; Tweaking some variables for improved startup time
(let ((normal-gc-cons-threshold (* 1024 1024 1024))
      (normal-file-name-handler-alist file-name-handler-alist)
      (normal-vc-handled-backends vc-handled-backends))
  ;; Initial values
  (setq gc-cons-threshold (* 8 1024 1024 1024)
        file-name-handler-alist nil
        vc-handled-backends nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    file-name-handler-alist normal-file-name-handler-alist
                    vc-handled-backends normal-vc-handled-backends))))


;; Setting themes and avoid flash of light during startup
(defun bb-emacs-avoid-flash-of-light-at-startup()
  (if (and (> (string-to-number(format-time-string "%H")) 7 )
           (< (string-to-number(format-time-string "%H")) 19))
      (progn
        (set-face-background 'default "#edf4f8")
        (setq hour-sets-theme 'ef-maris-light))
    (set-face-background 'default "#131c2b")
    (setq hour-sets-theme 'ef-maris-dark)))


;; setting the UI
(modify-all-frames-parameters  `((left-fringe . 10)
                                 (right-fringe . 10)
                                 (vertical-scroll-bars)
                                 (background-color . ,(face-background 'default))
                                 (menu-bar-lines . 0)
                                 (tab-bar-lines . 1)
                                 (tool-bar-lines . 0)))

(menu-bar-mode -1)


;; Early options to consider
(eval '(setq native-comp-async-report-warnings-errors  'silent
             load-prefer-newer                         t
             package-enable-at-startup                 nil
             package-quickstart                        t
             package-install-upgrade-built-in          t
             use-dialog-box                            nil
             inhibit-startup-screen                    t
             inhibit-startup-buffer-menu               t
             inhibit-x-resources                       t
             frame-resize-pixelwise                    t
             frame-inhibit-implied-resize              t
             garbage-collection-messages               t
             mode-line-format                          nil
             package-native-compile                    t))



(defun bb-add-to-list-elems(list &rest elements)
  "Simple way to add several ELEMENTS to a given LIST. LIST should be quoted and
will be created if does not exist"
  (if (nlistp elements)
      (user-error "ERROR: Unable to add element(s)")
    (mapc (lambda (elem)
            (set list (cons elem (symbol-value list))))
          elements)))

;; Through `display-buffer-alist' we can configure specific options for windows
;; Thanks to Prot for teaching me to successfully silence Warnings/Compile errors
(if display-buffer-alist (setq display-buffer-alist nil))
(bb-add-to-list-elems 'display-buffer-alist
                      '("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
                        (display-buffer-no-window)
                        (allow-no-window . t))
                      '("\\`\\*Bufler\\*\\'"
                        (display-buffer-pop-up-window)
                        (inhibit-same-window . t)))



(defun bb-emacs-invisible-dividers (_theme)
  "Source: https://github.com/protesilaos/dotfiles
Make windows dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'bb-emacs-invisible-dividers)


;; User info
(eval '(setq user-full-name    "Bruno Boal"
             user-login-name   "bb"
             user-mail-address "egomet@bboal.com"
             inhibit-startup-echo-area-message user-full-name))
;; 2023-07-12  REMINDER => eval is used for byte-compilation


(defun bb-emacs-re-enable-frame-theme (_frame)
  "Source: https://github.com/protesilaos/dotfiles
Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set above."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(add-hook 'after-make-frame-functions #'bb-emacs-re-enable-frame-theme)

(bb-emacs-avoid-flash-of-light-at-startup)

(provide 'early-init)
;;; early-init.el ends here
