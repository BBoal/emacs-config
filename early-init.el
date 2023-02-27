;;; early-init.el --- Emacs pre-initialisation config
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages
(scroll-bar-mode -1)               ; disable scrollbar
(menu-bar-mode -1)                 ; disable menubar
(tool-bar-mode -1)                 ; disable toolbar
;(tooltip-mode -1)                  ; disable tooltips
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
