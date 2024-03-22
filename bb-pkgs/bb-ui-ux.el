;;;; bb-ui-ux.el --- Improving UI and UX -*- lexical-binding: t -*-

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
;;; Configuration of packages related to embelish and empower Emacs UI/UX

;;; Code:

;;;; Font Configuration
(defvar bb-set-font--favorites
  '(("JetBrainsMono" . 110)
    ("Fantasque Sans Mono" . 130)
    ("Iosevka Zenodotus" . 130)
    ("Sudo" . 150)
    ("D2CodingLigature Nerd Font" . 120)
    ("CommitMonoZenodotus" . 120))
  "The list is organized for readability having in consideration that the time
  periods start from 00h00.")

(defun bb-set-font-name-size (font size)
  "Set FONT with SIZE using `set-face-attribute'.

FONT is the font name as a string with and SIZE an integer.
E.g. (bb-set-font-with-size \"Iosevka Zenodotus\" 120).

When called through Lisp, the values are tested for maximum and minimum accepted
  values (Max:190 and Min:110). If the user decides to ignore these values the
  program sets the SIZE to the aforementioned top or bottom values, warning the
  user of the fact."
  (interactive
   (let* ((font (completing-read
                 "Insert font: " (mapcar #'car bb-set-font--favorites)
                 nil :req-match))
          (size (string-to-number
                 (completing-read "Insert size: "
                                  (mapcar #'number-to-string
                                          (number-sequence 110 190 10))
                                  nil :req-match nil nil
                                  (number-to-string
                                   (map-elt bb-set-font--favorites font))))))
     (list font size)))
  (let ((max-size 190) (min-size 110) action threshold)
    (cond
     ((> size max-size)
      (setq size max-size action "reduced" threshold "max"))
     ((< size min-size)
      (setq size min-size action "increased" threshold "min")))
    (set-face-attribute 'default nil :family font :height size)
    (if action
        (message "Size was %s to %s value of %d" action threshold size))))

(defun bb-set-font-current-period (&rest _)
  "Get current period of the day and call `set-font-name-size'."
  (if-let* (((consp bb-set-font--favorites))
            (num-fonts (length bb-set-font--favorites))
            (present-hour (elt (decode-time) 2))
            (period-bounds (/ 24.0 num-fonts))
            (cur-period (truncate
                         (/ present-hour period-bounds))) ; 0 based math
            (attr-period (elt bb-set-font--favorites cur-period))
            (font (car attr-period))
            (size (cdr attr-period)))
      (progn
        (message "Loading font:  %s, size %d" font size)
        (bb-set-font-name-size font size))
    (message "Unable to proceed. Is 'bb-set-font--favorites' properly defined?")))

;; Set current font
(bb-set-font-current-period)




;;;; modus-themes personal preferences
(with-eval-after-load 'modus-themes
  (defvar modus-operandi-tinted-palette-overrides
        '((cursor blue-intense)
          (bg-mode-line-active "#d0d6ff")
          (bg-mode-line-inactive "#e6e6e6"))))




;;;; `theme-buffet'
(use-package theme-buffet
  :demand t
  :functions calendar-current-time-zone
  theme-buffet-modus-ef theme-buffet-timer-hours
  :config
  (require 'cal-dst)
  (setopt theme-buffet-time-offset
          (1+ (ceiling (cadr (calendar-current-time-zone)) 60)))
  (theme-buffet-modus-ef)
  (theme-buffet-timer-hours 1))




;;;; `minimap'
(use-package minimap
  :after modus-themes ef-themes
  :defines minimap-major-modes
  :commands minimap-mode
  :init
  (setopt minimap-major-modes nil
          minimap-width-fraction 0.18
          minimap-window-location 'right
          minimap-hide-fringes nil)

  (defun bb-minimap-mode()
    "Sets `minimap-major-mode' to the current major-mode before calling
`minimap-mode'"
    (interactive)
    (cl-pushnew major-mode minimap-major-modes)
    (minimap-mode 'toggle))

  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background unspecified)))
   '(minimap-font-face ((t :family unspecified :height 0.35)))))





;;;; `olivetti'
(use-package olivetti
  :defer 1
  :defines hl-line-mode olivetti--hl-line-mode
  :hook ((olivetti-mode-on . ensure-active-hl-line-mode)
         (olivetti-mode-off . recall-hl-line-mode-state))

  :config
  (setq-default olivetti-body-width 0.75)

  (defun ensure-active-hl-line-mode()
    ;; (defvar-local olivetti--hl-line-mode nil
    "Value of `hl-line-mode' when when `olivetti-mode' is enabled."
    (unless (bound-and-true-p olivetti--hl-line-mode)
      (set (make-local-variable 'olivetti--hl-line-mode) hl-line-mode))
    (hl-line-mode 1))

  (defun recall-hl-line-mode-state()
    (cond
     ((and olivetti--hl-line-mode (not hl-line-mode))
      (hl-line-mode 1))
     ((and (not olivetti--hl-line-mode) hl-line-mode)
      (hl-line-mode -1)))
    (kill-local-variable 'olivetti--hl-line-mode)))




;;;; `logos'
(use-package logos
  :defines logos-page-delimiter
  :after olivetti
  :hook ((logos-page-motion . prot-logos--recenter-top)
         (modus-themes-post-load . logos-update-fringe-in-buffers)
         (ef-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun prot-logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      ;; (unless ((/ (float (line-number-at-pos))(window-total-height))
      (recenter 1))) ; Use 0 for the absolute top

  (setopt logos-outlines-are-pages t
          logos-outline-regexp-alist
          `((emacs-lisp-mode . "^;;;;")
            (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
            (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
            (conf-toml-mode . "^\\[")
            (adoc-mode . "^=\\{1,6\\} \\|^\\[.*\\]$")))

  ;; the following variable are buffer-local
  (setq-default logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t))




;;;; `hl-todo'
(use-package hl-todo
  :defines hl-todo-mode-map
  :commands global-hl-todo-mode
  :defer 1
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)
              ("C-c t i" . hl-todo-insert))
  :config
  (setopt hl-todo-color-background nil
          hl-todo-keyword-faces
          '(("TODO"     . "#FFA623")
            ("NOTE"     . "#A31232")
            ("FIXME"    . "#FF4000")
            ("REVIEW"   . "#A020F0")
            ("REMINDER" . "#1111EE")
            ("TEMP"     . "#1CFF0F")
            ("HACK"     . "#1E90FF")))
  (global-hl-todo-mode))




;;;; `goggles'
(use-package goggles
  :defer 1
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing




;;;; `lin'
(use-package lin
  :defer 1
  :commands lin-global-mode
  :config
  (setopt lin-face 'lin-yellow)
  (lin-global-mode))




;;;; `rainbow-mode'
(use-package rainbow-mode
  :defer 1
  :commands rainbow-mode
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode))
  :hook ((css-mode
          css-ts-mode
          html-mode
          html-ts-mode
          js-mode
          js-ts-mode) . rainbow-mode)
  :config
  (setopt rainbow-ansi-colors nil
          rainbow-x-colors nil))




;;;; `modus-themes'
(use-package modus-themes
  :demand t)




;;;; `ef-themes'
(use-package ef-themes
  :demand t
  :defines hour-sets-theme
  :config
  (load-theme hour-sets-theme))




;;;; `breadcrumb'
(use-package breadcrumb
  :demand t
  :commands breadcrumb-mode
  :config
  (breadcrumb-mode 1))




;;;; `keycast'
(use-package keycast
  :defer 1
  :commands keycast-mode
  :defines keycast-substitute-alist
  :config
  (setopt keycast-mode-line-insert-after 'mode-line-format-right-align
          keycast-mode-line-format "%3s%k%c%R "
          keycast-mode-line-window-predicate 'mode-line-window-selected-p
          keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (cl-pushnew (list input "." "Typing…") keycast-substitute-alist))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (cl-pushnew (list event nil nil) keycast-substitute-alist)))




;;;; `which-key'
(use-package which-key
  :defer 1
  :commands which-key-mode which-key-setup-side-window-right-bottom
  :defines which-key-mode-map
  :config
  (dolist (keychords '("C-x <tab>"
                       "C-c <tab>"
                       "C-h <tab>"))
    (define-key which-key-mode-map (kbd keychords) 'which-key-C-h-dispatch))

  (setopt which-key-sort-order 'which-key-local-then-key-order
          which-key-max-description-length 0.75
          which-key-show-remaining-keys t
          which-key-use-C-h-commands nil
          which-key-side-window-max-height 0.333
          which-key-side-window-max-width 0.4
          which-key-idle-delay 0.5)
  :init
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))




;;;; `bicycle'
(use-package bicycle
  :defer 1
  :commands outline-minor-mode hs-minor-mode
  :defines outline-minor-mode-map
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-<tab>" . bicycle-cycle)
              ("<backtab>" . bicycle-cycle-global))
  :hook ((prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode)))




;;;; `pulsar'
(use-package pulsar
  :demand t
  :hook ((xref-after-return xref-after-jump) . pulsar-recenter-quarter)
  :commands pulsar-global-mode
  :functions pulsar-recenter-center pulsar-pulse-line
  :defines pulsar-pulse-functions
  :init
  (defun pulsar-recenter-quarter ()
    "Reposition point at 25% or in the 20th line of window and pulse line."
    (interactive)
    (recenter (max (ceiling (window-height) 4) 20))
    (pulsar-pulse-line))
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.05
          pulsar-iterations 50
          pulsar-face 'pulsar-green)
  (cl-pushnew 'find-file pulsar-pulse-functions)
  (pulsar-global-mode 1))




;;;; `kind-icon'
(use-package kind-icon
  :defer 1
  :defines kind-icon-use-icons kind-icon-default-face corfu-margin-formatters
  :functions kind-icon-margin-formatter
  :after corfu
  :config
  (setq kind-icon-use-icons t
        kind-icon-default-face 'corfu-default ; to compute blended backgrounds correctly
        corfu-margin-formatters '(kind-icon-margin-formatter)))



;; 
;; ;;;; `indent-guide'
;; (use-package indent-guide
;;   :defer 2
;;   :config
;;   (defun bb-maybe--get-color(arg)
;;     "It will return either a symbol or a color from the current palette."
;;     (or
;;      (car
;;       (alist-get arg
;;                  (symbol-value
;;                   (intern-soft
;;                    (format "%s-palette"
;;                            (car custom-enabled-themes))))))
;;      (face-foreground 'cursor nil 'default)))
;;
;;   (defun bb-get-color(arg)
;;     "To use with Ef or Modus themes. Get's the ARG color from the current
;; theme palette, recursively if necessary."
;;     (interactive)
;;     (let ((maybe-color (bb-maybe--get-color arg)))
;;       (if (stringp maybe-color)
;;           maybe-color
;;         (bb-maybe--get-color maybe-color))))
;;
;;   (setq indent-guide-char "")
;;
;;   (defun bb--update-indent-guide-face(_theme)
;;     (when indent-guide-mode
;;       (set-face-foreground 'indent-guide-face (bb-get-color 'cursor))))
;;   ;; Updating the indent-guide every time a theme is enabled
;;   (add-hook 'enable-theme-functions 'bb--update-indent-guide-face))




;;;; `indent-bars'
(use-package indent-bars
  :vc ( :url "https://github.com/jdtsmith/indent-bars.git"
        :rev :newest)
  :commands indent-bars-mode
  :hook (yaml-ts-mode . indent-bars-mode)
  :config
  ;; minimal colorpop
  (setopt indent-bars-color '(highlight :face-bg t :blend 0.15)
          indent-bars-pattern "."
          indent-bars-width-frac 0.2
          indent-bars-pad-frac 0.4
          indent-bars-zigzag nil
          indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
          indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
          indent-bars-display-on-blank-lines t))



(provide 'bb-ui-ux)
;;; bb-ui-ux.el ends here
