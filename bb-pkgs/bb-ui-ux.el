;;; bb-ui-ux.el --- Improving UI and UX -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration of packages related to embelish and empower emacs UI/UX

;;; Code:


;;;; `hl-todo'
(use-package hl-todo
  :defer 3
  :config
  (setq hl-todo-color-background nil
        hl-todo-keyword-faces
        '(("TODO"     . "#FFA623")
          ("NOTE"     . "#A31232")
          ("FIXME"    . "#FF4000")
          ("REVIEW"   . "#A020F0")
          ("REMINDER" . "#1111EE")
          ("TEMP"     . "#1CFF0F")
          ("HACK"     . "#1E90FF")))
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)
              ("C-c t i" . hl-todo-insert))
  :init
  (global-hl-todo-mode))


;;;; `goggles'
(use-package goggles
  :defer 3
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing


;;;; `lin'
(use-package lin
  :defer 2
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode))


;;;; `rainbow-mode'
(use-package rainbow-mode
  :defer 2
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode))
  :hook ((css-mode html-mode sass-mode) . rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil))


;;;; `keycast'
(use-package keycast
  :demand t
  :config
  (defun prot/keycast-current-window-p ()
    "Return non-nil if selected WINDOW modeline can show keycast."
    (and (not (minibufferp))
         (not (null mode-line-format))
         (eq (selected-window) (old-selected-window))))
  (setq keycast-mode-line-window-predicate #'prot/keycast-current-window-p)
  (setq keycast-separator-width 1)
  (setq keycast-mode-line-remove-tail-elements nil)
  (if (not (keycast--mode-active-p))
      (keycast-mode 1)
    (keycast-mode -1))

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))


;;;; `modus-themes'
(use-package modus-themes
  :demand t
  :after keycast
  :config
  (load-theme hour-sets-modus)
  ;; Clock in the modeline
  (setq display-time-string-forms
        '((propertize (concat " " 24-hours ":" minutes " ")
                      'face #'keycast-key)))
  (display-time-mode 1))

;; tab-bar tweaks
;; (setq tab-bar-format               ;; Emacs 28
;;      '(tab-bar-format-tabs-groups
;;        tab-bar-format-align-right
;;        tab-bar-format-global))
;;global-mode-string (TODO)


;;;; `ef-themes'
(use-package ef-themes)


;;;; `which-key'
(use-package which-key
  :defer 1
  :config
  (dolist (keychords '("C-x <tab>"
                       "C-c <tab>"
                       "C-h <tab>"))
    (define-key which-key-mode-map (kbd keychords) 'which-key-C-h-dispatch))

  (setq which-key-sort-order 'which-key-local-then-key-order
        which-key-max-description-length 0.75
        which-key-show-remaining-keys t
        which-key-use-C-h-commands nil
        which-key-side-window-max-height 0.333
        which-key-side-window-max-width 0.4
        which-key-idle-delay 0.2)
  :init
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))


;;;; `bicycle'
(use-package bicycle
  :defer 2
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-<tab>" . bicycle-cycle)
              ("<backtab>" . bicycle-cycle-global))
  :hook ((prog-mode elisp-mode) .
         (lambda()
           #'(outline-minor-mode)
           #'(hs-minor-mode))))


;;;; `pulsar'
(use-package pulsar
  :defer 2
  :demand t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 20
        pulsar-face 'pulsar-green
        pulsar--face 'pulsar-yellow)
  (pulsar-global-mode 1))


;;;; `kind-icon'
(use-package kind-icon
  :defer 1
  :after corfu
  :config
  (setq kind-icon-use-icons t
        kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



;;;; `minions'
(use-package minions
  :defer 2
  :config
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
        (list 'defining-kbd-macro
              'flymake-mode))
  :init
  (minions-mode t))



;;;; `indent-guide'
(use-package indent-guide
  :defer 2
  :config
  (defun bb-maybe--get-color(arg)
    "It will return either a symbol or a color from the current palette."
    (car
     (alist-get arg
                (symbol-value
                 (intern-soft
                  (format "%s-palette"
                          (car custom-enabled-themes)))))))

  (defun bb-get-color(arg)
    "To use with Ef or Modus themes. Get's the ARG color from the current
theme palette, recursively if necessary."
    (interactive)
    (let ((maybe-color (bb-maybe--get-color arg)))
      (if (stringp maybe-color)
          maybe-color
        (bb-maybe--get-color maybe-color))))

  (setq indent-guide-char "")

  (defun bb--update-indent-guide-face(_theme)
    (when indent-guide-mode
      (set-face-foreground 'indent-guide-face (bb-get-color 'cursor))))
  (add-hook 'enable-theme-functions 'bb--update-indent-guide-face))



(provide 'bb-ui-ux)
;;; bb-ui-ux.el ends here
