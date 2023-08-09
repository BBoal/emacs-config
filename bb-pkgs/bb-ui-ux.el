;;; bb-ui-ux.el --- Improving UI and UX -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration of packages related to embelish and empower emacs UI/UX

;;; Code:


(setq modus-operandi-tinted-palette-overrides
      '((cursor blue-intense)
        (bg-mode-line-active "#d0d6ff")
        (bg-mode-line-inactive "#e6e6e6")))


;;;; `hl-todo'
(use-package hl-todo
  :defer 1
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)
              ("C-c t i" . hl-todo-insert))
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
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode))


;;;; `rainbow-mode'
(use-package rainbow-mode
  :defer 1
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode))
  :hook ((css-mode
          css-ts-mode
          html-mode
          html-ts-mode
          js-mode
          js-ts-mode) . rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil))


;;;; `modus-themes'
(use-package modus-themes)


;;;; `ef-themes'
(use-package ef-themes
  :demand t
  :config
  (load-theme hour-sets-theme))



;;;; `modeline-setup'
(defvar bb-modeline-major-mode
  (list (propertize "%[" 'face 'error)
        '(:eval
          (propertize
           (capitalize
            (replace-regexp-in-string
             "-mode"
             ""
             (symbol-name major-mode)))
           'mouse-face 'mode-line-highlight))
        '("" mode-line-process)
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defvar prot-modeline-vc-branch
  '(:eval
    (when-let* (((mode-line-window-selected-p))
                (branches (vc-git-branches))
                (branch (car branches))
                ((stringp branch))
                (state (vc-state (buffer-file-name) 'Git))
                (face (pcase state
                        ('added 'vc-locally-added-state)
                        ('edited 'vc-edited-state)
                        ('removed 'vc-removed-state)
                        ('missing 'vc-missing-state)
                        ('conflict 'vc-conflict-state)
                        ('locked 'vc-locked-state)
                        (_ 'vc-up-to-date-state))))
      (concat "   " (propertize (capitalize branch)
                               'face face
                               'mouse-face 'mode-line-highlight))))
  "Mode line construct to return propertized VC branch.")


(defvar prot-modeline-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                         (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.") ;; 2023-06-30  REMINDER => See this in detail


(defvar prot-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")


(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar prot-modeline-kbd-macro
  '(:eval
    (when (and defining-kbd-macro (mode-line-window-selected-p))
      mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")


(defvar prot-modeline-buffer-identification
  (propertized-buffer-identification "%b")
  "Mode line construct for identifying the buffer being displayed.")


(defvar prot-modeline--line-and-column
  `((line-number-mode
     (column-number-mode
      (column-number-indicator-zero-based
       (:propertize
        mode-line-position-column-line-format
        ,@mode-line-position--column-line-properties)
       (:propertize
        (:eval (string-replace
                "%c" "%C" (car mode-line-position-column-line-format)))
        ,@mode-line-position--column-line-properties))
      (:propertize
	   mode-line-position-line-format
       ,@mode-line-position--column-line-properties))
     (column-number-mode
      (:propertize
       mode-line-position-column-format
       ,@mode-line-position--column-line-properties)
      (:propertize
       (:eval (string-replace
               "%c" "%C" (car mode-line-position-column-format)))
       ,@mode-line-position--column-line-properties)))
    " "
    (:propertize
     ("" mode-line-percent-position)
     mouse-face mode-line-highlight)
    " ")
  "Mode line construct for formatting `prot-modeline-position'.")


(defvar prot-modeline-position
  '(:eval
    (when (mode-line-window-selected-p)
      prot-modeline--line-and-column))
  "Mode line construct for the buffer position.")

;; Line/Column Pos%
(setq mode-line-position-column-line-format '("%l:%c "))
(setq mode-line-percent-position '(-3 "%o"))
(column-number-mode 1)


(defvar prot-modeline-flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode)
               (mode-line-window-selected-p))
      ;;(flatten-tree (cons flymake-mode-line-format "  "))))
      ;;flymake-mode-line-format))
      (list flymake-mode-line-exception flymake-mode-line-counters " ")))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")


(dolist (variables '(bb-modeline-major-mode
                     prot-modeline-align-right
                     prot-modeline-buffer-identification
                     prot-modeline-flymake
                     prot-modeline-kbd-macro
                     prot-modeline-misc-info
                     prot-modeline-position
                     prot-modeline-vc-branch))
  (put variables 'risky-local-variable t))


(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                prot-modeline-buffer-identification
                "  "
                bb-modeline-major-mode
                prot-modeline-flymake
                " "
                prot-modeline-position
                prot-modeline-vc-branch
                prot-modeline-align-right
                prot-modeline-misc-info ;; everything else not defined particularly
                mode-line-end-spaces))


;; Clock in the modeline
(setq display-time-string-forms
      '((capitalize (format-time-string " %a,%d %b %R "))))
(display-time-mode 1)


;;;; `keycast'
(use-package keycast
  :defer 2
  :config
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-format "%3s%k%c%R")
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))





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
        which-key-idle-delay 0.5)
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



;;;; `indent-guide'
(use-package indent-guide
  :defer 2
  :config
  (defun bb-maybe--get-color(arg)
    "It will return either a symbol or a color from the current palette."
    (or
     (car
     (alist-get arg
                (symbol-value
                 (intern-soft
                  (format "%s-palette"
                          (car custom-enabled-themes))))))
     (face-foreground 'cursor nil 'default)))

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
