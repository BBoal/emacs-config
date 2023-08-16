;;;; bb-ui-ux.el --- Improving UI and UX -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration of packages related to embelish and empower emacs UI/UX

;;; Code:


;;;; modus-themes personal preferences
(setq modus-operandi-tinted-palette-overrides
      '((cursor blue-intense)
        (bg-mode-line-active "#d0d6ff")
        (bg-mode-line-inactive "#e6e6e6")))



;;;; `desktop-mode'

;;;;; Customizations
(defconst default-name-desktop-dir ".emacs-desktop.d/"
  "Default name for directories that hold desktop files.")

(defconst default-desktop-dirname (concat "~/" default-name-desktop-dir)
  "Directory that holds all desktop files not referencing specific projects.")

;;;;; Functions
(defun bb--last-modified-in-dir (&optional directory)
  "Return the last modified file (full-path) in DIRECTORY."
  (interactive "DDirectory: ")
  (or directory (setq directory desktop-dirname))
  (cond
   ((not (file-directory-p directory))
    (user-error "Error: Directory doesn't seem to exist."))
   ((not (file-accessible-directory-p directory))
    (user-error "Error: Unable to open directory. Check permissions."))
   (t
    (let (files modified-times latest-file)
      (mapc
       (lambda (attr)
         (let ((names (expand-file-name (car attr))) ; expand-file-name takes '.' and '..'
               (times (nth 5 attr))) ; 5th position is the modified-time attribute
           (setq files (cons (cons names times)
                             files)
                 modified-times (cons times modified-times))))
      (directory-files-and-attributes directory :full)) ; list of filenames and attrs
    ;; sort the times in descending order to get the most recent first
    (setq modified-times (nreverse (sort modified-times #'time-less-p))
          latest-file (car (rassq (car modified-times) files)))
    ;; What if the directory is empty? latest-file = directory
    (when (string= latest-file (expand-file-name directory))
      (setq latest-file nil))
    ;; Output the result
    (if (called-interactively-p)
        (message latest-file)
      latest-file)))))


(defun bb-set-desktop-file-for-save()
  "Avoids the *temp* buffer names and sets the correct name for read and save."
  (setq desktop-base-file-name
        (concat (format-time-string "%d%b%Y-")
                (if-let ((buf-name (buffer-name))
                         ((not (string-match "\\*[[:word:]]*\*" buf-name))))
                    buf-name
                  (buffer-name (window-buffer))))))


(defun bb-set-desktop-file-for-read (&optional directory filename)
  "Set the desktop FILENAME name to be loaded from designated DIRECTORY.

This function can be called interactively with completion for the mentioned
variables or in lisp code by assigning the arguments.

Interactively, if visiting a file, the function searches for `default-name-desktop-dir' from present dir proceeding up in the hierarchy file structure. If DIRECTORY is found, the latest modified FILENAME is suggested.

If no arguments are passed, DIRECTORY defaults to `desktop-dirname' and FILENAME
  defaults to the latest modified filename inside that directory."
  (interactive
   (let*
       ((dir (or (if buffer-file-name
                     (locate-dominating-file (file-name-directory buffer-file-name)
                                             default-name-desktop-dir))
                 desktop-dirname))
        (file (read-file-name "Choose/Insert a filename to load: " dir nil nil
                              (file-name-nondirectory (bb--last-modified-in-dir dir))))))
   (list (file-name-directory file) file))
  ;; If not interactive
  (or directory (setq directory desktop-dirname))
  (or filename (setq filename (bb--last-modified-in-dir directory)))
  ;; Error checking
  (cond
   ((not (file-directory-p directory))
    (user-error "Error: Directory doesn't seem to exist."))
   ((not (file-accessible-directory-p directory))
    (user-error "Error: Unable to open directory. Check permissions."))
   ((not (file-exists-p filename))
    (user-error "Error: Filename doesn't seem to exist."))
   ((not (file-readable-p filename))
    (user-error "Error: Unable to open directory. Check permissions."))
   (t
    (setq desktop-base-file-name filename) ; sets file to load
    (desktop-release-lock desktop-dirname) ; releases lock if there's a desktop already loaded
    (desktop-read directory)               ; actual loading of FILENAME in DIRECTORY
    ;; Refreshes the loaded theme to avoid theme collision
    (load-theme (car custom-enabled-themes)))))

;; (defun bb-set-desktop-file-for-read (&optional directory)
;;   "Set the desktop file name to be the latest file modified on desktop-dirname"
;;   (interactive "DChoose a directory to load from: ")
;;   (unless directory (setq directory desktop-dirname))
;;   (if (not (file-directory-p directory))
;;       (user-error "Directory not found... Was it a typo? ")
;;     (setq desktop-base-file-name
;;           (bb--last-modified-in-dir directory))
;;     (desktop-read directory)
;;     (load-theme (car custom-enabled-themes))))


;;;;; Settings
(setq desktop-dirname (bb-ensure-dir-or-file-exist default-desktop-dirname)
      desktop-path `(,desktop-dirname)
      desktop-file-name-format 'absolute
      desktop-load-locked-desktop nil
      desktop-globals-to-save nil
      desktop-globals-to-clear nil
      desktop-lazy-idle-delay 1
      desktop-locals-to-save '(desktop-locals-to-save
                               tab-bar-show
                               truncate-lines
                               case-fold-search
                               case-replace
                               fill-column
                               overwrite-mode
                               change-log-default-name
                               line-number-mode
                               size-indication-mode
                               buffer-file-coding-system
                               indent-tabs-mode
                               tab-width
                               indicate-buffer-boundaries
                               indicate-empty-lines
                               show-trailing-whitespace)
      desktop-modes-not-to-save '(tags-table-mode)
      desktop-restore-eager 6
      ;;desktop-restore-reuses-frames 'keep
      desktop-auto-save-timeout 0
      desktop-save t)

(defun bb-silent-desktop-read()
  "xiiu"
  (interactive)
  (let ((inhibit-message t))
    (desktop-read)))

;;;;; Hooks for `desktop-mode'
(add-hook 'desktop-save-hook #'bb-set-desktop-file-for-save)
(add-hook 'desktop-no-desktop-file-hook #'bb-set-desktop-file-for-read)



;;;; `tab-bar-mode'
;; 2023-08-13  TODO => Improve to include choices such as *scratch*
(defun bb--tab-bar-new-tab-choice()
  "Allows the user to choose the type of the next new tab."
  (call-interactively 'find-file))

(defun bb--tab-bar-new-tab-group()
  "Sets the group of the new tab to the name of the parent directory of file."
  (if (buffer-file-name (current-buffer))
      ;; (file-name-base buffer-file-name)
      (abbreviate-file-name (file-name-parent-directory buffer-file-name))
    (buffer-name)))

(defun bb--tab-bar-tab-group-format-default (tab i &optional current-p)
  (propertize
   (concat (if (and tab-bar-tab-hints (not current-p)) (format " [%d]  " i) "")
           (funcall tab-bar-tab-group-function tab))
   'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

(defun bb--tab-bar-tab-name-format-default (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (string (concat (if tab-bar-tab-hints (format "%d\) " i) "")
                         (alist-get 'name tab)
                         (or (and tab-bar-close-button-show
                                  (not (eq tab-bar-close-button-show
                                           (if current-p 'non-selected 'selected)))
                                  tab-bar-close-button)
                             "")))
         (dif-widthmax-widthstring (- (cadr tab-bar-auto-width-max) (string-width string)))
         (spaces2add (if (<= dif-widthmax-widthstring 0)
                         0
                       (floor dif-widthmax-widthstring 2))))
    (propertize
     (concat (make-string spaces2add ?\ ) string)
     'face (funcall tab-bar-tab-face-function tab))))


(setq tab-bar-auto-width-max '(220 27) ; tweaked with (string-pixel-width (make-string 27 ?\=))
      tab-bar-close-button-show 'selected
      tab-bar-close-last-tab-choice 'delete-frame
      tab-bar-close-tab-select 'left
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-add-tab
                       tab-bar-format-align-right
                       tab-bar-format-global)
      tab-bar-new-tab-choice 'bb--tab-bar-new-tab-choice
      tab-bar-new-tab-group 'bb--tab-bar-new-tab-group
      tab-bar-select-tab-modifiers '(super)
      tab-bar-tab-group-format-function 'bb--tab-bar-tab-group-format-default
      tab-bar-tab-group-function 'tab-bar-tab-group-default
      tab-bar-tab-hints t
      tab-bar-tab-name-format-function 'bb--tab-bar-tab-name-format-default
      tab-bar-tab-name-function 'tab-bar-tab-name-truncated
      tab-bar-tab-name-truncated-max (cadr tab-bar-auto-width-max)
      tab-bar-show t)
(tab-bar-mode t)



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
