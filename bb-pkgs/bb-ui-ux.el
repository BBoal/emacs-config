;;;; bb-ui-ux.el --- Improving UI and UX -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration of packages related to embelish and empower emacs UI/UX

;;; Code:


;;;; modus-themes personal preferences
(setq modus-operandi-tinted-palette-overrides
      '((cursor blue-intense)
        (bg-mode-line-active "#d0d6ff")
        (bg-mode-line-inactive "#e6e6e6")))




;;;; `olivetti'
(use-package olivetti
  :defer 3
  :hook ((olivetti-mode-on . ensure-active-hl-line-mode)
         (olivetti-mode-off . recall-hl-line-mode-state))

  :config
  (setq-default olivetti-body-width 0.75)

  (defun ensure-active-hl-line-mode()
    (defvar-local olivetti--hl-line-mode nil
      "Value of `hl-line-mode' when when `olivetti-mode' is enabled.")
    (unless (bound-and-true-p olivetti--hl-line-mode)
      (setq olivetti--hl-line-mode hl-line-mode))
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
  :defer 3
  :after olivetti
  :hook ((logos-page-motion . prot-logos--recenter-top)
         (modus-themes-post-load . logos-update-fringe-in-buffers)
         (ef-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun prot-logos--recenter-top()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
   ;; (unless ((/ (float (line-number-at-pos))(window-total-height))
      (recenter 1))) ; Use 0 for the absolute top

  (setq logos-outlines-are-pages t
        logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;; ")
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




;;;; `desktop-mode'
;;;;; Customizations
(defcustom default-name-desktop-dir ".emacs-desktop.d/"
  "Default name for directories that hold desktop files.")

(defcustom default-desktop-dirname (concat user-emacs-directory default-name-desktop-dir)
  "Directory that holds all desktop files not referencing specific projects.")


;;;;; Hooks for `desktop-mode'
;; (add-hook 'desktop-save-hook #'bb-set-desktop-file-for-save)
;; (add-hook 'desktop-no-desktop-file-hook #'bb-set-desktop-file-for-read)


;;;;; Settings
(setq desktop-dirname (bb-ensure-dir-or-file-exist default-desktop-dirname)
      desktop-path `(,desktop-dirname) ;; 2023-09-02 TODO => Use this var to check for desktop-mode savefile

      desktop-file-name-format 'absolute
      ;; desktop-load-locked-desktop nil
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
      ;; desktop-restore-reuses-frames 'keep
      desktop-auto-save-timeout 0
      desktop-save t)


;;;;; Functions
(defun bb-last-modified-in-dir (&optional directory)
  "Return the last modified file (full-path) in DIRECTORY."
  (interactive "DDirectory: ")
  (or directory
      (setq directory (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        (read-directory-name "Choose a directory: "))))
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
      (setq latest-file ""))
    ;; Output the result
    (if (called-interactively-p)
        (message latest-file)
      latest-file)))))


(defun bb--choose-dir-desktop-save ()
  "Helper to set a possible directory string for saving desktop layout files"
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-desktop-dirname))
         (possible-dir (locate-dominating-file dir default-name-desktop-dir))
         (default-dir (abbreviate-file-name
                       (file-name-parent-directory default-desktop-dirname))))

    (when (not possible-dir)
      (let ((answer
             (yes-or-no-p
              "Unsure where to save desktop file: press 'y' to choose dir, 'n' to save in default")))
        (setq possible-dir (if answer
                               (file-name-as-directory
                                (read-directory-name "Insert directory where desktop-dir should be created: " dir))
                             default-dir))))
    (concat possible-dir default-name-desktop-dir)))


;; 2023-08-24 NOTE => Improve to allow user to choose existing .emacs-desktop
;; dir without creating another similar directory inside of the previous one.
(defun bb-set-desktop-file-for-save(&optional directory filename)
  "Avoids the *temp/scratch* buffer names and sets the directory and filename for
`desktop-save'."
  (interactive)

  (or directory (setq directory (bb--choose-dir-desktop-save)))
  (let* ((parent-dir (file-name-parent-directory directory))
         (existing-parent-dir (progn
                                (while (not (file-directory-p parent-dir))
                                  (setq parent-dir (file-name-parent-directory parent-dir)))
                                parent-dir))
         (writable-existing-parent-dir (file-writable-p existing-parent-dir))
         (dir-exists (file-directory-p directory))
         (dir-writable (file-writable-p directory)))
    (cond
     ((and dir-exists (not dir-writable))
      (user-error "Error: Unable to write to directory. Check permissions"))
     ((and (not dir-exists) writable-existing-parent-dir)
      (if (yes-or-no-p "Directory doesn't exist but can be created. Proceed? ")
          (progn
            (princ (format "Creating %s  ...  " directory))
            (unless (make-directory directory :parents) ;; meaning directory was created
              (princ "Done!")))
        (user-error "Exiting without creating dir")))
     ((and (not dir-exists) (not writable-existing-parent-dir))
      (user-error "Error: Unable to create child directory. Check permissions of parent dir"))
     (t
      (princ "Processing desktop file... ")))

    ;; following block of code must be outside of `t' othewise when the
    ;; directory has to be created the filename will not be processed.
    (or filename
        (setq filename (file-name-with-extension
                        (concat
                         (format-time-string "%d%b%Y-")
                         (if-let ((buf-name (buffer-name))
                                  ((not (string-match "\\*[[:word:]]+\*" buf-name))))
                             buf-name
                           (buffer-name (window-buffer)))) "desktop")))
    ;; (add-to-list 'desktop-path directory)
    (setq desktop-dirname directory
          desktop-base-file-name filename)
    (desktop-save directory :release)
    (princ (format "Desktop file saved as %s%s"
                   desktop-dirname desktop-base-file-name))))


(defun bb-set-desktop-file-for-read (&optional directory filename)
  "Set the desktop FILENAME name to be loaded from designated DIRECTORY.

This function can be called interactively with completion for the mentioned
variables or in lisp code by assigning the arguments.

Interactively, if visiting a file, the function searches for
`default-name-desktop-dir' from present dir proceeding up in the hierarchy file
structure. If DIRECTORY is found, the latest modified FILENAME is suggested.

If no arguments are passed, DIRECTORY defaults to `desktop-dirname' and
  FILENAME defaults to the latest modified filename inside that directory."
  (interactive
   (let*
       ((possible-dir
         (when buffer-file-name
           (locate-dominating-file (file-name-directory buffer-file-name)
                                   default-name-desktop-dir)))
        (final-dir (if possible-dir
                       (concat possible-dir default-name-desktop-dir)
                     desktop-dirname))
        (file
         (read-file-name "Choose/Insert a filename to load: " final-dir nil nil
                         (file-name-nondirectory (bb-last-modified-in-dir final-dir)))))
     (list (file-name-directory file) (file-name-nondirectory file))))
  ;; If not interactive
  (or directory (setq directory desktop-dirname))
  (or filename (setq filename (bb-last-modified-in-dir directory)))
  ;; Error checking
  (let ((full-path (concat directory filename)))
    (cond
     ((not (stringp directory))
      (user-error "Error: Directory name should be passed as 'string'"))
     ((not (file-directory-p directory))
      (user-error "Error: Directory doesn't seem to exist."))
     ((not (file-accessible-directory-p directory))
      (user-error "Error: Unable to open directory. Check permissions."))
     ((not (file-exists-p full-path))
      (user-error "Error: Filename doesn't seem to exist."))
     ((not (file-readable-p full-path))
      (user-error "Error: Unable to read file. Check permissions."))
     (t
      ;; sets file to load
      (ignore-errors
        (desktop-release-lock))         ; releases lock if there's a desktop already loaded
      (add-to-list 'desktop-path directory)
      (setq desktop-dirname directory
            desktop-base-file-name filename)
      (desktop-read)                    ; actual loading of FILENAME in DIRECTORY
      ;; I don't want the filename to be added to file-name-history
      (setq file-name-history (delete filename file-name-history))
      ;; Refreshes the loaded theme to avoid theme collision
      (load-theme (car custom-enabled-themes))))))


(defun bb-silent-desktop-read()
  "Kill the `No desktop file message` minibuffer message"
  (interactive)
  (call-interactively 'bb-set-desktop-file-for-read))




;;;; `tab-bar-mode'
;;;;; Settings
(setq tab-bar-auto-width-max '(220 27) ; tweaked with (string-pixel-width (make-string 27 ?\=))
      tab-bar-close-button-show 'selected
      tab-bar-close-last-tab-choice 'delete-frame
      tab-bar-close-tab-select 'left
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
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

;;;;; Functions
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
         (string (concat (if tab-bar-tab-hints (format "%d\. " i) "")
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

;;;;; Enable the tab-bar
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
  (setq lin-face 'lin-yellow)
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

(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(defun bb-modeline--style-major-mode()
  "Return string referring `major-mode', capitalized and with \"-mode\" deleted."
  (capitalize
   (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local bb-modeline-major-mode
  '(:eval
    (list (propertize (bb-modeline--style-major-mode)
                      'mouse-face 'mode-line-highlight)
          '("" mode-line-process)
          " "))
  "Mode line construct for displaying major modes.")

(defvar-local prot-modeline-vc-branch
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


(defvar-local prot-modeline-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                         (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.") ;; 2023-06-30  REMINDER => See this in detail


(defvar-local prot-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar-local prot-modeline-kbd-macro
  '(:eval
    (when (and defining-kbd-macro (mode-line-window-selected-p))
      mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")


(defvar-local prot-modeline-buffer-identification
  (propertized-buffer-identification "%b")
  "Mode line construct for identifying the buffer being displayed.")


(defvar-local prot-modeline--line-and-column
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


(defvar-local prot-modeline-position
  '(:eval
    (when (mode-line-window-selected-p)
      prot-modeline--line-and-column))
  "Mode line construct for the buffer position.")

;; Line/Column Pos%
(setq mode-line-position-column-line-format '("%l:%c "))
(setq mode-line-percent-position '(-3 "%o"))
(column-number-mode 1)


(defvar-local prot-modeline-flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode)
               (mode-line-window-selected-p))
      ;;(flatten-tree (cons flymake-mode-line-format "  "))))
      ;;flymake-mode-line-format))
      (list flymake-mode-line-exception flymake-mode-line-counters " ")))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")


;; Final assembly for the mode-line
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

(defcustom bb--mode-line-defining-strings
  '(prot-modeline-kbd-macro
    prot-modeline-buffer-identification
    bb-modeline-major-mode
    prot-modeline-flymake
    prot-modeline-position
    prot-modeline-vc-branch
    prot-modeline-align-right
    prot-modeline-misc-info)
  "List of variables that compose and define the mode-line-format")


;; Making the variables that compose the mode-line 'risky-local-variable. This
;; is mandatory, otherwise they will not be displayed.
(mapc (lambda (variable)
        (put variable 'risky-local-variable t))
      bb--mode-line-defining-strings)

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
  :demand t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.03
        pulsar-iterations 40
        pulsar-face 'pulsar-green)
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
  ;; Updating the indent-guide every time a theme is enabled
  (add-hook 'enable-theme-functions 'bb--update-indent-guide-face))



(provide 'bb-ui-ux)
;;; bb-ui-ux.el ends here
