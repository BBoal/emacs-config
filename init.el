;;; init.el --- BB's -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; ---
;;; Settings
;; Set default font
(set-face-attribute 'default nil
		    :family "AardvarkFixed Nerd Font Mono"
		    :foundry "CYEL"
		    :slant 'normal
		    :weight 'normal
		    :height 120
		    :width 'normal)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Org-mode
(setq org-directory "~/org.d"
      org-default-notes-file (concat org-directory "/notes.org")
      org-export-html-postamble nil
      org-startup-indented t)
(require 'bb-org-capture)

;; ediff
(setq ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;; Project
(require 'project)

;; Custom functions/libraries/modules
(require 'bb-simple)


;; Arranging things
(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
							 user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name ".tmp/"
								user-emacs-directory) t)))
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))
(setq delete-by-moving-to-trash t)

;; User info
(setq user-full-name    "Bruno Boal"
      user-login-name   "bb"
      user-mail-address "bruno.boal@tutanota.com")

;; Nice welcome message
(setq-default initial-scratch-message (format ";; Welcome %s! Be disciplined and maintain focus.\n" user-full-name)
	      kill-do-not-save-duplicates t)

;; User preferences
(setq column-number-mode t
      display-time-24hr-format t
      display-time-mode t
      electric-pair-mode t
      ispell-dictionary nil)

(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'dired-mode-hook  #'diredfl-mode)

;; Disabling restrictions
(put 'erase-buffer 'disabled nil)

;; Sentences do not end with a double space
(setq-default sentence-end-double-space nil)

;; Text scrolls progressively
(setq scroll-conservatively 1000)

;; Cursor covers the actual space of a character
(setq x-stretch-cursor t)

;; No annoying sounds
(setq visible-bell t)

;; One letter answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable line numbers, relative
(setq display-line-numbers 'relative)
					; (global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq scroll-margin 4)

;; Disable exit and processes confirmation
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

;; Ctrl-K removes the whole line
(setq kill-whole-line t)

;; No warnings
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Save clipboard before replace
(setq save-interprogram-paste-before-kill t)

;; Changing modeline from "Def" to "Macro"
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Automatically loads the files if they change in another process
(global-auto-revert-mode 1)

;; Custom themes are ok
(setq custom-safe-themes t)

(setq history-length 1000)
(put 'minibuffer-history 'history-length 500)
(put 'kill-ring 'history-length 25)


;;; Packages

;;;; Initializing
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")t)
(package-initialize)

;;;; `use-package'
;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;; `auto-package-update'
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;; `substitute'
(use-package substitute)

;;;; `chemtable'
(use-package chemtable)

;;;; `rainbow-mode'
(use-package rainbow-mode
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)
  (define-key ctl-x-x-map "c" #'rainbow-mode))

;;;; `modus-themes'
(use-package modus-themes
  :config
  (load-theme hour-sets-modus)

  ;; Clock in the modeline
  (setq display-time-string-forms
	'((propertize (concat " " 24-hours ":" minutes " ")
		      'face 'keycast-key)))
  (display-time-mode 1))

(setq tab-bar-format                    ; Emacs 28
      '(tab-bar-format-tabs-groups
	tab-bar-format-align-right
	tab-bar-format-global))
;;global-mode-string (TODO)

;;;; `beframe'
(use-package beframe
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `( :name     "Frame-specific buffers (current frame)"
	 :narrow   ?F
	 :category buffer
	 :face     beframe-buffer
	 :history  beframe-history
	 :items    ,#'beframe--buffer-names
	 :action   ,#'switch-to-buffer
	 :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source))
  (let ((map global-map))
    (define-key map (kbd "C-x f") #'other-frame-prefix) ; override `set-fill-column'
    ;; Replace the generic `buffer-menu'.  With a prefix argument, this
    ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
    ;; you absolutely need the global list of buffers.
    (define-key map (kbd "C-x C-b") #'beframe-buffer-menu))
  (beframe-mode 1))

;;;; `keycast'
(use-package keycast
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
      (keycast-mode-line-mode 1)
    (keycast-mode -1))

  (dolist (input '(self-insert-command
		   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
		   mouse-movement-p
		   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

;;;; `highlight-indent-guides'
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character))

;;;; `avy'
(use-package avy
  :bind ("C-ç" . avy-goto-char-2))

;;;; `which-key'
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;;;; `vertico'
(use-package vertico
  :init
  (vertico-mode))

;;;; `savehist'
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;;; `consult'
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("C-h a" . consult-apropos)               ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

;;;; `orderless'
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; `marginalia'
(use-package marginalia
  :config
  (marginalia-mode))

;;;; `embark'
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for \`describe-bindings\'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
					; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;;;; `embark-consult'
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;;; `org-modern'
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))


;;;; `corfu'
(use-package corfu
  :custom
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 4)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)     ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;;  quit, even if there is no match
  ;; (corfu-preview-current nil) ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match t)       ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  (:map corfu-map
	("ç" . yas-expand)
	("J" . corfu-next)
	("K" . corfu-previous))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;;;; `dabbrev'
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;; `cape'
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
	 ("M-p t" . complete-tag)        ;; etags
	 ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("M-p h" . cape-history)
	 ("M-p f" . cape-file)
	 ("M-p k" . cape-keyword)
	 ("M-p s" . cape-symbol)
	 ("M-p a" . cape-abbrev)
	 ("M-p &" . cape-sgml))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;;;; `yasnippet'
(use-package yasnippet
  :defer t
  :bind ("ç" . yas-expand)
  :init
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t))

;;;; `eglot'
(use-package eglot
  :defer t
  :init
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (corfu-popupinfo-mode t))

;;;; `consult-eglot'
(use-package consult-eglot
  :after (consult eglot))

;;;; `bicycle'
(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
	      ("C-<tab>" . bicycle-cycle)
	      ("<backtab>" . bicycle-cycle-global))
  :hook ((prog-mode elisp-mode) .
	 (lambda()
	   #'(outline-minor-mode)
	   #'(hs-minor-mode))))

;;;; `beacon'
(use-package beacon
  :config
  (beacon-mode 1))

;;;; `kind-icon'
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; `chezmoi'
;; TODO: Check how to integrate with CLI tool
;; (use-package chezmoi)

(use-package minions
  :config
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
	(list 'defining-kbd-macro
	      'flymake-mode))
  (minions-mode 1))

;;;; ---
;;;; PROGRAMMING LANGUAGES
;;;;;; `editorconfig'
(use-package editorconfig
  :config
  (editorconfig-mode t))

;;;;;; personal-programming-hooks function
(defun personal-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (highlight-indent-guides-mode t)
  (electric-pair-local-mode t)
  (subword-mode t)
  (hs-minor-mode t))

;;;;; C++
;;;;;; `cpp-auto-include'
(use-package cpp-auto-include)

;;;;;; `cc-mode'
(use-package cc-mode
  :init
  (defun project-find-root (dir)
    (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'CMakeLists root)))

  (cl-defmethod project-root ((project (head CMakeLists)))
    (cdr project))

  (add-hook 'c++-mode-hook 'personal-programming-hooks)
  :bind (:map c++-mode-map
	      ("C-c C-c" . compile))
  :config
  (add-hook 'project-find-functions 'project-find-root)
  (add-hook 'before-save-hook (lambda()
				(cpp-auto-include)
				#'(eglot-format)))
  :custom
  (setq compile-command "g++ -std=c++20 -Wall"))

;;;;;; `clang-capf'
(use-package clang-capf
  :after cape
  :hook (c-mode c++-mode objc-mode)
  :config
  (add-to-list 'completion-at-point-functions #'clang-capf))

;;;;; Go
;;;;;; `go-mode'
(use-package go-mode
  :init
  (add-to-list 'exec-path "~/go/bin")
  (defun project-find-go-work (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-mod root)))

  (cl-defmethod project-root ((project (head go-mod)))
    (cdr project))

  (add-hook 'go-mode-hook (lambda()
			    (personal-programming-hooks)
			    (setq-local tab-width 4)))
  :bind (:map go-mode-map
	      ("C-c C-c" . compile))
  :config
  (add-hook 'project-find-functions 'project-find-go-work)
  (add-hook 'before-save-hook (lambda()
				#'(eglot-code-action-organize-imports 0)
				#'(eglot-format)))
  :custom
  (setq compile-command "go build -v && go test -v && go vet"))

;;; Useful functions
;;;; Window Management
(defun split-window-right-and-focus ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))
(global-set-key (kbd "C-c -")  'split-window-right-and-focus)

(defun split-window-below-and-focus ()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))
(global-set-key (kbd "C-c \\") 'split-window-below-and-focus)

(defun kill-buffer-and-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))
(global-set-key (kbd "C-c q") 'kill-buffer-and-delete-window)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;;; Keybindings
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "<f2>") #'revert-buffer)
(global-set-key (kbd "<f9>") #'menu-bar-mode)
(global-set-key (kbd "<f8>") (lambda ()
			       (interactive)
			       (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c 0") #'kill-emacs)
(global-set-key (kbd "<f12>") #'save-buffer)
(global-set-key (kbd "<f10>") #'save-buffers-kill-emacs)

(global-set-key (kbd "C-c <up>")    #'windmove-up)
(global-set-key (kbd "C-c <down>")  #'windmove-down)
(global-set-key (kbd "C-c <left>")  #'windmove-left)
(global-set-key (kbd "C-c <right>") #'windmove-right)

(global-set-key (kbd "C-M-=") #'count-words)

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; (define-key map (kbd "<tab>")    #'log-view-toggle-entry-display)
;; (define-key map (kbd "<return>") #'log-view-find-revision)
;; (define-key map (kbd "s")        #'vc-log-search)


(provide 'init)
;;; ---
;;; init.el ends here
