;;; init.el --- BB's config -*- lexical-binding: t -*-

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

;;; Commentary: Still a sapling dreaming of becoming a tree

;;; Code:


;;;;;;;;;;;;;;
;; Settings ;;
;;;;;;;;;;;;;;

;; Set default font
(set-face-attribute 'default nil
                    :family "AardvarkFixed Nerd Font Mono"
                    :foundry "CYEL"
                    :slant 'normal
                    :weight 'normal
                    :height 120
                    :width 'normal)

;; Adding dirs to load-path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Custom functions/libraries/modules
(require 'bb-simple)

;; Auxiliary files written in .tmp/ dir
;;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/.tmp/\\1" t)))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/.tmp/\\1" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/.tmp/backups/")))

(setq-default custom-file "~/.emacs.d/.custom.el")
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

;; User info
(setq user-full-name    "Bruno Boal"
      user-login-name   "bb"
      user-mail-address "bruno.boal@tutanota.com")

;; Nice welcome message
(setq-default initial-scratch-message
              (let ((emacs-version (replace-regexp-in-string "\s\(.*\)\n" "" (emacs-version))))
                (format ";; %s\n;; Initialization in %s\n;; %s, be disciplined and maintain focus.\n\n"
                        emacs-version (emacs-init-time "%.3fs") user-full-name)))
;;(center-region (point-min) (point-max))))    FIXME

;; User preferences
(setq delete-by-moving-to-trash t
      column-number-mode t
      display-time-24hr-format t
      display-time-mode t
      electric-pair-mode t
      ispell-dictionary nil
      sentence-end-double-space t
      sentence-end-without-period nil
      colon-double-space nil
      use-hard-newlines nil
      adaptive-fill-mode t)

(setq-default fill-column 100
			  tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct
              tab-width 4
              scroll-margin 4
              indent-tabs-mode t
              kill-do-not-save-duplicates t)

;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Select text is replaced with input
(delete-selection-mode t)

;; Disable bidirectional reordering to improve performance of file with long lines TODO
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Text scrolls progressively (trying default of 0) TRYME
(setq scroll-conservatively 101)

;; Cursor covers the actual space of a character
(setq x-stretch-cursor t)

;; No annoying sounds
(setq visible-bell t)

;; One letter answers
;;;(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; Disable exit and processes confirmation
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

;; Ctrl-K removes the whole line
(setq kill-whole-line t)

;; No warnings and restrictions
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Save clipboard before replace
(setq save-interprogram-paste-before-kill t)

;; Changing modeline from "Def" to "Macro"
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Specify which files should be reverted without query
(setq revert-without-query '(".*"))

;; Automatically focus help buffer
(setq help-window-select t)

;; Automatically loads the files if they change in another process
(global-auto-revert-mode t)

;; File name shadow mode
(file-name-shadow-mode t)

;; Custom themes are ok
(setq custom-safe-themes t)


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;;;; Initializing
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")t)

;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 1)))

(package-initialize)

;;;; `use-package'
(when (not package-archive-contents)
  (package-refresh-contents))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;; `auto-package-update'
(use-package auto-package-update
  :hook (auto-package-update-after . package-autoremove)
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 2)
  :config
  (auto-package-update-maybe))


;;;; `lin'
(use-package lin
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode))

;;;; `ediff'
(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

;;;; `magit'
(use-package magit
  :bind ("C-c s" . magit-status))

;;;; `nov'
(use-package nov)

;;;; `project'
(use-package project)

;;;; `substitute'
(use-package substitute)

;;;; `chemtable'
(use-package chemtable)

;;;; `wgrep'
;; Make grep buffers editable
(use-package wgrep
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;;;; `rainbow-mode'
(use-package rainbow-mode
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode))
  :config
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil))

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
  :bind (:map global-map
              ("C-x f" . other-frame-preffix)  ; override `set-fill-column'
              ;; Replace the generic `buffer-menu'.  With a prefix argument, this
              ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
              ;; you absolutely need the global list of buffers.
              ("C-x C-b" . beframe-buffer-menu))
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
  (beframe-mode))

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
      (keycast-mode 1)
    (keycast-mode -1))

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

;;;; `highlight-indent-guides'  ;;; TODO
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'fill))

;;;; `diredfl'
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;;; `which-key'  ;;; TODO
(use-package which-key
  :config
  (dolist (keychords '("C-x <tab>"
                       "C-c <tab>"
                       "C-h <tab>"))
    (define-key which-key-mode-map (kbd keychords) 'which-key-C-h-dispatch))

  (setq which-key-sort-order 'which-key-local-then-key-order
        ;; which-key-popup-type 'side-window
        ;; which-key-side-window-location 'right
        ;; which-key-side-window-max-width 0.35
        which-key-use-C-h-commands nil
        ;; FIXME
        ;; which-key-paging-prefixes '("C-x" "C-c" "C-h")
        ;; which-key-paging-key "<tab>"
        which-key-idle-delay 0.2)
  :init
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

;;;; `vertico'
(use-package vertico
  ;; This works with `file-name-shadow-mode'.  When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input. Works with pasting as well.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))

;;;; `savehist'
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq	history-delete-duplicates t
        history-length 1000)
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
         ("C-h a" . apropos)                       ;; orig. apropos-command
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
  (completion-category-overrides '((file (styles basic partial-completion orderless)))))

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
  ;; Hide the mode line of the Embark live/completions buffers
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
  :config
  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if MCT or Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  :hook (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer)
  :custom
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)     ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)        ;;  quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 2)        ;; Use scroll margin
  :bind
  (:map corfu-map
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
;; (use-package yasnippet
;;   :bind (:map yas-minor-mode-map
;; 			  ("ç" . yas-expand))
;;   :hook (prog-mode . yas-minor-mode)
;;   :init
;;   (setq yas-snippet-dirs
;;         '("~/.emacs.d/snippets"))
;;   :config
;;   (setq yas-also-auto-indent-first-line t
;;         yas-also-indent-empty-lines t)
;;     :custom
;;   (yas-reload-all))

;;;; TODO List/table with LSP engines
;;;; `eglot'
(use-package eglot
  :hook ((go-mode c++-mode haskell-mode) . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  (setq corfu-popupinfo-mode t
		eglot-autoshutdown t
		eglot-confirm-server-initiated-edits nil))

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

;;;; `minions'
(use-package minions
  :config
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
        (list 'defining-kbd-macro
              'flymake-mode))
  (minions-mode 1))

;;;; `envrc'
(use-package envrc
  :config
  (envrc-global-mode))

;;;;;; `editorconfig'
(use-package editorconfig
  :config
  (editorconfig-mode))

;;;; `org'
(use-package org
  :ensure nil
  :config
  (setq org-directory "~/org.d"
        org-agenda-files (directory-files-recursively
                          (concat org-directory "/agenda.d/") "\\.org\\'")
        org-default-notes-file (concat org-directory "/notes.org")
        org-export-html-postamble nil
        org-startup-indented t
        org-src-preserve-indentation t
        org-src-tab-acts-natively nil
        org-edit-src-content-indentation 0)
  :custom
  (require 'bb-org-capture))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAMMING LANGUAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; personal-programming-hooks function
(defun personal-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (highlight-indent-guides-mode t)
  (subword-mode t)
  (hs-minor-mode t))

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

  (add-hook 'project-find-functions 'project-find-root)

  :hook ((c++-mode . personal-programming-hooks)
         (before-save . (lambda() #'(cpp-auto-include) #'(eglot-format))))
  ;;  (add-hook 'c++-mode-hook 'personal-programming-hooks)
  :bind (:map c++-mode-map
              ("C-c C-c" . compile))
  ;; :config
  ;; (add-hook 'before-save-hook (lambda()
  ;;				#'(cpp-auto-include)
  ;;				#'(eglot-format)))
  :custom
  (setq-local compile-command "g++ -std=c++20 -Wall"))

;;;;;; `clang-capf'
(use-package clang-capf
  :after cape
  :hook (c-mode c++-mode objc-mode)
  :config
  (add-to-list 'completion-at-point-functions #'clang-capf))

;;;;;; `go-mode'
(use-package go-mode
  :init
  ;; (add-to-list 'exec-path "~/go/bin")
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)

  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :hook
  ((go-mode . personal-programming-hooks)
   (before-save . (lambda() #'(eglot-code-action-organize-imports 0) #'(eglot-format))))
  :custom
  (setq-local compile-command "go build -v && go test -v && go vet"
              tab-width 4))

(use-package haskell-mode)

(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))


;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;;; Window Management
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


;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)


(global-set-key (kbd "C-c 0") #'kill-emacs)
(global-set-key (kbd "<f12>") #'save-buffer)
(global-set-key (kbd "<f10>") #'save-buffers-kill-emacs)
(global-set-key (kbd "<f9>") #'menu-bar-mode)
(global-set-key (kbd "<f8>") (lambda ()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f7>") #'bb-simple-cycle-display-line-numbers)
(global-set-key (kbd "<f6>") #'whitespace-mode)
(global-set-key (kbd "<f2>") #'revert-buffer)

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
;;; init.el ends here
