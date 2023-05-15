;;; init.el --- BB's config -*- lexical-binding: t -*-

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

;;; Commentary: Still a sapling dreaming of becoming a tree

;;; Code:


;;;;;;;;;;;;;;
;; Settings ;;
;;;;;;;;;;;;;;


;; Set default font
(set-face-attribute 'default nil
                    :family "Iosevka Zenodotus Fixed"
                    :height 130)

;; (set-frame-font "AardvarkFixed Nerd Font Mono 13" nil t t)

;; Custom functions/libraries/modules
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'bb-simple)

;;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setq create-lockfiles nil
	  make-backup-files nil
	  auto-save-file-name-transforms
	  `(("\\`/.*/\\([^/]+\\)\\'"
		 ,(concat (locate-user-emacs-file ".tmp/")"\\1") t)))

;; Nice welcome message
(setq-default initial-scratch-message
              (let ((emacs-version (replace-regexp-in-string "\s\(.*\)\n" "" (emacs-version))))
                (format ";; %s\n;; Initialization in %s\n;; %s, be disciplined and maintain focus.\n\n"
                        emacs-version (emacs-init-time "%.3fs") user-full-name)))

;; User preferences
(setq column-number-mode t
	  display-time-24hr-format t
	  display-time-mode t
	  ispell-dictionary nil
	  sentence-end-double-space t
	  sentence-end-without-period nil
	  colon-double-space nil
	  use-hard-newlines nil
	  adaptive-fill-mode t
	  bidi-inhibit-bpa t
	  scroll-conservatively 101
	  x-stretch-cursor t
	  ring-bell-function 'ignore
	  use-short-answers t
	  confirm-kill-emacs nil
      confirm-kill-processes nil
	  save-interprogram-paste-before-kill t
	  kill-read-only-ok t
	  mode-line-defining-kbd-macro
			(propertize " Macro" 'face 'mode-line-emphasis)
	  revert-without-query '(".*")
	  help-window-select t
	  kill-whole-line t
	  mouse-yank-at-point t
	  calendar-week-start-day 1
	  custom-safe-themes t
	  frame-title-format '(multiple-frames "%b"
										   ("" "Emacs - %b ")))

(setq-default fill-column 100
			  tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct
              tab-width 4
              scroll-margin 4
              indent-tabs-mode t
              kill-do-not-save-duplicates t
			  bidi-paragraph-direction 'left-to-right
			  large-file-warning-threshold (* 30 1048 1048))

;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'org-babel-post-tangle-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; No warnings and restrictions
(dolist (unrestricted '(erase-buffer
						narrow-to-region
						narrow-to-page
						dired-find-alternate-file
						upcase-region
						downcase-region))
(put unrestricted 'disabled nil))

(setq safe-local-variable-values
	  '((eval add-to-list 'whitespace-style 'indentation::tab)
		(eval delete 'indentation whitespace-style)
		(display-line-numbers . visual)
		(eval indent-tabs-mode t)))

;; Select text is replaced with input
(delete-selection-mode t)

;; Automatically loads the files if they change in another process
(global-auto-revert-mode t)

;; File name shadow mode
(file-name-shadow-mode t)


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;;;; Initializing
(require 'package)
(setq package-archives
	  '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
		("elpa-devel" . "https://elpa.gnu.org/devel/")))

;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities
      '(("melpa" . 2)
		("elpa"  . 1)))

(package-initialize)


;;;; `use-package'
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t
  	  use-package-always-defer t)


;;;; `auto-package-update'
(use-package auto-package-update
  :hook ((auto-package-update-after . package-autoremove)
		 (auto-package-update-after . package-quickstart-refresh))
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 2)
  :config
  (auto-package-update-maybe))


;;;; `repeat'
(use-package repeat
  :init
  (repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))


;;;; `vundo'
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;;; `hl-todo'
(use-package hl-todo
  :config
  (setq hl-todo-color-background nil
		hl-todo-keyword-faces
		'(("TODO"     . "#FFA623")
          ("FIXME"    . "#FF4000")
          ("BUG"    . "#A020F0")
		  ("REMINDER" . "#1111EE")
          ("TRY"   . "#1CFF0F")
          ("HACK"     . "#1E90FF")))
  :bind (:map hl-todo-mode-map
  			  ("C-c t p" . hl-todo-previous)
			  ("C-c t n" . hl-todo-next)
			  ("C-c t o" . hl-todo-occur)
			  ("C-c t i" . hl-todo-insert))
  :init
  (global-hl-todo-mode))


;;;; `affe'
(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))


;;;; `goggles'
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing


;;;; `smart-hungry-delete'
(use-package smart-hungry-delete
  :no-require t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char)))


;;;; `jinx'
(use-package jinx
  :after vertico
  :defines vertico-multiform-categories
  :hook ( emacs-startup . global-jinx-mode )
  :bind (( "M-$"  . jinx-correct )
		 ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_GB pt_PT-preao"
		jinx-include-faces '((prog-mode font-lock-doc-face)
							 (conf-mode font-lock-comment-face))
		jinx-exclude-regexps
        '((t "[A-Z]+\\>"
             "\\<[[:upper:]][[:lower:]]+\\>"
             "\\w*?[0-9\.'\"-]\\w*"
             "[a-z]+://\\S-+"
             "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))

  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 25))))


;;;; `osm'
(use-package osm
  :bind (("C-c o h" . osm-home)
         ("C-c o s" . osm-search)
         ("C-c o v" . osm-server)
         ("C-c o t" . osm-goto)
         ("C-c o x" . osm-gpx-show)
         ("C-c o j" . osm-bookmark-jump))
  :config
  ;; Take a look at the customization group `osm' for more options.
  (setq osm-server 'default  ;; Configure the tile server
		osm-copyright t)     ;; Display the copyright information
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))


;;;; `smtpmail'
(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-queue-mail nil))


;;;; `sendmail'
(use-package sendmail
  :config
  (setq send-mail-function 'smtpmail-send-it))


;;;; `notmuch'
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp/"
  :config
  (setq notmuch-identities '("Bruno Boal <bruno.boal@mailbox.org>")
		notmuch-fcc-dirs   '(("bruno.boal@mailbox.org" . "mailbox/Sent"))
		notmuch-show-logo nil))


;;;; `pass'
(use-package pass)


;;;; `vterm'
(use-package vterm)


;;;; `lin'
(use-package lin
  :config
  (setq lin-face 'lin-red)
  (lin-global-mode))


;;;; `ediff'
(use-package ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))


;;;; `magit'
(use-package magit
  :bind ("C-c s" . magit-status))


;;;; `magit-annex'
(use-package magit-annex)


;;;; `nov'
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Iosevka Zenodotus Fixed"
                           :height 130))
(add-hook 'nov-mode-hook 'my-nov-font-setup))


;;;; `project'
(use-package project)


;;;; `substitute'
(use-package substitute)


;;;; `pdf-tools'
(use-package pdf-tools)


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
  :demand t
  :config
  (load-theme hour-sets-modus)
  ;; Clock in the modeline
  (setq display-time-string-forms
        '((propertize (concat " " 24-hours ":" minutes " ")
                      'face #'keycast-key)))
  (display-time-mode 1))

;; tab-bar tweaks
(setq tab-bar-format                    ; Emacs 28
      '(tab-bar-format-tabs-groups
        tab-bar-format-align-right
        tab-bar-format-global))
;;global-mode-string (TODO)


;;;; `ef-themes'
(use-package ef-themes)


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


;;;; `diredfl'
(use-package diredfl
  :after dired
  :hook (dired-mode . (lambda()
						 (diredfl-mode)
						 (gnus-dired-mode)))
  :config
  (require 'gnus-dired))


;;;; `which-key'
(use-package which-key
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
  (setq savehist-file (locate-user-emacs-file "history")
		history-delete-duplicates t
		savehist-save-minibuffer-history t
        history-length 1000)
  :init
  (savehist-mode))


;;;; `consult'
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c M-x" . consult-mode-command)
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
         ("C-h a" . apropos-command)               ;; orig. apropos-command
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
  :config
  (setq completion-styles '(orderless basic)
		completion-category-overrides '((file (styles basic partial-completion orderless)))))


;;;; `marginalia'
(use-package marginalia
  :init
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
  :hook (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer)
  :config
  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (setq corfu-min-width 40
		corfu-max-width 80
		corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
		corfu-auto nil                 ;; Enable auto completion
		corfu-auto-delay 1
		corfu-auto-prefix 3
		corfu-separator ?\s          ;; Orderless field separator
		corfu-quit-at-boundary 'separator     ;; Never quit at completion boundary
		corfu-quit-no-match 'separator        ;;  quit, even if there is no match
		corfu-preview-current nil    ;; Disable current candidate preview
		corfu-preselect 'prompt      ;; Disable candidate preselection
		corfu-on-exact-match nil     ;; Configure handling of exact matches
		corfu-scroll-margin 2)        ;; Use scroll margin
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
  :config
  (setq dabbrev-ignored-buffer-regexps
		'("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;;;; `abbrev'
(require 'abbrev)

(setq abbrev-file-name (locate-user-emacs-file "bb-personal-abbrevs")
	  only-global-abbrevs nil)

(dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
  (add-hook hook #'abbrev-mode))
  ;; no confirmation on using ’abbrev-file-name’ to save abbreviations.
(remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

(define-key abbrev-map (kbd "C-x a u")  #'unexpand-abbrev)

(define-abbrev-table
  'global-abbrev-table '(("bmail" "egomet@bboal.com")
						 ("bberg" "https://codeberg.org/BBoal")
						 ("bhut"  "https://git.sr.ht/~bboal")
						 ("blab"  "https://gitlab.com/bboal")
						 ("bhub"  "https://github.com/BBoal")))
(with-eval-after-load 'message
  (define-abbrev-table
	'message-mode-abbrev-table '(("gm" "Good morning, ")
								 ("ga" "Good afternoon, ")
								 ("ge" "Good evening, ")
								 ("fyw" "I hope this email finds you well.\n")
								 ("br" "Best regards,\nBruno Boal")
								 ("ct" "Caros todos,\nEspero-vos bem.\n")
								 ("atent" "Atentamente,\nBruno Boal")
								 ("abest" "All the best,\nBruno Boal")
								 ("unt" "Cheers, until next time,\nBruno Boal "))))


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
  :demand t
  :bind (:map yas-minor-mode-map
			  ("ç" . yas-maybe-expand))
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))

  :config
  (setq yas-also-auto-indent-first-line t
        yas-also-indent-empty-lines t)
  (yas-reload-all))


;;;; `consult-yasnippet'
(use-package consult-yasnippet
  :after (consult yasnippet))


;;;; `eglot'
(use-package eglot
  :after envrc
  :bind (:map eglot-mode-map
			  ("C-c r" . eglot-rename)
			  ("C-c o" . eglot-code-action-organize-imports)
			  ("C-c h" . eldoc)
			  ("C-c f" . eglot-code-action-quickfix)
			  ("C-c i" . eglot-code-action-inline))
  :hook ((go-mode
		  c++-mode
		  haskell-mode
		  lua-mode
		  python-mode ) . eglot-ensure)
  :config
  (setq corfu-popupinfo-mode t
		corfu-popupinfo-delay 1.0
		eglot-sync-connect nil
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


;;;; `pulsar'
(use-package pulsar
  :demand t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 20
        pulsar-face 'pulsar-green
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))


;;;; `kind-icon'
(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-use-icons t
		kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
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
  :init
  (minions-mode t))


;;;; `envrc'
(use-package envrc
  :config
  (envrc-global-mode))


;;;; `editorconfig'
(use-package editorconfig
  :config
  (editorconfig-mode))


;;;; `org'
(use-package org
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
  (require 'bb-org-capture))


;;;; `expand-region'
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; TODO try mc/insert-numbers
;;;; `multiple-cursors'
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ( "C->" . mc/mark-next-like-this)
		 ( "C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)))


;;;; `gnuplot'
(use-package gnuplot
  :mode ("\\.gp$\\'" . gnuplot-mode))


;;;; `prism'
(use-package prism)


;;;; `highlight-indent-guides'
(use-package highlight-indent-guides
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

  (setq highlight-indent-guides-method #'character
		highlight-indent-guides-auto-enabled nil
		highlight-indent-guides-responsive #'top)
  (set-face-background
   'highlight-indent-guides-top-character-face (bb-get-color 'cursor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAMMING LANGUAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; personal-programming-hooks function
(defun personal-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (electric-pair-local-mode t)
  (subword-mode t)
  (highlight-indent-guides-mode t)
  (hs-minor-mode t))


;;;;;; `dhall-mode'
(use-package dhall-mode
  :mode "\\.dhall\\'")


;;;;;; `cc-mode'
(use-package cc-mode
  :hook ((c++-mode . personal-programming-hooks)
         (before-save . (lambda() #'(cpp-auto-include) #'(eglot-format))))
  :bind (:map c++-mode-map
              ("C-c C-c" . compile))
  :config
  (defun project-find-root (dir)
    (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'CMakeLists root)))

  (cl-defmethod project-root ((project (head CMakeLists)))
    (cdr project))

  (add-hook 'project-find-functions 'project-find-root)
  (setq-local compile-command "g++ -std=c++20 -Wall"))


;;;;;; `cpp-auto-include'
(use-package cpp-auto-include)

;;;;;; `clang-capf'
(use-package clang-capf
  :after cape
  :hook (c-mode c++-mode objc-mode)
  :config
  (add-to-list 'completion-at-point-functions #'clang-capf))


;;;;;; `go-mode'
(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :hook
  ((go-mode . personal-programming-hooks)
   (before-save . (lambda() #'(eglot-code-action-organize-imports 0) #'(eglot-format))))
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  (setq-local compile-command "go build -v && go test -v && go vet"
              tab-width 4))



;;;;;; `haskell-mode'
(use-package haskell-mode
  :hook
  (haskell-mode . personal-programming-hooks)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  (haskell-indent-mode t))

;;;;;; `ormolu'
(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))


;;;;;; `python-mode'
(use-package python-mode
  :hook ((python-mode . personal-programming-hooks)
		 (python-mode . flymake-ruff-load))
  :config
  (python-black-on-save-mode t))

;;;; `python-black'
(use-package python-black)

;;;;;; `flymake-ruff'
(use-package flymake-ruff)

;;;;; `anaconda-mode'
(use-package anaconda-mode)

;;;; `live-py-mode'
(use-package live-py-mode)

;;;; `poetry'
(use-package poetry)

;;;; `lua-mode'
(use-package lua-mode)

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

;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun bb/kill-beg-line()
  "Kills until the beginning of the text in current line.
If no text exists between point and the start of the line,
kills the text before point."
	(interactive)
	(let ((end (point)))
	  (beginning-of-line-text)
	  (if (= end (point))
		  (kill-line 0)
		(kill-region (point) end))))

(defun bb/insert-newline-below(&optional arg)
  "Inserts a new and indented line after the current one or, with prefix,
after ARG number of lines."
  (interactive "P")
  (when arg
	(forward-line arg))
  (move-end-of-line nil)
  (newline-and-indent))

(defun bb/insert-newline-above(&optional arg)
  "Inserts a new and indented line before the current one or, with prefix,
before ARG number of lines."
  (interactive "P")
  (if arg
	  (forward-line (- (1+ arg)))
	(forward-line -1))
  (if (/= (line-number-at-pos) 1)
	  (bb/newline-below)
	(newline-and-indent)
	(forward-line -1)))

(defun bb/kill-ring-save-line()
  "TODO save region if selected as argument"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun bb/duplicate-line()
  "TODO duplicate selected region if present"
  (interactive)
  (let ((dif-end-point
		 (- (line-end-position) (point))))
	(bb/kill-ring-save-line)
	(move-end-of-line nil)
	(newline)
	(yank-in-context)
	(cdr kill-ring)
	(goto-char (- (point) dif-end-point))))


;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

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

(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-l") #'windmove-right)

(global-set-key (kbd "C-M-=") #'count-words)
(global-set-key (kbd "M-DEL") #'backward-delete-word)
(global-set-key (kbd "M-k") #'bb/kill-beg-line)

(global-set-key (kbd "C-o") #'bb/insert-newline-below)
(global-set-key (kbd "M-o") #'bb/insert-newline-above)

(global-set-key (kbd "s-y") #'bb/kill-ring-save-line)
(global-set-key (kbd "s-d") #'bb/duplicate-line)

(global-set-key (kbd "C-`") #'push-mark-no-activate)
(global-set-key (kbd "M-`") #'jump-to-mark)
(define-key global-map [remap exchange-point-and-mark] #'exchange-point-and-mark-no-activate)

;; (define-key map (kbd "<tab>")    #'log-view-toggle-entry-display)
;; (define-key map (kbd "<return>") #'log-view-find-revision)
;; (define-key map (kbd "s")        #'vc-log-search)


(provide 'init)
;;; init.el ends here
