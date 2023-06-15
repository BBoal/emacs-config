;;; bb-smart-completion.el --- Gnomes and other helpers -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `vertico'
(use-package vertico
  ;; This works with `file-name-shadow-mode'. When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input. Works with pasting as well.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (setq vertico-preselect 'prompt
        vertico-cycle t)
  :init
  (vertico-mode))



;;;; `jinx'
(use-package jinx
  :defer 2
  :after vertico
  :hook ( emacs-startup . global-jinx-mode )
  :bind (( "M-$"  . jinx-correct )
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US pt_PT-preao"
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



;;;; `consult'
(use-package consult
  :defer 2
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c s" . consult-history)
         ("C-c M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)   ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)        ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)  ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)  ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("s-h a" . apropos-command)    ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)     ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)   ;; orig. goto-line
         ("M-g o" . consult-outline)       ;; Alternative: consult-org-heading
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
         ("M-s" . consult-isearch-history)    ;; orig. isearch-edit-string
         ("M-s s" . consult-isearch-history)  ;; orig. isearch-edit-string
         ("M-s l" . consult-line)        ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)  ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)       ;; orig. next-matching-history-element
         ("M-r" . consult-history))      ;; orig. previous-matching-history-element

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
  :defer 2
  :demand t
  :config
  (setq completion-styles '(basic initials orderless)
        orderless-matching-styles '(orderless-prefixes orderless-literal orderless-regexp)
        completion-category-overrides '((file (styles basic partial-completion orderless)))))


;;;; `marginalia'
(use-package marginalia
  :defer 2
  :init
  (marginalia-mode))


;;;; `embark'
(use-package embark
  :defer 2
  :bind
  (("C-." . embark-act)                 ;; pick some comfortable binding
   ("C-," . embark-dwim))                ;; good alternative: M-.
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
  :defer 2
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;;; `corfu'
(use-package corfu
  :hook (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer)
  :config
  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil   ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (setq corfu-min-width 40
        corfu-max-width 80
        corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
        corfu-auto nil               ;; Enable auto completion
        corfu-auto-delay 1
        corfu-auto-prefix 3
        corfu-popupinfo-mode t
        corfu-popupinfo-delay nil
        corfu-separator ?\s          ;; Orderless field separator
        corfu-quit-at-boundary 'separator  ;; Never quit at completion boundary
        corfu-quit-no-match 'separator     ;;  quit, even if there is no match
        corfu-preview-current t      ;; Disable current candidate preview
        corfu-preselect 'prompt      ;; Disable candidate preselection
        corfu-on-exact-match nil     ;; Configure handling of exact matches
        corfu-scroll-margin 2)       ;; Use scroll margin
  :bind
  (:map corfu-map
        ("s-SPC" . corfu-insert-separator)
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

(define-key abbrev-map (kbd "u")  #'unexpand-abbrev) ;; C-x a u

(define-abbrev-table
  'global-abbrev-table '(("BB" "Bruno Boal")
                         ("bmail" "<egomet@bboal.com>")
                         ("bberg" "https://codeberg.org/BBoal")
                         ("bhut"  "https://git.sr.ht/~bboal")
                         ("blab"  "https://gitlab.com/bboal")
                         ("bhub"  "https://github.com/BBoal")))
(with-eval-after-load 'message
  (define-abbrev-table
    'message-mode-abbrev-table '(("gm"  "Good morning,")
                                 ("ga"  "Good afternoon,")
                                 ("ge"  "Good evening,")
                                 ("fyw" "I hope this email finds you well.\n")
                                 ("br"  "Best regards,\nBruno Boal")
                                 ("ct"  "Caros todos,\nEspero-vos bem.\n")
                                 ("atent" "Atentamente,\nBruno Boal")
                                 ("abest" "All the best,\nBruno Boal")
                                 ("unt" "Cheers, until next time,\nBruno Boal"))))


;;;; `cape'
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-ç" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-symbol)
         ("M-p a" . cape-abbrev)
         ("M-p &" . cape-sgml))
  :init
  (dolist (functions '(cape-dabbrev cape-file cape-history cape-keyword
                       cape-elisp-block cape-sgml cape-abbrev cape-symbol))
    (add-to-list 'completion-at-point-functions functions)))


;;;; `yasnippet'
(use-package yasnippet
  :defer 2
  :demand t
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs
        `(,(concat (locate-user-emacs-file "snippets"))))

  :config
  (setq yas-also-auto-indent-first-line t
        yas-also-indent-empty-lines t)
  (keymap-set yas-minor-mode-map "ç" yas-maybe-expand)
  (yas-reload-all))


;;;; `consult-yasnippet'
(use-package consult-yasnippet
  :after (consult yasnippet))


;;;; `eglot'
(use-package eglot
  :after envrc
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e o" . eglot-code-action-organize-imports)
              ("C-c e h" . eldoc)
              ("C-c e f" . eglot-code-action-quickfix)
              ("C-c e i" . eglot-code-action-inline))
  :config
  (setq corfu-popupinfo-mode t
        corfu-popupinfo-delay 1.0
        eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))


;;;; `consult-eglot'
(use-package consult-eglot
  :after (consult eglot))



(provide 'bb-smart-completion)
;;; bb-smart-completion.el ends here