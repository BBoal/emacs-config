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
  (defun not-current-or-previous-dir-p (str)
    "Return nil if passed argument end in either \"/.\" or \"/..\""
    (let ((final-dir (car (last (file-name-split str)))))
      (not (or (string-equal "." final-dir)
               (string-equal ".." final-dir)))))


  (defcustom backup-sort-function #'vertico-sort-history-length-alpha
    "Default backup sorting function, similarly to vertico-sort-function."
    :type `(choice
            (const :tag "No sorting" nil)
            (const :tag "By history, length and alpha" ,#'vertico-sort-history-length-alpha)
            (const :tag "By history and alpha" ,#'vertico-sort-history-alpha)
            (const :tag "By length and alpha" ,#'vertico-sort-length-alpha)
            (const :tag "Alphabetically" ,#'vertico-sort-alpha)
            (function :tag "Custom function")))


  (defun vertico-latest-files (candidates)
    "CANDIDATES are sorted in last-modified order, `backup-sort-function' is used as
a fallback."
    (if-let ((input (file-name-directory (minibuffer-contents-no-properties)))
             ((file-directory-p input)))
        (let (list-files list-attr ordered-files)
          (mapc (lambda (file)
                  (let* ((cfile (concat input file))
                         (time (nth 5 (file-attributes cfile))))
                    (setq list-files (cons (cons file time)
                                           list-files)
                          list-attr (cons time
                                          list-attr))))
                candidates)
          (sort list-attr #'time-less-p)
          (while list-attr
            (setq ordered-files (cons (car (rassq (car list-attr) list-files))
                                      ordered-files)
                  list-attr (cdr list-attr)))
          ordered-files)
      (funcall backup-sort-function candidates)))

  (setq vertico-preselect 'prompt
        vertico-scroll-margin 1
        vertico-count 5
        vertico-resize nil
        vertico-multiform-categories '((file (vertico-sort-function . vertico-latest-files)))
        vertico-cycle t)
  (vertico-multiform-mode 1)

  :init
  (vertico-mode))




;;;; `jinx'
(use-package jinx
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

  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 25))))




;;;; `consult'
(use-package consult
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c s" . consult-history)
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
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

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

  :config
  ;; Setting the correct path to the config variable in order to get rid
  ;; of ripgrep errors during execution
  (setenv "RIPGREP_CONFIG_PATH" "/home/bb/.config/ripgrep/ripgreprc")

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
  :demand t
  :config
  (setq completion-styles '(basic initials orderless)
        orderless-matching-styles '(orderless-prefixes orderless-literal orderless-regexp)
        completion-category-overrides '((file (styles basic partial-completion orderless)))))




;;;; `marginalia'
(use-package marginalia
  :init
  (marginalia-mode))




;;;; `corfu'
(use-package corfu
  :bind (:map corfu-map
              ("s-SPC" . corfu-insert-separator)
              ("J" . corfu-next)
              ("K" . corfu-previous))
  :hook (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer)
  :config
  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil     ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (setq corfu-min-width 40
        corfu-max-width 80
        corfu-cycle t                      ;; Enable cycling for `corfu-next/previous'
        corfu-auto nil                     ;; Enable auto completion
        corfu-auto-delay 1
        corfu-auto-prefix 3
        corfu-popupinfo-mode t
        corfu-popupinfo-delay nil
        corfu-separator ?\s                ;; Orderless field separator
        corfu-quit-at-boundary 'separator  ;; Never quit at completion boundary
        corfu-quit-no-match 'separator     ;;  quit, even if there is no match
        corfu-preview-current t            ;; Disable current candidate preview
        corfu-preselect 'prompt            ;; Disable candidate preselection
        corfu-on-exact-match nil           ;; Configure handling of exact matches
        corfu-scroll-margin 2)             ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode))




;;;; `dabbrev'
(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps
        '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))




;;;; `abbrev'
(require 'abbrev)

(setq abbrev-file-name (locate-user-emacs-file "bb-personal-abbrevs")
      only-global-abbrevs nil)

(mapc (lambda (hook)
        (add-hook hook #'abbrev-mode))
      '(text-mode-hook prog-mode-hook git-commit-mode-hook))
;; no confirmation on using ’abbrev-file-name’ to save abbreviations.
(remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

;; undo abbrev
(keymap-set abbrev-map "u"  #'unexpand-abbrev) ;; C-x a u

(define-abbrev-table
  'global-abbrev-table '(("BB" "Bruno Boal")
                         ("bmail" "<egomet@bboal.com>")
                         ("bberg" "<https://codeberg.org/BBoal>")
                         ("bhut"  "<https://git.sr.ht/~bboal>")
                         ("blab"  "<https://gitlab.com/bboal>")
                         ("bhub"  "<https://github.com/BBoal>")))
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
  (mapc (lambda (functions)
          (add-to-list 'completion-at-point-functions functions))
        '(cape-dabbrev
          cape-file
          cape-history
          cape-keyword
          cape-elisp-block
          cape-sgml
          cape-abbrev
          cape-symbol)))




;;;; `yasnippet'
(use-package yasnippet
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs
        `(,(concat (locate-user-emacs-file "snippets"))))

  :config
  (setq yas-also-auto-indent-first-line t
        yas-also-indent-empty-lines t)
  (yas-reload-all))




;;;; `consult-yasnippet'
(use-package consult-yasnippet
  :defer 1
  :after (consult yasnippet))




;;;; `eglot'
(use-package eglot
  :defer 1
  :after envrc
  :config
  (setq corfu-popupinfo-mode t
        corfu-popupinfo-delay 1.0
        eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))


;;;; `consult-eglot'
(use-package consult-eglot
  :defer 3
  :after (consult eglot))



(provide 'bb-smart-completion)
;;; bb-smart-completion.el ends here
