;;; bb-smart-completion.el --- Gnomes and other helpers -*- lexical-binding: t -*-

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
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:


;;;; `vertico'
(use-package vertico
  ;; This works with `file-name-shadow-mode'. When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input. Works with pasting as well.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :defines
  bb-list-of-hidden-files +vertico-fallback-sort
  :commands vertico-multiform-mode vertico-mode
  :functions
  vertico-sort-history-length-alpha vertico-sort-history-alpha
  vertico-sort-length-alpha vertico-sort-alpha
  :config
  (defcustom +vertico-fallback-sort #'vertico-sort-history-length-alpha
    "Copy of original sorting variable for when `vertico-latest-files' fails.
We need a fallback since `vertico-sort-function' is bound to the
`+vertico-latest-files'. A loop would occur otherwise."
    :type `(choice
            (const :tag "No sorting" nil)
            (const :tag "By history, length and alpha" ,#'vertico-sort-history-length-alpha)
            (const :tag "By history and alpha" ,#'vertico-sort-history-alpha)
            (const :tag "By length and alpha" ,#'vertico-sort-length-alpha)
            (const :tag "Alphabetically" ,#'vertico-sort-alpha)
            (function :tag "Custom function"))
    :group 'vertico)

  (defcustom bb-list-of-hidden-files
    '(".zlua" ".gnupg/" ".Xauthority" "history" "places" "auto-save-list/")
    "List of files that should not appear when sorted with `vertico-latest-files'
function"
    :type '(list strings)
    :group 'vertico)

  (defun +vertico-latest-files (candidates)
    "CANDIDATES are sorted in last-modified order, `+vertico-fallback-sort' is used
as a fallback."
    (if-let ((input (file-name-directory (minibuffer-contents-no-properties)))
             ((file-directory-p input)))
        (let (list-files list-attr ordered-files)
          (mapc (lambda (file)
                  (let* ((cfile (concat input file))
                         (time (nth 5 (file-attributes cfile))))
                    (unless (member file bb-list-of-hidden-files)
                      (setq list-files (cons (cons file time)
                                             list-files)
                            list-attr (cons time
                                            list-attr)))))
                candidates)
          (setq list-attr (sort list-attr #'time-less-p))
          (while list-attr
            (setq ordered-files (cons (car (rassq (car list-attr) list-files))
                                      ordered-files)
                  list-attr (cdr list-attr)))
          ordered-files)
      (funcall +vertico-fallback-sort candidates)))

  (setopt vertico-preselect 'prompt
          vertico-scroll-margin 1
          vertico-count 5
          vertico-resize nil
          vertico-multiform-categories '((file (vertico-sort-function
                                                . +vertico-latest-files))
                                         (buffer flat (vertico-cycle . t)))
          vertico-cycle t)
  (vertico-multiform-mode 1)

  ;; Cleaning the UI of ffap-menu-ask
  (declare-function ffap-menu-ask "ffap")
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args))))

  :init
  (vertico-mode))



;;;; `text menu bar'
(declare-function tmm-add-prompt "tmm")
;; More UI cleanup
(with-eval-after-load 'tmm
  (setopt tmm-completion-prompt nil)
  ;; Taking *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))




;;;; `jinx'
(use-package jinx
  :after vertico
  :defines vertico-multiform-categories
  :hook ( emacs-startup . global-jinx-mode )
  :bind (( "M-$"  . jinx-correct )
         ("C-M-$" . jinx-languages))
  :config
  (setopt jinx-languages "en_US pt_PT-preao"
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
  :functions
  consult-register-format consult-register-window
  consult-xref consult-customize
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
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  :config
  ;; Setting the correct path to the config variable in order to get rid
  ;; of ripgrep errors during execution
  (setenv "RIPGREP_CONFIG_PATH"
          (expand-file-name "ripgrep/ripgreprc" (getenv "XDG_CONFIG_HOME")))


  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
      ;;; (consult-customize
      ;;;  consult-theme
      ;;;  :preview-key '(:debounce 0.2 any)
      ;;;  consult-ripgrep consult-git-grep consult-grep
      ;;;  consult-bookmark consult-recent-file consult-xref
      ;;;  consult--source-bookmark consult--source-recent-file
      ;;;  consult--source-project-recent-file
      ;;;  :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and or (kbd "C-+") work reasonably well.
  (setopt consult-narrow-key "<"))




;;;; `orderless'
(use-package orderless
  :demand t
  :functions +orderless--consult-suffix +orderless-consult-dispatch
  orderless-affix-dispatch
  :config
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  (setopt completion-styles '(orderless initials basic)
          completion-category-defaults nil
          orderless-matching-styles '(orderless-initialism
                                      orderless-literal
                                      orderless-regexp)
          completion-category-overrides '((file (styles partial-completion orderless))
                                          (eglot (styles orderless))
                                          (eglot-capf (styles orderless)))
          orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                            #'orderless-affix-dispatch)))




;;;; `marginalia'
(use-package marginalia
  :autoload marginalia-mode
  :init
  (marginalia-mode))




;;;; `corfu'
(use-package corfu
  :demand t
  :defines corfu-map corfu-separator
  :commands corfu-quit corfu-insert corfu-insert-separator
  :autoload corfu-mode global-corfu-mode
  :bind (:map corfu-map
              ("s-SPC" . double-space-separator)
              ("J" . corfu-next)
              ("K" . corfu-previous))
  :hook (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer)
  :config
  (defun double-space-separator ()
    "One press of SPC to self-insert, two for selecting the candidate."
    (interactive)
    (if current-prefix-arg              ; C-u prefixes SPC
        ;;we suppose that we want leave the word like that, so do a space
        (progn
          (corfu-quit)
          (insert " "))
      (if (and (= (char-before) corfu-separator)
               (or
                ;; check if space, return or nothing after
                (not (char-after))
                (= (char-after) ?\s)
                (= (char-after) ?\n)))
          (progn
            (corfu-insert)
            (insert " "))
        (corfu-insert-separator))))

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil     ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (setopt corfu-min-width 40
          corfu-max-width 80
          corfu-cycle t                      ;; Enable cycling for `corfu-next/previous'
          corfu-auto nil                     ;; Enable auto completion
          corfu-auto-delay 0.5
          corfu-auto-prefix 3
          corfu-popupinfo-mode t
          corfu-popupinfo-delay 0.1
          corfu-separator ?\s                ;; Orderless field separator "SPC"
          corfu-quit-at-boundary 'separator  ;; Never quit at completion boundary
          corfu-quit-no-match 'separator     ;; quit, even if there is no match
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
                         ("bboal" "<egomet@bboal.com>")
                         ("btut" "<bruno.boal@tutanota.com>")
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
  :bind (("M-ç"   . completion-at-point) ;; capf
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-symbol)
         ("M-p a" . cape-abbrev)
         ("M-p &" . cape-sgml))
  :init
  (mapc (lambda (function)
          (cl-pushnew function completion-at-point-functions))
        '(cape-symbol cape-dabbrev cape-sgml cape-elisp-block cape-keyword
                      cape-history cape-file cape-abbrev)))
        ;; '(cape-dabbrev
        ;;   cape-file
        ;;   cape-history
        ;;   cape-keyword
        ;;   cape-elisp-block
        ;;   cape-sgml
        ;;   cape-abbrev
        ;;   cape-symbol)))




;;;; `yasnippet'
(use-package yasnippet
  :defines yas-snippet-dirs
  :commands yas-reload-all
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")))

  :config
  (setopt yas-also-auto-indent-first-line t
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
  (setopt eglot-sync-connect nil
          eglot-autoshutdown t
          eglot-confirm-server-initiated-edits nil))




;;;; `consult-eglot'
(use-package consult-eglot
  :defer 1
  :after (consult eglot))




;;;; `immersive-translate'
(use-package immersive-translate
  :defer 2
  :config
  (setopt immersive-translate-backend 'trans
          immersive-translate-trans-source-language "de"
          immersive-translate-trans-target-language "en"))



(provide 'bb-smart-completion)
;;; bb-smart-completion.el ends here
