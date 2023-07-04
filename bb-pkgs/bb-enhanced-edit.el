;;; bb-enhanced-edit.el --- Add-on functions to supercharge Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `repeat'
(use-package repeat
  :defer 3
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeat.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t)
  (repeat-mode))


;;;; `smart-hungry-delete'
(use-package smart-hungry-delete
  :bind (("C-<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char)))


;;;; `ediff'
(use-package ediff
  :defer 3
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))


;;;; `wgrep'
;; Make grep buffers editable
(use-package wgrep
  :defer 3
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))


;;;; `beframe'
(use-package beframe
  :demand t
  :bind (("C-x f" . other-frame-prefix)    ; override `set-fill-column'
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


;;;; `savehist'
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "history")
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(register-alist kill-ring)
        history-length 1000)
  :init
  (savehist-mode))


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



(provide 'bb-enhanced-edit)
;;; bb-enhanced-edit.el ends here
