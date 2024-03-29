;;; bb-enhanced-edit.el --- Add-on functions to supercharge Emacs -*- lexical-binding: t -*-

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


;;;; `bookmark'
(with-eval-after-load 'bookmark
  (setopt bookmark-default-file (concat user-emacs-directory "var/bookmarks")))



;;;; `dictionary'
(with-eval-after-load 'dictionary
  (setopt dictionary-server "dict.org"))



;;;; `imenu'
(with-eval-after-load 'imenu
  (setopt imenu-auto-rescan t))



;;;; `ibuffer'
(with-eval-after-load 'ibuffer
  (setopt ibuffer-default-shrink-to-minimum-size t
          ibuffer-expert t
          ibuffer-use-other-window t))




;;;; `repeat'
(use-package repeat
  :demand t
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeat.el, though it is the same idea.
        set-mark-command-repeat-pop t) ;; C-u C-SPC once, then C-SPC, C-SPC, ...
  (repeat-mode))




;;;; `smart-hungry-delete'
(use-package smart-hungry-delete
  :bind (("C-<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char)))




;;;; `ediff'
(use-package ediff
  :defer 1
  :functions ediff-setup-windows-plain
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))




;;;; `wgrep'
;; Make grep buffers editable
(use-package wgrep
  :defer 1
  :defines grep-mode-map
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config
  (setopt wgrep-auto-save-buffer t
          wgrep-change-readonly-file t))




;; ;;;; `beframe'
;; (use-package beframe
;;   :demand t
;;   :bind ("C-x f" . other-frame-prefix)    ; override `set-fill-column'
;;   ;; Replace the generic `buffer-menu'.  With a prefix argument, this
;;   ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
;;   ;; you absolutely need the global list of buffers.
;;   ;;("C-x C-b" . beframe-buffer-menu)
;;   :config
;;   (setopt beframe-functions-in-frames '(project-prompt-project-dir))
;;   ;;(defvar consult-buffer-sources)
;;   (declare-function consult--buffer-state "consult")
;;
;;   (with-eval-after-load 'consult
;;     (defface beframe-buffer
;;       '((t :inherit font-lock-string-face))
;;       "Face for `consult' framed buffers.")
;;
;;     (defvar beframe--consult-source
;;       `( :name     "Frame-specific buffers (current frame)"
;;          :narrow   ?F
;;          :category buffer
;;          :face     beframe-buffer
;;          :history  beframe-history
;;          :items    ,#'beframe--buffer-names
;;          :action   ,#'switch-to-buffer
;;          :state    ,#'consult--buffer-state))
;;
;;     (add-to-list 'consult-buffer-sources 'beframe--consult-source))
;;   (beframe-mode))




;;;; `savehist'
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :demand t
  :config
  (setq savehist-file (concat user-emacs-directory "var/history")
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(register-alist kill-ring)
        savehist-ignored-variables '(org-tags-history read-char-history)
        history-length 1000)
  (savehist-mode t))




;;;; `expreg'
(use-package expreg
  :demand t
  :commands expreg-expand
  :functions expreg--sentence
  :defines expreg-functions
  :hook (text-mode . add-expreg-sentence)
  :bind (("C-«" . bb-expreg-try-expand-symbol)
         ("C-»" . expreg-contract))
  :config
  (defun bb-expreg-expand (n)
    "Expand to N syntactic units, defaulting to 1 in interactive use."
    (interactive "p")
    (while (> n 0)
      (expreg-expand)
      (setq n (1- n))))

  (defun bb-expreg-try-expand-symbol ()
    "If point is over a symbol, mark it, otherwise fall-back to regular
`expreg-expand'."
    (interactive)
    (if (and (bounds-of-thing-at-point 'symbol)
             (not (use-region-p)))
        (expreg-expand))
    (expreg-expand))

  (defun add-expreg-sentence()
    (cl-pushnew #'expreg--sentence expreg-functions)))




;; TODO try mc/insert-numbers
;;;; `multiple-cursors'
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-c s-<" . mc/mark-more-like-this-extended))
  :config
  (setopt mc/list-file (concat user-emacs-directory "var/mc-lists.el")))




;;;; `newcomment'
(with-eval-after-load 'newcomment
  (setq comment-empty-lines t        ;; default nil
        comment-inline-offset 2      ;; default 1
        comment-multi-line t         ;; default nil
        comment-style 'extra-line))  ;; default 'indent



(provide 'bb-enhanced-edit)
;;; bb-enhanced-edit.el ends here
