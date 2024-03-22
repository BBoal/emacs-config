;;; bb-keybindings.el --- Personal preference keybinds -*- lexical-binding: t -*-

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


;;;; Prefix map Template
;; (defvar-keymap testing-prefix-motion-map
;;   "e" #'move-end-of-line)
;; (define-key testing-prefix-map "s-m" testing-prefix-motion-map)


(with-eval-after-load 'consult
;;;; "M-g" g-goto-prefix-map
  (defvar-keymap g-goto-prefix-map
    "e"   #'consult-compile-error
    "M-g" #'consult-goto-line     ;; orig. goto-line
    "o"   #'consult-outline       ;; Alternative: consult-org-heading
    "m"   #'consult-mark
    "M-m" #'consult-global-mark
    "i"   #'consult-imenu
    "I"   #'consult-imenu-multi)
  (keymap-global-set "M-g" g-goto-prefix-map)



;;;; "M-s" s-search-prefix-map
  (defvar-keymap s-search-prefix-map
    "G" #'consult-git-grep
    "L" #'consult-line-multi
    "f" #'consult-find
    "g" #'consult-grep
    "k" #'consult-keep-lines
    "l" #'consult-line
    "o" #'occur
    "r" #'consult-ripgrep
    "s" #'consult-isearch-history
    "u" #'consult-focus-lines)
  (keymap-global-set "M-s" s-search-prefix-map))



;;;; "s-c" s-change-prefix-map
(defvar-keymap s-change-prefix-map
  "a" #'bb-change-around-char-pairs
  "i" #'bb-change-inside-char-pairs
  "m" #'bb-cut-marks
  "w" #'bb-wrap-region
  "u" #'bb-simple-escape-url-dwim)
(keymap-global-set "s-c" s-change-prefix-map)



;;;; "s-e" s-eglot-prefix-map
(with-eval-after-load 'eglot
  (defvar-keymap s-eglot-prefix-map
    "r" #'eglot-rename
    "o" #'eglot-code-action-organize-imports
    "h" #'eldoc
    "f" #'eglot-code-action-quickfix
    "i" #'eglot-code-action-inline)
  (keymap-set eglot-mode-map "s-e" s-eglot-prefix-map))



;;;; "s-h" s-help-prefix-map
(defvar-keymap s-help-prefix-map
  "a" #'apropos-command
  "m" #'consult-man
  "B" #'embark-bindings) ;; alternative for \`describe-bindings\'
(keymap-global-set "s-h" s-help-prefix-map)



;;;; "s-o" s-org-prefix-map
(defvar-keymap s-org-prefix-map
  "a" #'org-agenda
  "c" #'org-capture
  "l" #'org-store-link
  "m" #'org-cmenu)
(keymap-global-set "s-o" s-org-prefix-map)



;;;; "s-x" s-x-prefix-map
(defvar-keymap s-x-prefix-map
  "f" #'find-all-files
  "k" #'bb-assassinate-buffer
  "K" #'bb-assassinate-buffer-and-window
  "n" #'logos-narrow-dwim
  "m" #'bb-minimap-mode
  "o" #'bb-delete-blank-lines-dwim)
(keymap-global-set "s-x" s-x-prefix-map)



;;;; Function keys
(keymap-global-set "<f12>" #'save-buffer)
(keymap-global-set "<f10>" #'save-buffers-kill-emacs)
(keymap-global-set "<f9>" #'bb-simple-cycle-menu-bar-mode)
(keymap-global-set "<f8>" #'logos-focus-mode)
(keymap-global-set "<f7>" #'bb-simple-cycle-display-line-numbers)
(keymap-global-set "<f6>" #'whitespace-mode)
(keymap-global-set "<f5>" #'keycast-mode-line-mode)
(keymap-global-set "<f2>" #'bb-revert-buffer-no-confirm)
(keymap-global-set "<f1>" (lambda () (interactive)
                            (find-file "~/.emacs.d/init.el")))

(keymap-global-set "s-<f5>" #'bb-set-desktop-file-for-save)
(keymap-global-set "s-<f8>" #'bb-silent-desktop-read)



;;;; Window management "s-..."
(keymap-global-set "s-k" #'windmove-up)
(keymap-global-set "s-j" #'windmove-down)
(keymap-global-set "s-h" #'windmove-left)
(keymap-global-set "s-l" #'windmove-right)
(keymap-global-set "s-r" #'window-swap-states)
(keymap-global-set "s-\\" #'bb-split-window-right-and-focus)
(keymap-global-set "s-\-" #'bb-split-window-below-and-focus)
(keymap-global-set "s-q" #'bb-kill-buffer-and-delete-window)
(keymap-global-set "s-!" #'delete-other-windows-vertically)



;;;; Text manipulation
(keymap-global-set "M-k" #'bb-kill-beg-line)
(keymap-global-set "M-t" #'bb-transpose-words)
(keymap-global-set "M-u" #'upcase-dwim)

(keymap-global-set "C-o" #'bb-insert-newline-below)
(keymap-global-set "M-o" #'bb-insert-newline-above)

(keymap-global-set "s-y" #'bb-kill-ring-save-line)
(keymap-global-set "s-D" #'bb-duplicate-line-above-dwim)
(keymap-global-set "s-d" #'bb-duplicate-line-below-dwim)
(keymap-global-set "s-," #'logos-backward-page-dwim)
(keymap-global-set "s-." #'logos-forward-page-dwim)

(keymap-global-set "C-+" #'bb-push-mark-no-activate)
(keymap-global-set "M-+" #'bb-jump-to-mark)

(keymap-global-set "s-z" #'bb-zap-from-char-to-end)
(keymap-global-set "M-z" #'zap-up-to-char)

(keymap-global-set "<home>" #'beginning-of-visual-line)
(keymap-global-set "<end>" #'end-of-visual-line)
(keymap-global-set "ç" #'bb-simple-ç-dwim)
(keymap-global-set "Ç" #'bb-simple-Ç-dwim)

(keymap-global-set "s-f" #'forward-to-word)
(keymap-global-set "s-b" #'backward-to-word)
(keymap-global-set "s-*" #'isearch-forward-thing-at-point)
(keymap-global-set "s-p" #'bb-move-line-above-dwim)
(keymap-global-set "s-n" #'bb-move-line-below-dwim)
(keymap-global-set "M-<up>" #'bb-scroll-other-window-down)
(keymap-global-set "M-<down>" #'bb-scroll-other-window)



;;;; notmuch
(with-eval-after-load 'notmuch
  (keymap-set notmuch-search-mode-map "/" #'notmuch-search-filter)           ; alias for l
  (keymap-set notmuch-search-mode-map "r" #'notmuch-search-reply-to-thread)  ; easier to reply to all by default
  (keymap-set notmuch-search-mode-map "R" #'notmuch-search-reply-to-thread-sender)
  (keymap-set notmuch-show-mode-map "r" #'notmuch-show-reply)
  (keymap-set notmuch-show-mode-map "R" #'notmuch-show-reply-sender))

;; (defvar-keymap s-notmuch-prefix-map
;; "m" #'notmuch-hello)
;; "u" #'notmuch-sort-saved-searches)

(defun bb-notmuch-new-mail ()
  "Open unread email."
  (interactive)
  (notmuch-search "tag:new or tag:unread"))
(keymap-global-set "s-m" #'bb-notmuch-new-mail)



;;;; dired
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "º" #'dired-up-directory)
  (keymap-unset dired-jump-map "j" :remove))


;;;; flymake
(with-eval-after-load 'flymake
  (keymap-global-set "s-?" #'flymake-mode)
  (keymap-global-set "M-s-n" #'flymake-goto-next-error)
  (keymap-global-set "M-s-p" #'flymake-goto-prev-error))


;;;; other bindings
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "s-t" #'vterm-other-window)
(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-M-=" #'count-words)
(keymap-global-set "C-c 0" #'kill-emacs)
(keymap-global-set "C-c <delete>" #'delete-frame)
(keymap-global-set "C-c l" #'dictionary-lookup-definition)
(keymap-set lisp-interaction-mode-map "C-j" #'bb-eval-print-current-sexp-lisp)
;; (keymap-set minibuffer-local-map "C-s" #'consult-history)

;; smart <Esc> key in context
;; Thanks to Prot for showing me how to do this.
(keymap-global-set "<escape>" #'bb-simple-keyboard-quit-dwim)


(provide 'bb-keybindings)
;;; bb-keybindings.el ends here
