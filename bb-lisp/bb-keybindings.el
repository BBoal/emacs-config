;;; bb-keybindings.el --- Personal preference keybinds -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

(keymap-set lisp-interaction-mode-map "C-j" #'bb-eval-print-current-sexp-lisp)

(keymap-global-set "<escape>" #'bb-simple-keyboard-quit-dwim)

(defvar-keymap s-help-prefix-map
  "a" #'apropos-command
  "m" #'consult-man
  "B" #'embark-bindings) ;; alternative for \`describe-bindings\'
(keymap-global-set "s-h" s-help-prefix-map)

(defvar-keymap s-org-prefix-map
  "a" #'org-agenda
  "c" #'org-capture
  "l" #'org-store-link)
(keymap-global-set "s-o" s-org-prefix-map)

(keymap-global-set "C-c 0" #'kill-emacs)
(keymap-global-set "C-c <delete>" #'delete-frame)
(keymap-global-set "C-c l" #'dictionary-lookup-definition)

(keymap-global-set "<f12>" #'save-buffer)
(keymap-global-set "<f10>" #'save-buffers-kill-emacs)
(keymap-global-set "<f9>" #'menu-bar-mode)
(keymap-global-set "<f8>" (lambda () (interactive)
                            (find-file "~/.emacs.d/init.el")))
(keymap-global-set "<f7>" #'bb-simple-cycle-display-line-numbers)
(keymap-global-set "<f6>" #'whitespace-mode)
(keymap-global-set "<f5>" #'keycast-mode-line-mode)
(keymap-global-set "<f2>" #'bb-revert-buffer-no-confirm)

(keymap-global-set "s-<f5>" #'desktop-save-in-desktop-dir)
(keymap-global-set "s-<f8>" #'desktop-read)


(keymap-global-set "s-k" #'windmove-up)
(keymap-global-set "s-j" #'windmove-down)
(keymap-global-set "s-h" #'windmove-left)
(keymap-global-set "s-l" #'windmove-right)
(keymap-global-set "s-\\" #'bb-split-window-right-and-focus)
(keymap-global-set "s-\-" #'bb-split-window-below-and-focus)
(keymap-global-set "s-q" #'bb-kill-buffer-and-delete-window)
(keymap-global-set "s-!" #'delete-other-windows-vertically)

(keymap-global-set "M-s o" #'occur)
(keymap-global-set "C-M-=" #'count-words)
(keymap-global-set "M-k" #'bb-kill-beg-line)
(keymap-global-set "M-t" #'bb-transpose-words)


(keymap-global-set "C-o" #'bb-insert-newline-below)
(keymap-global-set "M-o" #'bb-insert-newline-above)

(keymap-global-set "s-y" #'bb-kill-ring-save-line)
(keymap-global-set "s-D" #'bb-duplicate-line-above-dwim)
(keymap-global-set "s-d" #'bb-duplicate-line-below-dwim)
(keymap-global-set "s-t" #'vterm-other-window)

(keymap-global-set "C-+" #'bb-push-mark-no-activate)
(keymap-global-set "M-+" #'bb-jump-to-mark)
;; (define-key global-map [remap exchange-point-and-mark] #'exchange-point-and-mark-no-activate)

(keymap-global-set "C-c n" #'bb-find-occurrence-direction-kill-sexp)
(keymap-global-set "C-c a" #'bb-find-occurrence-direction-kill-around-sexp)


(defvar-keymap s-change-prefix-map
            "i" #'bb-change-inside-char-pairs
            "a" #'bb-change-around-char-pairs
            "u" #'bb-simple-escape-url-dwim)
(keymap-global-set "s-c" s-change-prefix-map)

;; (defvar-keymap testing-prefix-f-map
;;   "e" #'move-end-of-line)
;; (define-key testing-prefix-map "f" testing-prefix-f-map)

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
(keymap-global-set "s-o" #'bb-delete-blank-lines-dwim)


(defvar-keymap s-notmuch-prefix-map
            "m" #'notmuch-hello)
            ;; "u" #'notmuch-sort-saved-searches)
(keymap-global-set "s-m" s-notmuch-prefix-map)

(with-eval-after-load 'notmuch
  (keymap-set notmuch-search-mode-map "/" #'notmuch-search-filter) ; alias for l
  (keymap-set notmuch-search-mode-map "r" #'notmuch-search-reply-to-thread) ; easier to reply to all by default
  (keymap-set notmuch-search-mode-map "R" #'notmuch-search-reply-to-thread-sender)
  (keymap-set notmuch-show-mode-map "r" #'notmuch-show-reply)
  (keymap-set notmuch-show-mode-map "R" #'notmuch-show-reply-sender))


(provide 'bb-keybindings)
;;; bb-keybindings.el ends here
