;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Tweaked dired configs

;;; Code:


(require 'gnus-dired)

; When there are two Dired buffers side-by-side make Emacs
;; automatically suggest the other one as the target of copy or rename
;; operations.  Remember that you can always use M-p and M-n in the minibuffer
;; to cycle through the history, regardless of what this does.  (The "dwim"
;; stands for "Do What I Mean".)
(setq dired-dwim-target t)


;; Teach Dired to use a specific external program with either the
;; `dired-do-shell-command' or `dired-do-async-shell-command' command (with the
;; default keys, those are bound to `!' `&', respectively).  The first string
;; is a pattern match against file names.  The remaining strings are external
;; programs that Dired will provide as suggestions.  Of course, you can always
;; type an arbitrary program despite these defaults.
(setq dired-guess-shell-alist-user
      '(("\\.\\(png\\|jpe?g\\|webp\\|tiff\\)" "nsxiv" "xdg-open")
        ("\\.\\(xps\\|cbz\\|mobi\\|fb2\\|svg\\)" "mupdf" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		(".*" "xdg-open")))

;; Copy recursively without asking
(setq dired-recursive-copies 'always)

;; Automatically revert dired buffers whenever ‘dired-directory-changed-p'
;; returns non-nil
(setq dired-auto-revert-buffer #'dired-directory-changed-p)

;; Switches passed to ‘ls’ for Dired.  MUST contain the ‘l’ option. If you have
;; files with names with embedded newline characters, adding ‘b’ to the
;; switches will allow Dired to handle those files better.
(setq dired-listing-switches "-AbFhlt --time-style=long-iso")

;; Avoid opening a new dired buffer for each new visited dir
(setq dired-kill-when-opening-new-dired-buffer t)

;; Ask for creation of destination dir and treat '/' specially
(setq dired-create-destination-dirs 'ask
      dired-create-destination-dirs-on-trailing-dirsep t)

;; Automatically hide the detailed listing when visiting a Dired buffer.  This
;; can always be toggled on/off by calling the `dired-hide-details-mode'
;; interactively with M-x or its keybindings (the left parenthesis by default).
(add-hook 'dired-mode-hook #'dired-hide-details-mode)




;;;; `diredfl'
(use-package diredfl
  :defer 1
  :config
  (add-hook 'dired-mode-hook #'diredfl-mode)
  (add-hook 'dired-mode-hook #'gnus-dired-mode))




;;;; `dired-preview'
(use-package dired-preview
  :defer 2
  :config
  (add-hook 'dired-mode-hook #'dired-preview-mode))


(provide 'bb-dired)
;;; bb-dired.el ends here
