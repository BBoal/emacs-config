;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Tweaked dired configs

;;; Code:


; When there are two Dired buffers side-by-side make Emacs
;; automatically suggest the other one as the target of copy or rename
;; operations.  Remember that you can always use M-p and M-n in the
;; minibuffer to cycle through the history, regardless of what this
;; does.  (The "dwim" stands for "Do What I Mean".)
(setq dired-dwim-target t)


;; Teach Dired to use a specific external program with either the
;; `dired-do-shell-command' or `dired-do-async-shell-command' command
;; (with the default keys, those are bound to `!' `&', respectively).
;; The first string is a pattern match against file names.  The
;; remaining strings are external programs that Dired will provide as
;; suggestions.  Of course, you can always type an arbitrary program
;; despite these defaults.
(setq dired-guess-shell-alist-user
      '(("\\.\\(png\\|jpe?g\\|webp\\|tiff\\)" "nsxiv" "xdg-open")
        ("\\.\\(xps\\|cbz\\|mobi\\|fb2\\|svg\\)" "mupdf" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		(".*" "xdg-open")))


;; Automatically hide the detailed listing when visiting a Dired
;; buffer.  This can always be toggled on/off by calling the
;; `dired-hide-details-mode' interactively with M-x or its keybindings
;; (the left parenthesis by default).
(add-hook 'dired-mode-hook #'dired-hide-details-mode)


;;;; `diredfl'
(use-package diredfl
  :hook (dired-mode . (lambda()
                        (diredfl-mode)
                        (gnus-dired-mode)))
  :config
  (require 'gnus-dired))


(use-package dired-preview
  :vc ( :url "https://git.sr.ht/~protesilaos/dired-preview"
        :rev :newest)
  :config
  (add-hook 'dired-mode-hook #'dired-preview-mode))


(provide 'bb-dired)
;;; bb-dired.el ends here
