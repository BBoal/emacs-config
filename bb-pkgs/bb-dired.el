;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Tweaked dired configs

;;; Code:


;; When there are two Dired buffers side-by-side make Emacs
;; automatically suggest the other one as the target of copy or rename
;; operations.  Remember that you can always use M-p and M-n in the
;; minibuffer to cycle through the history, regardless of what this
;; does.
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

;;;; `diredfl'
(use-package diredfl
  :defer 2
  :after dired
  :hook (dired-mode . (lambda()
                        (diredfl-mode)
                        (gnus-dired-mode)))
  :config
  (require 'gnus-dired))




(provide 'bb-dired)
;;; bb-dired.el ends here
