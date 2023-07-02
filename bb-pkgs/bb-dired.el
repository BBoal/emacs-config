;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Tweaked dired configs

;;; Code:


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


;;;; `dired'
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t))



;;;; `diredfl'
(use-package diredfl
  :after dired
  :hook (dired-mode . (lambda()
                        (diredfl-mode)
                        (gnus-dired-mode)))
  :config
  (require 'gnus-dired))


(use-package dired-preview
  :after dired
  :vc ( :url "https://git.sr.ht/~protesilaos/dired-preview"
        :rev :newest)
  :config
  (add-hook 'dired-mode-hook #'dired-preview-mode))


(provide 'bb-dired)
;;; bb-dired.el ends here
