;;; bb-dired.el --- Dired settings -*- lexical-binding: t -*-

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
;; Tweaked Dired configs

;;; Code:


(require 'gnus-dired)


;;;; `wdired'
(use-package wdired
  :demand t
  :functions bb-set-track-eol
  :config
  (defun bb-set-track-eol ()
    "Bind variables, for easy multi-file operation."
    (setq-local line-move-visual nil
                track-eol t))

  (add-hook 'wdired-mode-hook #'bb-set-track-eol :depth t)
  (setq wdired-allow-to-change-permissions 'advanced
        wdired-use-dired-vertical-movement 'sometimes))




(use-package emacs
  :demand t
  ;; :bind ((:map image-mode-map
  ;;              ("q" . image-kill-buffer)
  ;;              ("n" . image-next-file)
  ;;              ("p" . image-previous-file))
  ;;        (:map dired-mode-map
  ;;              ("o" . image-dired-dired-display-external)))
  :config
  ;; When there are two Dired buffers side-by-side make Emacs automatically
  ;; suggest the other one as the target of copy or rename operations.  Remember
  ;; that you can always use M-p and M-n in the minibuffer to cycle through the
  ;; history, regardless of what this does.  (The "dwim" stands for "Do What I
  ;; Mean".)
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
  ;; files with names with embedded newline characters, adding ‘b’ to the switches
  ;; will allow Dired to handle those files better.
  (setq dired-listing-switches "-AbFhlt --time-style=long-iso")

  ;; Avoid opening a new dired buffer for each new visited dir
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Ask for creation of destination dir and treat '/' specially
  (setopt dired-create-destination-dirs 'ask
          dired-create-destination-dirs-on-trailing-dirsep t)

  ;; Automatically hide the detailed listing when visiting a Dired buffer.  This
  ;; can always be toggled on/off by calling the `dired-hide-details-mode'
  ;; interactively with M-x or its keybindings (the left parenthesis by
  ;; default).
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))



;; 2024-02-11  TODO => Configure image-dired and emacs above


;;;; `image-dired'
;; (use-package image-dired
;;   :bind ((:map image-dired-thumbnail-mode-map
;;                ("j"  . image-dired-display-next)
;;                ("k"  . image-dired-display-previous)))
;;   :config
;;   (setopt image-dired-external-viewer (if (display-graphic-p) "nsxiv" "imgcat")
;;           image-dired-thumb-margin 4))



;;;; `diredfl'
(use-package diredfl
  :functions diredfl-mode
  :defer 1
  :config
  (add-hook 'dired-mode-hook #'diredfl-mode)
  (add-hook 'dired-mode-hook #'gnus-dired-mode))




;;;; `dired-preview'
(use-package dired-preview
  :functions dired-preview-mode
  :defer 1
  :config
  (add-hook 'dired-mode-hook #'dired-preview-mode))




;;;; `dired-duplicates'
(use-package dired-duplicates
  :defer 1)



(provide 'bb-dired)
;;; bb-dired.el ends here
