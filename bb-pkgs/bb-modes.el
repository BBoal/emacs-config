;;; bb-modes.el --- Various modes -*- lexical-binding: t -*-

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

;;;; `xref'
(with-eval-after-load 'xref
  (setopt xref-search-program 'ripgrep))


;;;; `autorevert'
(with-eval-after-load 'autorevert
  (setopt auto-revert-avoid-polling t))


;;;; `tramp'
(with-eval-after-load 'tramp
  (setopt tramp-histfile-override t
          tramp-completion-use-cache nil
          tramp-persistency-file-name (concat user-emacs-directory
                                              "var/tramp")))

;;;; `nsm'
(with-eval-after-load 'nsm
  (setopt nsm-settings-file (concat user-emacs-directory
                                    "var/network-security.data")))


;;;; `url'
(with-eval-after-load 'url
  (setopt url-configuration-directory (concat user-emacs-directory
                                              "var/url/")))


;;;; `calendar'
(with-eval-after-load 'calendar
  (setopt calendar-date-style 'iso
          calendar-week-start-day 1))



;;;; `nov'
;; (use-package nov
;;   :defer 2
;;   :mode ("\\.epub\\'" . nov-mode)
;;   :config
;;   (defun my-nov-font-setup ()
;;     (face-remap-add-relative 'variable-pitch
;;                              :family "Iosevka Zenodotus Fixed"
;;                              :height 130))
;;   (add-hook 'nov-mode-hook 'my-nov-font-setup))




;;;; `doc-view'
(with-eval-after-load 'doc-view
  (setopt doc-view-continuous t
          doc-view-imenu-enabled t
          doc-view-resolution 300))




;;;; `csv-mode'
(use-package csv-mode
  :defer 1
  :mode	("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (setopt csv-separators '("," ";" "|" " ")))




;;;; `adoc-mode'
(use-package adoc-mode
  :defer 1
  :mode
  ("\\.adoc\\'" . adoc-mode))




;;;; `markdown-mode'
(use-package markdown-mode
  :defer 1
  :mode
  ("\\.md\\.html\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (setopt markdown-command
          (concat
           (executable-find "pandoc")
           " --from=markdown --to=html5"
           " --standalone --mathjax --highlight-style=pygments")
          markdown-fontify-code-blocks-natively t)
  (defun no-delete-whitespace ()
    (remove-hook 'before-save-hook #'delete-trailing-whitespace :local))
  (add-hook 'markdown-mode-hook 'no-delete-whitespace))



(provide 'bb-modes)
;;; bb-modes.el ends here
