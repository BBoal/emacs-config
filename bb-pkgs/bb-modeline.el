;;; bb-modeline.el --- Configuration of the Mode-Line -*- lexical-binding: t -*-

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


(require 'json)

;;;; `mlscroll'
(use-package mlscroll
  :after (modus-themes ef-themes)
  :demand t
  :commands mlscroll-mode
  :config
  (setopt mlscroll-right-align nil
          mlscroll-alter-percent-position nil
          mlscroll-border 5
          mlscroll-minimum-current-width 2
          mlscroll-width-chars 15
          modus-themes-common-palette-overrides
          '((border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified))
          ef-themes-common-palette-overrides
          '((border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)))
  (mlscroll-mode 1))


;;;; Clock
(with-eval-after-load 'time
  (setopt display-time-string-forms
          '((capitalize (format-time-string "  %a,%d %b %R "))))
  (display-time-mode 1))


;;; Modeline setup

;;;; Keyboard Macro
(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar-local bb-modeline-kbd-macro
  '(:eval
    (when (and defining-kbd-macro (mode-line-window-selected-p))
      mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")



;;;; VC
(defvar-local bb-modeline-vc-branch
    '(:eval
      (when-let (((mode-line-window-selected-p))
                 (filename (buffer-file-name))
                 (branch (car (vc-git-branches)))
                 ((char-or-string-p branch))
                 (state (vc-state filename 'Git))
                 (vcface (pcase state
                           ('added 'vc-locally-added-state)
                           ('edited 'vc-edited-state)
                           ('removed 'vc-removed-state)
                           ('missing 'vc-missing-state)
                           ('conflict 'vc-conflict-state)
                           ('locked 'vc-locked-state)
                           (_ 'vc-up-to-date-state))))
        (propertize (concat "  "  (capitalize branch) " ")
                    'face vcface
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to return propertized VC branch.")



;;;; Major mode
(defconst base-icons-alist
  (let ((assets-dir "~/CustomBuilds/nerdfont.vim/assets/json/"))
    (json-read-file
     (expand-file-name "basename.json" assets-dir)))
  "Basename icons for major-mode")

(defconst ext-icons-alist
  (let ((assets-dir "~/CustomBuilds/nerdfont.vim/assets/json/"))
    (json-read-file
     (expand-file-name "extension.json" assets-dir)))
    "Extension icons for major-mode")

(defun bb-modeline--style-major-mode()
  "Define symbols/icons to indicate major-mode in use."
  (let* ((file (if buffer-file-name (file-name-nondirectory buffer-file-name)))
         (known-icon
          (and file
               (or
                (alist-get (intern file) base-icons-alist)  ; Basename known
                (when-let ((file-ext (file-name-extension file)))  ; check for extension
                  (alist-get (intern file-ext) ext-icons-alist))))))
    (cond
     (known-icon (concat " " known-icon " "))
     ((string-match-p "^\\*[[:alnum:]]+\\*$" (buffer-name)) "   ")
     (t "  "))))

(defun bb-short-str-major-mode ()
  "Return capitalized string of `major-mode', with \"-mode\"  deleted."
  (capitalize (string-remove-suffix "-mode" (symbol-name major-mode))))

(defvar-local bb-modeline-major-mode
  '(:eval
    (list (propertize (bb-modeline--style-major-mode)
                      'local-map nil
                      'help-echo (bb-short-str-major-mode)
                      'mouse-face 'mode-line-highlight)))
    "Mode line construct for displaying major modes.")



;;;; Buffer Identification
(defvar-local bb-modeline-buffer-identification
    '(:eval
      (if (and (mode-line-window-selected-p)
               (bound-and-true-p breadcrumb-mode)
               (derived-mode-p 'prog-mode))
          (let* ((pcrumbs (breadcrumb-project-crumbs))
                 (icrumbs (breadcrumb-imenu-crumbs))
                 (sep (if icrumbs " : ")))
            (setq-local header-line-format nil)
            (delq nil (list pcrumbs sep icrumbs)))
        (propertized-buffer-identification " %b")))
  "Mode line construct for identifying the buffer being displayed.")



;;;; Flymake
(defvar-local bb-modeline-flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode)
               (mode-line-window-selected-p))
      (list flymake-mode-line-exception flymake-mode-line-counters)))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")



;;;; Misc Info
(defvar-local bb-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")



;;;; Modeline scroll bar
;; 2024-02-19  TODO => Hover action on scroll to compute (count-words--buffer-format)
(defvar-local bb-modeline-scroll
    '(:eval
      (and-let* (((bound-and-true-p mlscroll-mode))
                 ((mode-line-window-selected-p))
                 (ml (mlscroll-mode-line)))
        (put-text-property 0 (length ml) 'help-echo nil ml)
        ml))
  "Custom help-echo setup.")



;;;; Line/Column Pos%
(setq mode-line-position-column-line-format '("%l:%C ")
      mode-line-percent-position nil)  ; was '(-3 "%o")
(column-number-mode 1)

(defvar-local bb-modeline--position-col-line-props
  (list 'local-map nil
        'mouse-face 'unspecified
        'help-echo nil))

(defvar-local bb-modeline--line-and-column
    `((line-number-mode
       (column-number-mode
        (:propertize
         mode-line-position-column-line-format
         ,@bb-modeline--position-col-line-props)
        (:propertize
         mode-line-position-line-format
         ,@bb-modeline--position-col-line-props)
        (column-number-mode
         (:propertize
          mode-line-position-column-format
          ,@bb-modeline--position-col-line-props))))
      (:propertize
       (" " mode-line-percent-position)
       mouse-face 'unspecified))
  "Mode line construct for formatting `bb-modeline-position'.")

(defvar-local bb-modeline-position
    '(:eval
      (when (mode-line-window-selected-p)
        bb-modeline--line-and-column))
  "Mode line construct for the buffer position.")



;; 2024-02-19  TODO => Change modified, remote

;;;; Final modeline assembly
(setq-default mode-line-format
              '("%e"
                bb-modeline-kbd-macro
                " "
                ;; mode-line-mule-info
                mode-line-modified
                ;; mode-line-remote
                "  "
                bb-modeline-vc-branch
                " "
                bb-modeline-major-mode
                bb-modeline-buffer-identification
                mode-line-format-right-align
                bb-modeline-flymake
                bb-modeline-misc-info ;; everything else
                bb-modeline-scroll
                bb-modeline-position
                mode-line-end-spaces))

(defcustom bb--mode-line-defining-strings
  '(bb-modeline-kbd-macro
    bb-modeline-vc-branch
    bb-modeline-major-mode
    bb-modeline-buffer-identification
    bb-modeline-flymake
    bb-modeline-misc-info
    bb-modeline-scroll
    bb-modeline-position)
  "List of variables that compose and define the `mode-line-format'."
  :type '(repeat symbol)
  :group 'mode-line)


;; Making the variables that compose the mode-line 'risky-local-variable. This
;; is mandatory, otherwise they will not be displayed.
(mapc (lambda (variable)
        (put variable 'risky-local-variable t))
      bb--mode-line-defining-strings)



(provide 'bb-modeline)
;;; bb-modeline.el ends here
