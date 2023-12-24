;;; bb-modeline.el --- Configuration of the Mode-Line -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;; Clock in the modeline
(use-package time
  :demand t
  :init
  (setq display-time-string-forms
        '((capitalize (format-time-string "  %a,%d %b %R "))))
  :config
  (display-time-mode 1))


(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(defun bb-modeline--style-major-mode()
  "Return string referring `major-mode', capitalized and with \"-mode\" deleted."
  (capitalize
   (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local bb-modeline-major-mode
  '(:eval
    (list (propertize (bb-modeline--style-major-mode)
                      'mouse-face 'mode-line-highlight)
          '("" mode-line-process)
          " "))
  "Mode line construct for displaying major modes.")

(defvar-local prot-modeline-vc-branch
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
        (propertize (concat "   "  (capitalize branch))
                    'face vcface
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to return propertized VC branch.")


(defvar-local prot-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
       mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar-local prot-modeline-kbd-macro
  '(:eval
    (when (and defining-kbd-macro (mode-line-window-selected-p))
      mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")


(defvar-local prot-modeline-buffer-identification
    '(:eval
      (if (and (mode-line-window-selected-p)
               (bound-and-true-p breadcrumb-mode)
               (derived-mode-p 'prog-mode))
          (let* ((pcrumbs (breadcrumb-project-crumbs))
                 (icrumbs (breadcrumb-imenu-crumbs))
                 (sep (if icrumbs " : ")))
            (setq-local header-line-format nil)
            (delq nil (list pcrumbs sep icrumbs)))
        (propertized-buffer-identification "%b")))
  "Mode line construct for identifying the buffer being displayed.")


(defvar-local prot-modeline--line-and-column
    `((line-number-mode
       (column-number-mode
        (column-number-indicator-zero-based
         (:propertize
          mode-line-position-column-line-format
          ,@mode-line-position--column-line-properties)
         (:propertize
          (:eval (string-replace
                  "%c" "%C" (car mode-line-position-column-line-format)))
          ,@mode-line-position--column-line-properties))
        (:propertize
         mode-line-position-line-format
         ,@mode-line-position--column-line-properties))
       (column-number-mode
        (:propertize
         mode-line-position-column-format
         ,@mode-line-position--column-line-properties)
        (:propertize
         (:eval (string-replace
                 "%c" "%C" (car mode-line-position-column-format)))
         ,@mode-line-position--column-line-properties)))
      " "
      (:propertize
       ("" mode-line-percent-position)
       mouse-face mode-line-highlight)
      " ")
  "Mode line construct for formatting `prot-modeline-position'.")


(defvar-local prot-modeline-position
  '(:eval
    (when (mode-line-window-selected-p)
      prot-modeline--line-and-column))
  "Mode line construct for the buffer position.")

;; Line/Column Pos%
(setq mode-line-position-column-line-format '("%l:%c "))
(setq mode-line-percent-position '(-3 "%o"))
(column-number-mode 1)


(defvar-local prot-modeline-flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode)
               (mode-line-window-selected-p))
      ;;(flatten-tree (cons flymake-mode-line-format "  "))))
      ;;flymake-mode-line-format))
      (list flymake-mode-line-exception flymake-mode-line-counters " ")))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")


;; Final assembly for the mode-line
(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                prot-modeline-vc-branch
                "  "
                prot-modeline-buffer-identification
                mode-line-format-right-align
                prot-modeline-flymake
                " "
                bb-modeline-major-mode
                " "
                prot-modeline-position
                prot-modeline-misc-info ;; everything else not defined particularly
                mode-line-end-spaces))

(defcustom bb--mode-line-defining-strings
  '(prot-modeline-kbd-macro
    prot-modeline-buffer-identification
    bb-modeline-major-mode
    prot-modeline-flymake
    prot-modeline-position
    prot-modeline-vc-branch
    mode-line-format-right-align
    prot-modeline-misc-info)
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
