;;; bb-tab-bar.el --- Configuration of Tab-Bar -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

(use-package tab-bar
  :defer 1
  :config
  (setq tab-bar-auto-width-max '(220 27) ; tweaked with (string-pixel-width (make-string 27 ?\=))
        tab-bar-close-button-show 'selected
        tab-bar-close-last-tab-choice 'delete-frame
        tab-bar-close-tab-select 'left
        tab-bar-format '(tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         notmuch-indicator-tab-bar-format
                         tab-bar-format-global)
        tab-bar-new-tab-choice 'bb--tab-bar-new-tab-choice
        tab-bar-new-tab-group 'bb--tab-bar-new-tab-group
        tab-bar-select-tab-modifiers '(super)
        tab-bar-tab-group-format-function 'bb--tab-bar-tab-group-format-default
        tab-bar-tab-group-function 'tab-bar-tab-group-default
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function 'bb--tab-bar-tab-name-format-default
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max (cadr tab-bar-auto-width-max)
        tab-bar-show t)

;;;;; Functions
  ;; 2023-08-13  TODO => Improve to include choices such as *scratch*
  (defun bb--tab-bar-new-tab-choice()
    "Allows the user to choose the type of the next new tab."
    (call-interactively 'find-file))

  (defun bb--tab-bar-new-tab-group()
    "Sets the group of the new tab to the name of the parent directory of file."
    (if (buffer-file-name (current-buffer))
        ;; (file-name-base buffer-file-name)
        (abbreviate-file-name (file-name-parent-directory buffer-file-name))
      (buffer-name)))

  (defun bb--tab-bar-tab-group-format-default (tab i &optional current-p)
    (propertize
     (concat (if (and tab-bar-tab-hints (not current-p)) (format " [%d]  " i) "")
             (funcall tab-bar-tab-group-function tab))
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

  (defun bb--tab-bar-tab-name-format-default (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (string (concat (if tab-bar-tab-hints (format "%d\. " i) "")
                           (alist-get 'name tab)
                           (or (and tab-bar-close-button-show
                                    (not (eq tab-bar-close-button-show
                                             (if current-p 'non-selected 'selected)))
                                    tab-bar-close-button)
                               "")))
           (dif-widthmax-widthstring (- (cadr tab-bar-auto-width-max) (string-width string)))
           (spaces2add (if (<= dif-widthmax-widthstring 0)
                           0
                         (floor dif-widthmax-widthstring 2))))
      (propertize
       (concat (make-string spaces2add ?\ ) string)
       'face (funcall tab-bar-tab-face-function tab))))

;;;;; Enable the tab-bar
  (add-hook 'after-init-hook  #'tab-bar-mode))



(provide 'bb-tab-bar)
;;; bb-tab-bar.el ends here
