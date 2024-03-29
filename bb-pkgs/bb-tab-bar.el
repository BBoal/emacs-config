;;; bb-tab-bar.el --- Configuration of Tab-Bar -*- lexical-binding: t -*-

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
;; An opinionated view of tab-bar

;;; Code:


(defun prot-project-in-tab--get-tab-names (&optional frame)
  "Return list of tab names associated with FRAME.
If FRAME is nil, use the current frame."
  (setq frame (or frame (selected-frame)))
  (mapcar
   (lambda (tab)
     (alist-get 'name tab))
   (frame-parameter frame 'tabs)))

(defun prot-project-in-tab--switch (directory)
  "Do the work of `project-switch-project' in the given DIRECTORY."
  (let ((command (if (symbolp project-switch-commands)
                     project-switch-commands
                   (project--switch-project-command)))
        (buffer (current-buffer)))
    (unwind-protect
        (progn
          (setq-local project-current-directory-override directory)
          (call-interactively command))
      (with-current-buffer buffer
        (kill-local-variable 'project-current-directory-override)))))

(defun prot-project-in-tab--create-tab (directory name)
  "Create new tab visiting DIRECTORY and named NAME."
  (tab-new)
  (find-file directory)
  (prot-project-in-tab--switch directory)
  (tab-rename name)
  ;; NOTE 2024-01-15 06:52 +0200: I am adding this because
  ;; `tab-rename' is not persistent for some reason. Probably a bug...
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-to-rename (nth (tab-bar--current-tab-index) tabs)))
    (setf (alist-get 'explicit-name tab-to-rename) name)))

;;;###autoload
(defun prot-project-in-tab (directory)
  "Switch to project DIRECTORY in a tab.
If a tab is named after the non-directory component of DIRECTORY,
switch to it.  Otherwise, create a new tab and name it after the
non-directory component of DIRECTORY.

Use this as an alternative to `project-switch-project'."
  (interactive (list (funcall project-prompter)))
  (project--remember-dir directory)
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (if (member name (prot-project-in-tab--get-tab-names))
        (tab-switch name)
      (prot-project-in-tab--create-tab directory name))))








;; 2023-12-30  TODO => Take a look at groups.


;;;;; Functions
;; 2023-08-13  TODO => Improve to include choices such as *scratch*
(defun bb--tab-bar-new-tab-choice ()
  "Allow the user to choose the type of the next new tab."
  (scratch-buffer))

(defun bb--tab-bar-new-tab-group ()
  "Set the group of the new tab to the name of the parent directory of file."
  (if (buffer-file-name (current-buffer))
      ;; (file-name-base buffer-file-name)
      (abbreviate-file-name (file-name-parent-directory buffer-file-name))
    (buffer-name)))

(defun bb--tab-bar-tab-group-format (tab i &optional current-p)
  "Default formatting for groups for TAB with number I.
Optionally CURRENT-P will refer to the current tab."
  (propertize
   (concat (if (and tab-bar-tab-hints (not current-p)) (format " [%d]  " i) "")
           (funcall tab-bar-tab-group-function tab))
   'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))


(defun bb-notmuch-indicator--global-string ()
  "Return the non-properties string of the concatenated notmuch indicators."
  (if (bound-and-true-p notmuch-indicator--counters)
      (mapconcat
       (lambda (field)
         (if (stringp field)
             (substring-no-properties field)))
       notmuch-indicator--counters)))

(defun bb--tab-bar-spaces-string-centering (string)
  "Calculate num spaces that STRING requires to be centered in tab."
  (if-let
      ((str-cols (string-width string))
       (tab-bar-max-width (cadr tab-bar-auto-width-max))
       ;; check if inside max boundary or exit
       ((< str-cols tab-bar-max-width))
       ;; Notmuch and date strings
       (cols-str-global-format
        (string-width
         (concat (bb-notmuch-indicator--global-string)
                 (caddar (tab-bar-format-global)))))
       ;; number of cols available for each tab
       (effect-cols-per-tab
        (/
         (- (frame-text-cols) cols-str-global-format)  ; available cols
         (length (tab-bar-tabs))))                     ; num tab-bars
       ;; calculate maximum value allowed
       (calc-max-str-cols (min effect-cols-per-tab tab-bar-max-width))
       ((< str-cols calc-max-str-cols)))
      ;; I'm only adding spaces on the left side hence the division by 2
      (ash (- calc-max-str-cols str-cols) -1)
    0))


(defun bb--tab-bar-tab-name-format (tab i)
  "Custom TAB format with number I."
  (let* ((tab-num (and tab-bar-tab-hints (format "%d " i)))
         (tab-name (alist-get 'name tab))
         (maybe-close-btn (and tab-bar-close-button-show
                               tab-bar-close-button))
         (string (concat tab-num tab-name maybe-close-btn))
         ;; 2023-12-30  TODO => Check memoization for usage here
         (num-spaces-add (bb--tab-bar-spaces-string-centering string)))
    (propertize
     (concat (make-string num-spaces-add ?\ ) string)
     'face (tab-bar-tab-face-default tab))))



;;;; `tab-bar'
(use-package tab-bar
  :config
  (setq tab-bar-auto-width-max `(,(string-pixel-width (make-string 28 ?\=)) 28)
        tab-bar-close-button-show nil
        tab-bar-close-last-tab-choice 'delete-frame
        tab-bar-close-tab-select 'left
        tab-bar-format '(tab-bar-format-tabs
                         tab-bar-format-align-right
                         notmuch-indicator-tab-bar-format
                         tab-bar-format-global)
        tab-bar-new-tab-choice 'bb--tab-bar-new-tab-choice
        tab-bar-new-tab-group 'bb--tab-bar-new-tab-group
        tab-bar-select-tab-modifiers '(super)
        tab-bar-tab-group-format-function 'bb--tab-bar-tab-group-format
        tab-bar-tab-group-function 'tab-bar-tab-group-default
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function 'bb--tab-bar-tab-name-format
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max (cadr tab-bar-auto-width-max)
        tab-bar-show t)

  ;;;; Enable the tab-bar
  :hook (after-init . tab-bar-mode))



(provide 'bb-tab-bar)
;;; bb-tab-bar.el ends here
