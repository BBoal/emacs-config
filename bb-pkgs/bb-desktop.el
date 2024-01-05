;;; bb-desktop.el --- Configuration of Desktop-mode -*- lexical-binding: t -*-

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


(defcustom default-name-desktop-dir ".emacs-desktop.d/"
  "Default name for directories that hold desktop files."
  :type '(directory)
  :group 'desktop)

(defcustom default-desktop-dirname (concat user-emacs-directory default-name-desktop-dir)
  "Directory that holds all desktop files not referencing specific projects."
  :type '(directory)
  :group 'desktop)


;;;;; Hooks for `desktop-mode'
;; (add-hook 'desktop-save-hook #'bb-set-desktop-file-for-save)
;; (add-hook 'desktop-no-desktop-file-hook #'bb-set-desktop-file-for-read)

(use-package desktop
  :functions bb-ensure-dir-or-file-exist
  :autoload bb--choose-dir-desktop-save bb-last-modified-in-dir desktop-release-lock
  :config
  (setq desktop-dirname (bb-ensure-dir-or-file-exist default-desktop-dirname)
        desktop-path `(,desktop-dirname) ;; 2023-09-02 TODO => Use this var to check for desktop-mode savefile

        desktop-file-name-format 'absolute
        ;; desktop-load-locked-desktop nil
        desktop-globals-to-save nil
        desktop-globals-to-clear nil
        desktop-lazy-idle-delay 1
        desktop-locals-to-save '(desktop-locals-to-save
                                 tab-bar-show
                                 truncate-lines
                                 case-fold-search
                                 case-replace
                                 fill-column
                                 overwrite-mode
                                 change-log-default-name
                                 line-number-mode
                                 size-indication-mode
                                 buffer-file-coding-system
                                 indent-tabs-mode
                                 tab-width
                                 indicate-buffer-boundaries
                                 indicate-empty-lines
                                 show-trailing-whitespace)
        desktop-modes-not-to-save '(tags-table-mode)
        desktop-restore-eager 6
        ;; desktop-restore-reuses-frames 'keep
        desktop-auto-save-timeout 0
        desktop-save t)


;;;;; Functions
  (defun bb-last-modified-in-dir (&optional directory)
    "Return the last modified file (full-path) in DIRECTORY."
    (interactive "DDirectory: ")
    (or directory
        (setq directory (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          (read-directory-name "Choose a directory: "))))
    (cond
     ((not (file-directory-p directory))
      (user-error "Error: Directory doesn't seem to exist"))
     ((not (file-accessible-directory-p directory))
      (user-error "Error: Unable to open directory.  Check permissions"))
     (t
      (let (files modified-times latest-file)
        (mapc
         (lambda (attr)
           (let ((names (expand-file-name (car attr))) ; expand-file-name takes '.' and '..'
                 (times (nth 5 attr))) ; 5th position is the modified-time attribute
             (setq files (cons (cons names times)
                               files)
                   modified-times (cons times modified-times))))
         (directory-files-and-attributes directory :full)) ; list of filenames and attrs
        ;; sort the times in descending order to get the most recent first
        (setq modified-times (nreverse (sort modified-times #'time-less-p))
              latest-file (car (rassq (car modified-times) files)))
        ;; What if the directory is empty? latest-file = directory
        (when (string= latest-file (expand-file-name directory))
          (setq latest-file ""))
        ;; Output the result
        (if (called-interactively-p 'any)
            (message latest-file)
          latest-file)))))


  (defun bb--choose-dir-desktop-save ()
    "Helper to set a possible directory string for saving desktop layout files."
    (let* ((dir (if buffer-file-name
                    (file-name-directory buffer-file-name)
                  default-desktop-dirname))
           (possible-dir (locate-dominating-file dir default-name-desktop-dir))
           (default-dir (abbreviate-file-name
                         (file-name-parent-directory default-desktop-dirname))))

      (when (not possible-dir)
        (let ((answer
               (yes-or-no-p
                "Unsure where to save desktop file: press 'y' to choose dir, 'n' to save in default ?")))
          (setq possible-dir (if answer
                                 (file-name-as-directory
                                  (read-directory-name "Insert directory where desktop-dir should be created: " dir))
                               default-dir))))
      (concat possible-dir default-name-desktop-dir)))


  ;; 2023-08-24 NOTE => Improve to allow user to choose existing .emacs-desktop
  ;; dir without creating another similar directory inside of the previous one.
  (defun bb-set-desktop-file-for-save(&optional directory filename)
    "Set the DIRECTORY and FILENAME for function `desktop-save'.
Avoids the *temp/scratch* buffer name."
    (interactive)

    (or directory (setq directory (bb--choose-dir-desktop-save)))
    (let* ((parent-dir (file-name-parent-directory directory))
           (existing-parent-dir (progn
                                  (while (not (file-directory-p parent-dir))
                                    (setq parent-dir (file-name-parent-directory parent-dir)))
                                  parent-dir))
           (writable-existing-parent-dir (file-writable-p existing-parent-dir))
           (dir-exists (file-directory-p directory))
           (dir-writable (file-writable-p directory)))
      (cond
       ((and dir-exists (not dir-writable))
        (user-error "Error: Unable to write to directory.  Check permissions"))
       ((and (not dir-exists) writable-existing-parent-dir)
        (if (yes-or-no-p "Directory doesn't exist but can be created.  Proceed? ")
            (progn
              (princ (format "Creating %s  ...  " directory))
              (unless (make-directory directory :parents) ;; meaning directory was created
                (princ "Done!")))
          (user-error "Exiting without creating dir")))
       ((and (not dir-exists) (not writable-existing-parent-dir))
        (user-error "Error: Unable to create child directory.  Check permissions of parent dir"))
       (t
        (princ "Processing desktop file... ")))

      ;; following block of code must be outside of `t' othewise when the
      ;; directory has to be created the filename will not be processed.
      (or filename
          (setq filename (file-name-with-extension
                          (concat
                           (format-time-string "%d%b%Y-")
                           (if-let ((buf-name (buffer-name))
                                    ((not (string-match "\\*[[:word:]]+\*" buf-name))))
                               buf-name
                             (buffer-name (window-buffer)))) "desktop")))
      ;; (add-to-list 'desktop-path directory)
      (setq desktop-dirname directory
            desktop-base-file-name filename)
      (desktop-save directory :release)
      (princ (format "Desktop file saved as %s%s"
                     desktop-dirname desktop-base-file-name))))


  (defun bb-set-desktop-file-for-read (&optional directory filename)
    "Set the desktop FILENAME name to be loaded from designated DIRECTORY.

This function can be called interactively with completion for the mentioned
variables or in Lisp code by assigning the arguments.

Interactively, if visiting a file, the function searches for
`default-name-desktop-dir' from present dir proceeding up in the hierarchy file
structure. If DIRECTORY is found, the latest modified FILENAME is suggested.

If no arguments are passed, DIRECTORY defaults to `desktop-dirname' and
  FILENAME defaults to the latest modified filename inside that directory."
    (interactive
     (let*
         ((possible-dir
           (when buffer-file-name
             (locate-dominating-file (file-name-directory buffer-file-name)
                                     default-name-desktop-dir)))
          (final-dir (if possible-dir
                         (concat possible-dir default-name-desktop-dir)
                       desktop-dirname))
          (file
           (read-file-name "Choose/Insert a filename to load: " final-dir nil nil
                           (file-name-nondirectory (bb-last-modified-in-dir final-dir)))))
       (list (file-name-directory file) (file-name-nondirectory file))))
    ;; If not interactive
    (or directory (setq directory desktop-dirname))
    (or filename (setq filename (bb-last-modified-in-dir directory)))
    ;; Error checking
    (let ((full-path (concat directory filename)))
      (cond
       ((not (stringp directory))
        (user-error "Error: Directory name should be passed as 'string'"))
       ((not (file-directory-p directory))
        (user-error "Error: Directory doesn't seem to exist"))
       ((not (file-accessible-directory-p directory))
        (user-error "Error: Unable to open directory.  Check permissions"))
       ((not (file-exists-p full-path))
        (user-error "Error: Filename doesn't seem to exist"))
       ((not (file-readable-p full-path))
        (user-error "Error: Unable to read file.  Check permissions"))
       (t
        ;; sets file to load
        (ignore-errors
          (desktop-release-lock))         ; releases lock if there's a desktop already loaded
        (add-to-list 'desktop-path directory)
        (setq desktop-dirname directory
              desktop-base-file-name filename)
        (desktop-read)                    ; actual loading of FILENAME in DIRECTORY
        ;; I don't want the filename to be added to file-name-history
        (setq file-name-history (delete filename file-name-history))
        ;; Refreshes the loaded theme to avoid theme collision
        (load-theme (car custom-enabled-themes))))))


  (defun bb-silent-desktop-read()
    "Kill the `No desktop file message` minibuffer message."
    (interactive)
    (call-interactively 'bb-set-desktop-file-for-read)))




(provide 'bb-desktop)
;;; bb-desktop.el ends here
