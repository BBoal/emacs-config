;;; package-init.el --- Package init -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;; Initializing
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")))

;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities
      '(("melpa" . 2)
        ("elpa" . 1)))

(package-initialize)


;;;; `use-package'
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-defer t)


;;;; `auto-package-update'
(use-package auto-package-update
  :hook ((auto-package-update-after . package-autoremove)
         (auto-package-update-after . package-quickstart-refresh))
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 2)
  :config
  (auto-package-update-maybe))


;;;; `use-package-ensure-system-package'
(use-package use-package-ensure-system-package)


(defvar prot-emacs-packages
  '(agitate
    beframe
    denote
    dired-preview
    ef-themes
    fontaine
    lin
    logos
    modus-themes
    notmuch-indicator
    pulsar
    spacious-padding
    standard-themes
    substitute
    tmr)
  "List of symbols representing the packages Prot develops/maintains.")

(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "elpa-devel"))
           prot-emacs-packages)))


(defun bb-ensure-dir-or-file-exist (arg)
  "Creates ARG if non-existant. Prefix '/' is considered dir otherwise file."
  (interactive "G")
  (if (or (listp arg) (not (stringp arg)))
    (user-error "ERR: Argument must be a solo string"))
  (let* ((pdir arg)
         (edir (progn
                 (while (not (file-exists-p pdir))
                   (setq pdir (file-name-parent-directory pdir)))
                 pdir))
         (bdir (file-name-directory arg)))
    (cond
     ((file-exists-p arg)
      (if (called-interactively-p)
          (user-error "Error: Destination already exists")))
     ((not (file-writable-p edir))
      (user-error "Error: Destination is not writable. Check permissions"))
     ((string-suffix-p "/" arg)
      (message "Creating dir %s" arg)
      (make-directory arg :parents))
     ((not (string-equal edir bdir))
      (message "Creating dir %s" bdir)
      (make-directory bdir :parents)
      (message "Creating file %s" arg)
      (write-region "" nil arg))
     (t
      (write-region "" nil arg)))
    arg))


(defun bb-require-bb-lisp-files-in-dir (directory)
  "Requires all elisp files prefixed with \"bb-\" in DIRECTORY."
  (mapc
   (lambda (file)
     (require (intern (file-name-sans-extension file))))
   (directory-files directory nil "^bb-.*\.el$")))


(defun bb-require-lisp-files-in-dir-matching (parent-dir regex-string)
  "Require all elisp files suffixed with \"bb-\" through function
 `bb-require-bb-lisp-files-in-dir', in directories matched by REGEX-STRING that
have PARENT-DIR as a parent directory.

e.g  (bb-require-lisp-files-in-dir-matching user-emacs-directory \"lisp-\")

Will `require' all \'bb-*.el\' files in ~/.emacs.d/{lisp-init,lisp-utils,lisp-user}"
(let ((dirs-to-load  (directory-files parent-dir :fullpath regex-string)))
  (while dirs-to-load
      (bb-require-bb-lisp-files-in-dir (car dirs-to-load))
      (setq dirs-to-load (cdr dirs-to-load)))))


(provide 'package-init)
;;; package-init.el ends here
