;;; package-init.el --- Package init -*- lexical-binding: t -*-

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


;;; Initializing
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/"))
      package-quickstart-file (concat user-emacs-directory "var/package-quickstart.el"))

;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities '(("elpa" . 1) ("melpa" . 2)))

(package-initialize)

;; Git is the only backend I use
(with-eval-after-load 'package-vc
  (setcdr package-vc-heuristic-alist nil))


;;;; `use-package'
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-defer t)


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
    theme-buffet
    tmr)
  "List of symbols representing the packages Prot develops/maintains.")

(setq package-pinned-packages
      `(,@(mapcar (lambda (package)
                    (cons package "elpa-devel"))
                  prot-emacs-packages)))


(defun bb-ensure-dir-or-file-exist (arg)
  "Create ARG if non-existant.  Prefix '/' is considered dir otherwise file."
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
      (if (called-interactively-p 'any)
          (user-error "Error: Destination already exists")))
     ((not (file-writable-p edir))
      (user-error "Error: Destination is not writable.  Check permissions"))
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
   (directory-files directory nil "\\`bb-.*\\.el\\'")))


(defun bb-require-lisp-files-in-dir-matching (parent-dir regex-string)
  "Require \\='bb-*.el\\=' files matched by REGEX-STRING with PARENT-DIR.
All Elisp files suffixed with \"bb-\" will be required through function
`bb-require-bb-lisp-files-in-dir', in directories matched by REGEX-STRING that
have PARENT-DIR as a parent directory.

e.g (bb-require-lisp-files-in-dir-matching user-emacs-directory \"lisp-\")

Will `require' all \\='bb-*.el\\=' files in
~/.emacs.d/{lisp-init,lisp-utils,lisp-user}"
(let ((dirs-to-load  (directory-files parent-dir :fullpath regex-string)))
  (while dirs-to-load
      (bb-require-bb-lisp-files-in-dir (car dirs-to-load))
      (setq dirs-to-load (cdr dirs-to-load)))))



(provide 'package-init)
;;; package-init.el ends here
