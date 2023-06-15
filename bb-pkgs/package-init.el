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
(when (not package-archive-contents)
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


(defun bb-require-bb-lisp-files-in-dir (directory)
  "Requires all elisp files prefixed with \"bb-\" in DIRECTORY."
  (mapcar
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
