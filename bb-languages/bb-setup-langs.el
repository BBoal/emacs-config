;;; bb-setup-langs.el --- General functions for different langs -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `bb-programming-hooks'
(defun bb-programming-hooks ()
  "Useful hooks for programming."
  (interactive)
  (electric-pair-local-mode t)
  (subword-mode t)
  (indent-guide-mode t)
  (hs-minor-mode t))

;;;; `bb-eglot-arange-file'
(defun bb-eglot-arrange-file()
  "Imports and formats programming file using Eglot"
  (interactive)
  (if (eq major-mode 'c++-mode)
      (cpp-auto-include))
  (ignore-errors
    (eglot-code-action-organize-imports (point-min)))
  (eglot-format-buffer))




(provide 'bb-setup-langs)
;;; bb-setup-langs.el ends here
