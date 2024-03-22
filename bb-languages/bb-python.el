;;; bb-python.el --- Python setup -*- lexical-binding: t -*-

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

(require 'setup-langs)
(declare-function flymake-ruff-load "ext:flymake-ruff")


;; 2024-02-09  TODO => Setup Eglot for ruff and pyright
;; Ruff stuff is: diagnostics, code_action, formatting
;; pyright for the rest



;;;; `python-mode'
(use-package python-mode
  :functions get--formatee-for-ruff
  :config
  (defun get--formatee-for-ruff ()
    "Get the region or buffer for formatting.
The returned value is a string passed to `format-with-ruff'."
    (let ((beg (or (use-region-beginning) (point-min)))
          (end (or (use-region-end) (point-max)))
          beg-col end-col)
      (save-excursion
        (goto-char beg)
        (setq beg-col (current-column))
        (goto-char end)
        (setq end-col (current-column)))
      ;; ruff is based 1 when counting columns. In Emacs that is an option
      ;; defined in `column-number-indicator-zero-based'
      (if column-number-indicator-zero-based
          (setq beg-col (1+ beg-col)
                end-col (1+ end-col)))
      (concat "--range="
              (format "%s" (line-number-at-pos beg))
              (format ":%d" beg-col)
              (format "-%s" (line-number-at-pos end))
              (format ":%d" end-col))))

  (defun format-with-ruff ()
    "Use ruff to format the editing python region or visible buffer."
    (when (derived-mode-p 'python-base-mode)
      (start-process "Ruff formatter" nil
                     "ruff" "format" (format "--line-length=%d" fill-column)
                     (get--formatee-for-ruff)
                     buffer-file-name)))


  (defun bb-python-configs ()
    "Personal hooks and commands to run when python mode is active"
    (when (executable-find "ruff")
      (add-hook 'after-save-hook 'format-with-ruff :depth :local)
      (flymake-ruff-load))
    (eglot-ensure)
    (bb-programming-hooks)
    (setq-local python-indent-guess-indent-offset-verbose nil
                python-indent-offset 4
                indent-bars-treesit-support t
                indent-bars-no-descend-string t
                indent-bars-treesit-ignore-blank-lines-types '("module")
                indent-bars-treesit-wrap '((python
                                            argument_list
                                            parameters
                                            list list_comprehension
                                            dictionary
                                            dictionary_comprehension
                                            parenthesized_expression
                                            subscript))))

  :hook (python-base-mode . bb-python-configs))




;;;; `flymake-ruff'
(use-package flymake-ruff)




;;;; `anaconda-mode'
(use-package anaconda-mode)




;;;; `live-py-mode'
(use-package live-py-mode)




;;;; `poetry'
(use-package poetry)



(provide 'bb-python)
;;; bb-python.el ends here
