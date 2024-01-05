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
(declare-function python-black-on-save-mode "ext:python-black")

;;;; `python-mode'
(use-package python-mode
  :config
  (defun bb-python-configs ()
    "Personal hooks and commands to run when python mode is active"
    (bb-programming-hooks)
    (eglot-ensure)
    (flymake-ruff-load)
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
  (python-black-on-save-mode t)
  :hook (python-base-mode . bb-python-configs))




;;;; `python-black'
(use-package python-black)




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
