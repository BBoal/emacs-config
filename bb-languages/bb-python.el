;;; bb-python.el --- Python setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


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
(use-package python-black
  :defer 3)




;;;; `flymake-ruff'
(use-package flymake-ruff
  :defer 3)




;;;; `anaconda-mode'
(use-package anaconda-mode
  :defer 3)




;;;; `live-py-mode'
(use-package live-py-mode
  :defer 3)




;;;; `poetry'
(use-package poetry
  :defer 3)


(provide 'bb-python)
;;; bb-python.el ends here
