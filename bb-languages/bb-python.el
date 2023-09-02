;;; bb-python.el --- Python setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `python-mode'
(use-package python-mode
  :hook (python-mode . (lambda()
                          (bb-programming-hooks)
                          (eglot-ensure)
                          (flymake-ruff-load)))
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (python-black-on-save-mode t))




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
