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
