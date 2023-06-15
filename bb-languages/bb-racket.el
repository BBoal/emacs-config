;;; bb-racket.el --- Racket setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `racket-mode'
(use-package racket-mode
  :hook (racket-mode . eglot-ensure))


(provide 'bb-racket)
;;; bb-racket.el ends here
