;;; bb-common-lisp.el --- Common Lisp setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


(setq inferior-lisp-program "/usr/bin/clisp")




;;;; `sly'
(use-package sly
  :bind (:map sly-prefix-map
              ("M-h" . sly-documentation-lookup)))


(provide 'bb-common-lisp)
;;; bb-common-lisp.el ends here
