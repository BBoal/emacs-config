;;; bb-lua.el --- Lua setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


(defun bb--lua-custom-variables()
  (setq-local comment-style 'extra-line
              comment-start "--\[\["
              lua-comment-start comment-start
              comment-end "--]]"
              comment-continue (make-string (length comment-start) ?\ )))


;;;; `lua-mode'
(use-package lua-mode
  :hook ((lua-mode . eglot-ensure)
         (lua-mode . bb--lua-custom-variables)))




(provide 'bb-lua)
;;; bb-lua.el ends here
