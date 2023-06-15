;;; bb-lua.el --- Lua setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `lua-mode'
(use-package lua-mode
  :hook (lua-mode . eglot-ensure))



(provide 'bb-lua)
;;; bb-lua.el ends here
