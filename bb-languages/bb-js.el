;;; bb-js.el --- Javascript setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `js-mode'
(with-eval-after-load 'js
  (add-hook 'js-mode-hook #'js-ts-mode)
  (add-hook 'js-ts-mode-hook
            (lambda()
              (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
              (eglot-ensure)
              (bb-programming-hooks)))

  (prot-find-project-root js-ts-mode "jsconfig.json"))

(provide 'bb-js)
;;; bb-js.el ends here
