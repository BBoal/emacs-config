;;; bb-js.el --- Javascript setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `js-mode'
(with-eval-after-load 'js
  (add-hook 'js-mode-hook
            (lambda()
              (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
              (eglot-ensure)
              (bb-programming-hooks)))

  (defun project-find-jsconfig-json (dir)
    (when-let ((root (locate-dominating-file dir "jsconfig.json")))
      (cons 'jsconfig-json root)))

  (cl-defmethod project-root ((project (head jsconfig-json)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-jsconfig-json))



(provide 'bb-js)
;;; bb-js.el ends here
