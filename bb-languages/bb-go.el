;;; bb-go.el --- Golang setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `go-mode'
(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-c" . compile))
  :hook ((go-mode .
                  (lambda()
                    (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
                    (eglot-ensure)
                    (bb-programming-hooks))))
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  (setq-local compile-command "go build -v && go test -v && go vet"
              tab-width 4))


;;;; `go-add-tags'
(use-package go-add-tags
  :bind (:map go-mode-map
  		      ("C-c t" . go-add-tags))
  :config
  (setq go-add-tags-style 'snake-case))





(provide 'bb-go)
;;; bb-go.el ends here
