;;; bb-rust.el --- Rust setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; `rust-mode'
(use-package rust-mode)


;;;; `rust-ts-mode'
(use-package rust-ts-mode
  :bind (:map rust-ts-mode-map
              ("C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c C-c" . compile))
  :hook ((rust-ts-mode .
                  (lambda()
                    (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
                    (eglot-ensure)
                    (bb-programming-hooks)
                    (setq-local compile-command "cargo run"
                                indent-tabs-mode nil))))
  :config
  (prot-find-project-root rust-ts-mode "Cargo.toml"))



(provide 'bb-rust)
;;; bb-rust.el ends here
