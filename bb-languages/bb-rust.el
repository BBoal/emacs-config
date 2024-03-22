;;; bb-rust.el --- Rust setup -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:

(require 'setup-langs)

;;;; `rust-mode'
;(use-package rust-mode)


;;;; `rust-ts-mode'
(use-package rust-ts-mode
  :defines eglot-stay-out-of
  :functions  bb-rust-ts-mode-project-find-function
              project-find-rust-ts-mode-root
              eglot-flymake-backend
  :bind (:map rust-ts-mode-map
              ("C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c C-c" . compile))
  :init
  (defun manually-activate-flymake ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode t))
  :hook ((rust-ts-mode .
                  (lambda()
                    (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
                    (eglot-ensure)
                    (bb-programming-hooks)
                    (add-to-list 'eglot-stay-out-of 'flymake)
                    (setq-local compile-command "cargo run"
                                indent-tabs-mode nil)))
         (eglot-managed-mode . manually-activate-flymake))
  :config
  (prot-find-project-root rust-ts-mode "Cargo.toml"))



;;;; `flymake-clippy
(use-package flymake-clippy
  :hook (rust-ts-mode . flymake-clippy-setup-backend))



(provide 'bb-rust)
;;; bb-rust.el ends here
