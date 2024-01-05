;;; bb-go.el --- Golang setup -*- lexical-binding: t -*-

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

;;;; `go-mode'
(use-package go-mode
  :defines go-ts-mode-map
  :functions bb-go-mode-project-find-function project-find-go-mode-root
  :bind (:map go-ts-mode-map
              ("C-c C-c" . compile))
  :hook ((go-mode .
                  (lambda()
                    (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
                    (eglot-ensure)
                    (bb-programming-hooks))))
  :config
  (prot-find-project-root go-mode "go.mod")
  (setq-local compile-command "go build -v && go test -v && go vet"
              tab-width 4))




;;;; `go-add-tags'
(use-package go-add-tags
  :after go-mode
  :defines go-ts-mode-map go-add-tags-style
  :bind (:map go-ts-mode-map
  		      ("C-c t" . go-add-tags))
  :config
  (setq go-add-tags-style 'snake-case))


(provide 'bb-go)
;;; bb-go.el ends here
