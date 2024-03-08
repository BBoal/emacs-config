;;; bb-ocaml.el --- OCaml setup -*- lexical-binding: t -*-

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
;;;

;;; Code:


;;;; `merlin'
(use-package merlin
  :defer 1
  :hook ((tuareg-mode caml-mode)))
  ;; :config
  ;; (require 'merlin-iedit))




;;;; `merlin-eldoc'
(use-package merlin-eldoc
  :defer 1
  :after merlin
  :hook ((tuareg-mode caml-mode) . merlin-eldoc-setup)
  :config
  (setopt eldoc-echo-area-use-multiline-p nil
          merlin-eldoc-max-lines 1
          merlin-eldoc-type-verbosity 'min
          merlin-eldoc-function-arguments t
          merlin-eldoc-doc t))



;;;; `ocamlformat'
(use-package ocamlformat
  :defer 1
  :after tuareg
  :hook (before-save . ocamlformat-before-save))


(provide 'bb-ocaml)
;;; bb-ocaml.el ends here
