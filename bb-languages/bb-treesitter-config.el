;;; bb-treesitter-config.el --- treesit configs -*- lexical-binding: t -*-

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
;;; Setup of the various languages available to treesitter

;;; Code:

(defvar treesit-language-source-alist
  '((awk "https://github.com/Beaglefoot/tree-sitter-awk")
    (bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
    (heex "https://github.com/phoenixframework/tree-sitter-heex")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (numbat "https://github.com/irevoire/tree-sitter-numbat")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
    (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  "Alist containing programming major modes with correspondent URL repos.")

(setq major-mode-remap-alist
      '((awk-mode . awk-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (mhtml-mode . html-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (rust-mode . rust-ts-mode)
        (ocaml-mode . ocaml-ts-mode)
        (haskell-mode . haskell-ts-mode)
        (yaml-mode . yaml-ts-mode)))


;; 2024-01-04  TODO => https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun bb-treesit-install-all-gramars()
  "Install all grammars defined in `treesit-language-source-alist'."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (treesit-install-language-grammar (car grammar))))



(provide 'bb-treesitter-config)
;;; bb-treesitter-config.el ends here
