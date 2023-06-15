;;; bb-treesitter-config.el --- treesit configs -*- lexical-binding: t -*-

;;; Commentary:
;;; Setup of the various languages available to treesitter

;;; Code:

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash" "master" "src")
     (c "https://github.com/tree-sitter/tree-sitter-c" "master" "src")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "master" "src")
     (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
     (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (yaml-mode . yaml-ts-mode)))


(provide 'bb-treesitter-config)
;;; bb-treesitter-config.el ends here
