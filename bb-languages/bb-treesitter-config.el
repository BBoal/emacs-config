;;; bb-treesitter-config.el --- treesit configs -*- lexical-binding: t -*-

;;; Commentary:
;;; Setup of the various languages available to treesitter

;;; Code:

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
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


(defun bb-treesit-install-all-gramars()
  "Installs all the tree-sitter grammars defined in
`treesit-language-source-alist'"
  (interactive)
  (let ((num-langs (length treesit-language-source-alist)))
    (dotimes (n num-langs)
      (treesit-install-language-grammar (car (nth n treesit-language-source-alist))))))


(provide 'bb-treesitter-config)
;;; bb-treesitter-config.el ends here
