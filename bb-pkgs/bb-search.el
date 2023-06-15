;;; bb-search.el --- Search and Find stuff -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; I-search
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?")


;;;; `affe'
(use-package affe
  :defer 2
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))


;;;; `substitute'
(use-package substitute
  :demand t)



(provide 'bb-search)
;;; bb-search.el ends here
