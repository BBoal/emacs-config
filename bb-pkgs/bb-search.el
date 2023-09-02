;;; bb-search.el --- Search and Find stuff -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; I want Emacs to write the list of bookmarks to the `bookmark-file'
;; as soon as I set a new bookmark.  The default behaviour of Emacs is
;; to write to the disk as a final step before closing Emacs.  Though
;; this can lead to data loss, such as in the case of a power failure.
;; Storing the data outright mitigates this problem.
(defun prot-bookmark-save-no-prompt (&rest _)
  "Run `bookmark-save' without prompts.

The intent of this function is to be added as an :after advice to
`bookmark-set-internal'.  Concretely, this means that when
`bookmark-set-internal' is called, this function is called right
afterwards.  We set this up because there is no hook after
setting a bookmark and we want to automatically save bookmarks at
that point."
  (funcall 'bookmark-save))

(advice-add 'bookmark-set-internal :after 'prot-bookmark-save-no-prompt)



;;;; I-search
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?")




;;;; `substitute'
(use-package substitute
  :defer 3)


(provide 'bb-search)
;;; bb-search.el ends here
