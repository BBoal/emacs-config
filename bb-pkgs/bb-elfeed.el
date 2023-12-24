;;; bb-elfeed.el --- Elfeed config  -*- lexical-binding: t -*-

;;; Commentary:
;;; RSS, news and podcast. An all in one solution

;;; Code:

(defvar urls '((https://blog.patternsinthevoid.net/feeds/all.atom.xml)
               (https://mbreen.com/rss.xml)
               (https://beej.us/blog/rss.xml)
               (https://yannherklotz.com/index.xml)
               (https://fabiensanglard.net/rss.xml)
               (https://drewdevault.com/blog/index.xml))
  ".")


;;;; `elfeed'
(use-package elfeed
  :hook (elfeed-show-mode . (lambda () (setq-local shr-width (current-fill-column))))
  :config
  (setq elfeed-use-curl nil))



(provide 'bb-elfeed)
;;; bb-elfeed.el ends here
