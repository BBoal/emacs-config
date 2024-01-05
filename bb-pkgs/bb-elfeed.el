;;; bb-elfeed.el --- Elfeed config  -*- lexical-binding: t -*-

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
;; RSS, news and podcast.  An all in one solution

;;; Code:


;; 2024-01-04  REMINDER => Improve

(defvar bb-urls '((https://blog.patternsinthevoid.net/feeds/all.atom.xml)
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
  (setopt elfeed-use-curl nil))



(provide 'bb-elfeed)
;;; bb-elfeed.el ends here
