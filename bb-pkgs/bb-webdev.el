;;; bb-webdev.el --- Web Development packages -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


(use-package websocket
  :demand t)

(use-package deno-bridge
  :after websocket
  :vc ( :url "https://github.com/manateelazycat/deno-bridge"
        :rev :newest))

(use-package emmet2-mode
  :after deno-bridge
  :vc ( :url "https://github.com/P233/emmet2-mode"
        :rev :newest)
  :hook (mhtml-mode html-ts-mode sgml-mode css-mode))


(provide 'bb-webdev)
;;; bb-webdev.el ends here
