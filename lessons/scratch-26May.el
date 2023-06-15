;; GNU Emacs 30.0.50 of 2023-04-16
;; Initialization in 0.276s
;; Bruno Boal, be disciplined and maintain focus.

gaming crunch time


asciidoctor

keybindings


(defvar-keymap testing-prefix-map
  "a" #'move-beginning-of-line)

(keymap-global-set "C-z" testing-prefix-map)

(defvar-keymap testing-prefix-map
  "e" #'move-end-of-line)

(define-key testing-prefix-map "i" testing-prefix-i-map)
