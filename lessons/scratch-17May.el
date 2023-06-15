;; GNU Emacs 30.0.50 of 2023-04-16
;; Initialization in 1.463s
;; Bruno Boal, be disciplined and maintain focus.


post-new directory .notmuch
notmuch hooks

(defun prot-simple-delete-pair-dwim ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

(define-key yas-minor-mode-map (kbd s-c) #'prot-simple-delete-pair-dwim)
