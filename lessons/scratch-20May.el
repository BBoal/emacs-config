;; GNU Emacs 30.0.50 of 2023-04-16
;;(setq denote-directory (expand-file-name "~/projects/"))
;;(setq denote-known-keywords '("emacs"))

(defun bb--find-occurrence(char count)
  (interactive "cFind char: \np")
  (let ((bounds
         (if (>= count 0) (line-end-position) (line-beginning-position))))
    (search-forward (format "%c" char) bounds nil count)))


(defun bb-find-occurrence-direction-kill-sexp(char count)
  "Tries to find the COUNT'th occurrence of CHAR searching forward from point.
If successful in the current line, the symbolic expression after the
occurrence is killed. If you want the opposite function use
 `bb--find-occurrence-backward-kill-previous-sexp'.

Helper function `bb--find-occurrence' is used in the process."
  (interactive "cChar: \np")
  (bb--find-occurrence char count)
  (bb--kill-sexp-in-direction count))

(defun bb--kill-sexp-in-direction(dir)
  (if (>= dir 0)
      (setq direction 1
            bounds (line-end-position))
    (setq direction -1
          bounds (line-beginning-position)))
  (unless (eq (point) bounds)
    (kill-sexp direction)))


(defun bb-find-occurrence-direction-kill-around-sexp(char count)
  (interactive "cChar: \np")
  (bb--find-occurence char count)
  (if (>= count 0) (left-char) (right-char))
  (bb--kill-sexp-in-direction count))


(defun bb-zap-from-char-to-end(char count)
  "If COUNT is positive, tries to find the COUNT'th occurrence of CHAR
searching forward from point. Kills from occurrence to the end of line.

If COUNT is negative, tries to find the COUNT'th occurrence of CHAR
searching backward from point. Kills from occurrence to the beginning of
the line."
  (interactive "cZap from char: \np")
  (bb--find-occurrence char count)
  (kill-region (point) (if (>= count 0)
                           (line-end-position)
                         (line-beginning-position))))



(define-key global-map (kbd "s-f") #'bb--find-occurrence)
(define-key global-map (kbd "s-c") #'bb-find-occurrence-direction-kill-sexp)

(define-key global-map (kbd "s-a") #'bb-find-occurrence-direction-kill-around-sexp)

(define-key global-map (kbd "s-z") #'bb-zap-from-char-to-end)
(define-key global-map (kbd "M-z") #'zap-up-to-char)


;; (define-key global-map (sdflsfd) #'zap-up-to-char)
;;(define-key global-map (kbd "M-z") #'zap-up-to-char)


;;(line-number-at-pos 2861)
;;(forward-sexp )

;;(line-number-at-pos (scan-sexps 2868 1))
