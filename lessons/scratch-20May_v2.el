;; GNU Emacs 30.0.50 of 2023-04-16
;;(setq denote-directory (expand-file-name "~/projects/"))
;;(setq denote-known-keywords '("emacs"))

(defun bb--find-occurrence(char count)
  "Searches for CHAR, COUNTth times. Leaves point in occurrence
 or produces an error.

Under the hood, the `search-forward' function is used."
  (let ((bounds
         (if (>= count 0) (line-end-position) (line-beginning-position))))
    (search-forward (format "%c" char) bounds nil count)))


(defun bb--kill-sexp-in-direction(dir)
  "This is a helper function. Usually called by
 `bb-find-occurrences-direction-kill-sexp' or
 `bb-find-occurrences-direction-kill-around-sexp'.

If DIR is a positive number or 0, the kill direction is set forward and
bounded to the end of the line.
If DIR is negative, the direction becomes backward and the kill is bounded
to the beginning of the line.

On both cases, the function does not interfere with different lines, meaning
that the kill is restricted to the same line used during the funcall."
  (if (>= dir 0)
      (setq direction 1
            bounds (line-end-position))
    (setq direction -1
          bounds (line-beginning-position)))
  (unless (eq (point) bounds)
    (kill-sexp direction)))


(defun bb-find-occurrence-direction-kill-sexp(char count)
  "A search is conducted for a specific CHAR, COUNTth times.
If successful, then a sexp is killed in the direction defined by the sign of
COUNT. The kill is restricted to the same line.

See `bb--find-occurrence' and `bb--kill-sexp-in-direction'."
  (interactive "cKill sexp next to char: \np")
  (bb--find-occurrence char count)
  (bb--kill-sexp-in-direction count))

(defun bb-find-occurrence-direction-kill-around-sexp(char count)
    "A search is conducted for a specific CHAR, COUNTth times.
If successful, the area to kill is enlarged by means or moving the point
away from the desired kill area. The kill direction is determined by the sign of
the COUNT parameter and the kill command is restricted to the same line.

See `bb--find-occurrence' and `bb--kill-sexp-in-direction'."
  (interactive "cKill sexp around char: \np")
  (bb--find-occurrence char count)
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



(define-key global-map (kbd "s-c") #'bb-find-occurrence-direction-kill-sexp)
(define-key global-map (kbd "s-a") #'bb-find-occurrence-direction-kill-around-sexp)
(define-key global-map (kbd "s-z") #'bb-zap-from-char-to-end)
(define-key global-map [remap zap-to-char] #'zap-up-to-char)
