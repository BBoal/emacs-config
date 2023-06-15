;; GNU Emacs 30.0.50 of 2023-04-16
;; Initialization in 0.282s
;; Bruno Boal, be disciplined and maintain focus.

(defun bb--find-occurrence-t(char count)
  "This is a helper function usually called by:
`bb-find-occurrence-direction-kill-sexp'
`bb-find-occurrence-direction-kill-around-sexp'
`bb-zap-from-char-to-end'

Searches for CHAR, COUNTth times. Leaves point in occurrence
or produces an error. To achieve this effect, the `search-forward'
function is used.

Returns DIR, with possible values of 1, when the search is meant to be
forward; -1, when backward, or 0, when the occurrence matches either
the values of `line-end-position' or `line-beginning-position'."

  (let ((dir 1)
        (bounds (line-end-position)))
    (when (< count 0)
      (setq dir -1
            bounds (line-beginning-position)))
    (search-forward (format "%c" char) bounds nil count)
    (when (= (point) bounds)
      (setq dir 0))
    dir))



(defun bb-change-inside-char-pairs-t(char count)
  "."
  (interactive "cChange pair: \np")

    (let ((pair-num (cond
                      ((= count 0) 1)
                      (t (abs count))))
           (abs-count (/ count (abs count))))

      (while (> pair-num 1)
        (bb--pos-2nd-char char (bb--find-occurrence-t char abs-count))
        (setq pair-num (1- pair-num)))

      (bb--find-occurrence-t char abs-count)
      (setq first-char (point))
      (bb--pos-2nd-char char abs-count)
      (forward-char (- abs-count))
      (kill-region first-char (point))))


(defun bb--pos-2nd-char(char dir)
  (interactive "cChar: \np")
    (let* ((pairs '((?\( . ?\))
                   (?\{ . ?\})
                   (?\[ . ?\])
                   (?\< . ?\>)))
           (closing-delim (if (>= dir 0)
                              (cdr (assq char pairs))
                            (car (rassq char pairs)))))
      (bb--find-occurrence-t (or closing-delim char)
                             dir)))



(keymap-local-set "s-b" #'bb-change-inside-char-pairs-t)
(keymap-local-set "s-v" #'bb--pos-2nd-char)


(keymap-local-set "s-b" #'bb-change-inside-char-pairs-t) [] (keymap-local-set "" [dsfs]  #'bb-change-inside-char-pairs-t)

Test "1 klj" {1 fsdklf} /1 kldsf/ =1sdlfksd = -- 1 dfsdf -- <> Test "1 klj" [ klds] {1 fsdklf} /1 kldsf/ =1sdlfksd = -- 1 dfsdf -- <1sdkfj>
