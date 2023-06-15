
(message (format "%s" "dklsfkl"))

;;V3
(defun bb-eval-print-current-sexp-lisp()
  (interactive)
  ;; Major mode is not from the Lisp family
  (unless (derived-mode-p 'lisp-data-mode)
    (electric-newline-and-maybe-indent))
  (let* ((orig-point-pos (point)))
    (cond
     ((or (looking-at ")")
          (eq ?\) (char-before)))
      (let ((end (if (looking-at )point))
            (beg (forward-list -1 :interactive)))
        (bb-func-to-do origin beg end)))
     ((or (looking-at "(")
            (re-search-forward "(" (buffer-end -1) t -1))
      (let ((beg (point))
            (end (forward-list 1 :interactive)))
        (cond
         ;; in case of a symbol evaluation, the previous sexp is before
         ;; the aforementioned symbol.
         ((> orig-point-pos end)
          (goto-char orig-point-pos)
          ;;(eval-print-last-sexp)
          )
         ;; If "somewhere" inside the top sexp, just eval-print
         ((= (line-end-position) end)
          ;;(eval-print-last-sexp)
          )
         ;; All the other cases
         (t
          (kill-ring-save beg end)
          (move-end-of-line nil)
          (newline-and-indent 2)
          (yank-in-context)
          (cdr kill-ring)))
        (eval-print-last-sexp)
        (goto-char end))
      (eval-print-last-sexp)
      (goto-char orig-point-pos))))

(keymap-local-set "C-j" #'bb-eval-print-current-sexp-lisp)
