(message (concat (format "%s" "Blabberish ") (format "%s" "This is a test")))


(defun test()
  (message "tetste"))


(defun bb-eval-print-current-sexp-lisp()
  "Evaluate expression if point is right after ending parenthesis,
over one of the delimiters, or inside of a given sexp. Symbols are also
taken into consideration and proper evaluated."
  (interactive)
  ;; Major mode is not from the Lisp family
  (unless (derived-mode-p 'lisp-data-mode)
    (electric-newline-and-maybe-indent))
  (let ((orig-point-pos (point))
        beg
        end)
    (cond

     ;; Position after ")"
     ((eq ?\) (char-before (point)))
      ;; Go back to "("
      (setq end (point))
      (setq beg (forward-list -1 :interactive)))

     ;; On top of ")"
     ((eq ?\) (char-after (point)))
      (forward-char 1)
      (setq end (point))
      (setq beg (forward-list -1 :interactive)))

     ;; On top of "(" or inside a sexp
     ((if (looking-at "(") (setq beg (point))
        (setq beg (re-search-forward "(" (buffer-end -1) t -1)))
      (setq end (when beg (forward-list 1 :interactive)))))

    (cond
     ;; in case of a symbol evaluation, the previous sexp position, existing,
     ;; comes before the position of the aforementioned symbol.
     ;; ERROR if try to eval symbol without a previous sexp.
     ((or (null beg) (> orig-point-pos end))
      (goto-char orig-point-pos))
     ;; If "somewhere" inside the top sexp, just eval-print
     ;; HERE line-end-position should be last char in line not
     ;; counting with possible comments.
     ((= (line-end-position) end)
      (goto-char end))
     ;; All other cases
     (t
      (kill-ring-save beg end)
      (forward-sentence)
      (newline-and-indent 2)
      (yank-in-context)
      (cdr kill-ring)))
    (when (current-word)
      (eval-print-last-sexp))
    (goto-char orig-point-pos)))

(keymap-local-set "C-j" #'bb-eval-print-current-sexp-lisp)
