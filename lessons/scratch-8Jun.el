3

(message (concat (format "%s" "Blabberish ") (format "%s" "This is a test")))

(defun test()
  (message "tetste"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(defun bb-maybe-eval-string (string)
  "Maybe evaluate elisp in a given STRING."
  (or
   (ignore-errors (eval (car (read-from-string string))))
   string))

(defun bb-show-string-and-eval-in-other-buffer(string mode)
  (pop-to-buffer "testing")
  (funcall mode)
  (insert (format "%s %s %s%s"
                  string
                  comment-start
                  (bb-maybe-eval-string string)
                  "\n\014\n\n"))
  (other-window 1))   ;; Added because save-excursion is not working

(defun bb-eval-print-current-sexp-lisp()
  "Evaluate expression if point is right after ending parenthesis,
over one of the delimiters, or inside of a given sexp. Symbols are also
taken into consideration and proper evaluated."
  (interactive)
  ;; Major mode is not from the Lisp family
  (unless (derived-mode-p 'lisp-data-mode)
    (electric-newline-and-maybe-indent))
  (save-excursion
    (let ((orig-point-pos (point))
          (mode major-mode))

      (cond
       ;; If point is after ")"
       ((eq ?\) (char-before (point))))
       ;; If point is on top of "("
       ((looking-at "("))
       ;; If point is on top of ")"
       ((eq ?\) (char-after (point)))
        (forward-char 1))
       ;; All the other cases
       (t
        (if-let ((beg (re-search-forward "(" (buffer-end -1) t -1))
                 (end (forward-list 1 :interactive)))
            (if (or (null end) (> orig-point-pos end))
                (goto-char orig-point-pos)))))
            
      ;; Let's see what do we have for evaluation
      (if-let (string (thing-at-point 'sexp :no-properties))
          (bb-show-string-and-eval-in-other-buffer string mode)))))


(keymap-local-set "C-ç" #'bb-eval-print-current-sexp-lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;






(message (concat (format "%s" "Blabberish ") (format "%s" "This is a test")))     ;;Tstest

4



(print (maybe-eval-string "(format \"hello, %s\" \"world\")") (current-buffer))
"hello, world"
(insert "\n\014\n")


"hello, world"

(print (maybe-eval-string "+ 8 7)") (current-buffer))
"(+ 8 7)"

15

"hello, world"

(setq dog 12)

(insert (+ 4 9))
(if dog (message "bark")
  (message "..."))
(null dog)

(setq gj (re-search-forward "«" (buffer-end 1) t 1))


(if-let (dog (+ dog 18))
    (message "%s" dog))




    
      ;; (if (eq ?\) (char-after (point)))   ;; If point is on top of ")"
      ;;     (forward-char 1))               ;; put point after ")"
      ;; (unless
      ;;     (or
      ;;      (eq ?\) (char-before (point)))   ;; Point is after ")"
      ;;      (looking-at "("))                ;; or on top of "("
      ;;   ;; inside a sexp or anywhere else
      ;;   (setq beg (re-search-forward "(" (buffer-end -1) t -1)
      ;;         end (if beg (forward-list 1 :interactive)))
      ;;   ;; if didn't found a set of parenthesis or point if outside
      ;;   (if (or (null end) (> orig-point-pos end))
      ;;       (goto-char orig-point-pos)))



;; (defun t()
;; (interactive)
;; (let ((thing)
;;       (mode major-mode))
;;   (save-excursion
;;     (backward-up-list)
;;     (setq thing (thing-at-point 'sexp :no-properties)))

;; (defun bb-show-arg-in-other-buffer(arg)
;;   (with-current-buffer (pop-to-buffer "testing")
;;     (insert (format "%s %s " arg comment-start))
;;     (funcall mode)
;;     (with-demoted-errors "Error: %S"
;;         (eval arg))
;;     (insert "\n\n\014\n\n")))
(+ 3 4)
