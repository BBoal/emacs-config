4
(message (concat (format "%s" "Blabberish ") (format "%s" "This is a test")))     ;;Tstest


(let ((string (buffer-substring-no-properties (point) (point-max))))
  (with-current-buffer (pop-to-buffer "testing")
    (insert string)))

(let ((string (buffer-substring-no-properties (point) (point-max)))
      (mode major-mode))
  (with-current-buffer (pop-to-buffer "testing")
    (insert string)
    (funcall mode)))

(defun test(arg)
  (message arg))

(with-demoted-errors)
(condition-case )


(defun t()
(interactive)
(let ((thing)
      (mode major-mode))
  (save-excursion
    (backward-up-list)
    (setq thing (thing-at-point 'sexp :no-properties)))
  (with-current-buffer (pop-to-buffer "testing")
    (insert (format "%s %s " thing comment-start))
    (funcall mode)
    (with-demoted-errors "Error: %S"
        (eval-last-sexp 1))
    (insert "\n\n\014\n\n"))))
