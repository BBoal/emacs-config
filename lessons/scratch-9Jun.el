;; GNU Emacs 30.0.50 of 2023-04-16
;; Specific functions with modes

(let* ((mode-spec-func
        '((c++-mode . cpp-auto-include)
          (haskell-mode . ormolu-format-on-save-mode)
          (python . ))
         (closing-delim (if (>= dir 0)
                            (cdr (assq char pairs))
                          (car (rassq char pairs)))))
    (bb--find-occurrence (or closing-delim char)
                           dir)))

;;;;;;; Idea
(setq my-modes
      '((lisp-interaction-mode . testing)
        (emacs-lisp-mode . testing-two)))

(defun my-get-mode (mode)
  (alist-get mode my-modes))

(when-let ((fn (my-get-mode major-mode)))
  (if (listp fn)
      (dolist ...)
  (funcall fn)))


      Instead of:                     Alternative:                           Or even:
(cond                         (pcase major-mode                    (setq-local cmd (pcase major-mode
 ((eq major-mode 'MODE))        ('c++ (setq-local command ..))                           ('c++ val)
 ((eq major-mode 'MODE))        ('c (setq-local command ....))                           ('c val)
 ((eq major-mode 'MODE)))       (_ (setq-local command ....)))                           (_ val)))


;;;;;;;;;;;;;;;;;;;;
(defun bb-maybe-eval-string (string)
  "Maybe evaluate elisp in a given STRING."
  (or
   (ignore-errors (eval (car (read-from-string string))))
   string))

(defun bb-show-string-and-eval-in-other-buffer(string mode)
  "Uses another buffer to show and evaluate STRING according to MODE."
  (let ((buffer (get-buffer-create "testing")))
    (with-current-buffer buffer
      (funcall mode)
      (goto-char (point-min))
      (insert (format "%s %s %s%s"
                      string
                      comment-start
                      (bb-maybe-eval-string string)
                      "\n\014\n\n")))
    (display-buffer buffer
                    '((display-buffer-below-selected)
                      ;; TODO 2023-06-09: Check `body-function'
                      (window-height . 10)
                      (body-function . (lambda (window &rest _)
                                         (select-window window)
                                         (beginning-of-buffer)
                                         (recenter)
                                         (other-window 1)))))))

;; If you do not want the display buffer logic to be part of the
;; function:
;; (setq display-buffer-alist
;;       '(("testing"
;;          (display-buffer-below-selected)
;;          ;; TODO 2023-06-09: Check `body-function'
;;          (window-height . 10)
;;          (body-function . (lambda (window &rest _)
;;                             (select-window window)
;;                             (beginning-of-buffer)
;;                             (recenter)
;;                             (other-window 1))))))

(defun bb-eval-print-current-sexp-lisp()
  "Evaluate expression if point is right after ending parenthesis,
over one of the delimiters, or inside of a given sexp. Symbols are also
taken into consideration and proper evaluated."
  (interactive)
  ;; Major mode is not from the Lisp family
  (unless (derived-mode-p 'lisp-data-mode)
    (electric-newline-and-maybe-indent))
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
    (when-let ((string (thing-at-point 'sexp :no-properties)))
      (bb-show-string-and-eval-in-other-buffer string mode))))

(keymap-local-set "C-J" #'bb-eval-print-current-sexp-lisp)
;;;;;;;;;;;;;;;;;;;;
