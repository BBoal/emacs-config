;;; package --- Summary
;;; Commentary:
;;; Code:

(defun comment-date-keyword(arg)
  "Inserts language comment with date, ARG must be given to complete call to action."
   (format "%s %s  %s => "
                        (make-string 2 (aref comment-start 0))
                        (format-time-string "%Y-%m-%d")
                        arg))


(provide '.yas-setup)
;;; .yas-setup.el ends here
