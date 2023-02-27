;;; package --- Summary
;;; Commentary:
;;; Code:

(defun get-returns(str)
  "Using dolist, reverse the order of STR."
  (let ((value)
	(list (split-string str ",")))
    (dolist (element list value)
      (setq value (cons (car(split-string element)) value)))
    (string-join (reverse value) ",")))

(provide '.yas-setup)
;;; .yas-setup.el ends here
