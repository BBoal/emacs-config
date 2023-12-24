;;; bb-extras.el --- Useful extra functions -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

(require 'pp)

(defun expose (function &rest args)
  "Return an interactive version of FUNCTION with ARGS, \\='exposing\\=' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))


(defun numcores ()
  "Return the number of logical processors on this system.
Adapter from <https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el>"
  (cond
   ((and (eq system-type 'gnu/linux)
         (file-exists-p "/proc/cpuinfo"))
    (with-temp-buffer
      (insert-file-contents "/proc/cpuinfo")
      (how-many "^processor[[:space:]]+:")))
   ((eq system-type 'darwin)
    (with-temp-buffer
      (ignore-errors
        (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
          (string-to-number (buffer-string))))))
   (t
    (user-error "System is not Gnu Linux neither BSD based"))))


(defmacro measure-time (&rest body)
  "Measure and return the running time of BODY block."
  (declare (indent defun))
  ;; Fresh garbage collection before making any measurements.
  (garbage-collect)
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))


(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its VALUE.
Source <https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el>"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))


(defun find-all-files (&optional dir)
  "Visits all the non-hidden files in DIR recursively."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (setq dir (dired-current-directory)))
   ((not dir)
    (setq dir (read-directory-name "Base directory: " nil nil t nil)))
   ((string-equal dir "")
    (if (buffer-file-name)
        (setq dir (file-name-directory buffer-file-name))
      (user-error "Unable to guess the directory to work with")))
   ((not (stringp dir))
    (user-error "Directory must be given as a STRING"))
   ((not (file-directory-p dir))
    (user-error "Cannot find directory %s" dir))
   (t
    (message "Visiting the files recursively...")))
  (mapc (lambda (file)
          (find-file-noselect file))
        (directory-files-recursively dir "^[^.]" nil t t)))


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG or if current buffer is not visiting a file, will prompt for a
file to visit.  Source
<https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el>"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun pp-macroexpand-all-last-sexp (arg)
  "Run `macroexpand-all' on sexp before point.
With argument ARG, pretty-print output into current buffer, ignoring leading
comment characters.  Source
<https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el>."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (pp-last-sexp))))
    (pp-display-expression (macroexpand-all (pp-last-sexp))
                           "*Pp Macroexpand Output*")))



(provide 'bb-extras)
;;; bb-extras.el ends here
