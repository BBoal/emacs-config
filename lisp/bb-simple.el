;;; bb-simple.el --- Simple functions and commands -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <bruno.boal@tutanota.com>
;; Author: Bruno Boal <bruno.boal@tutanota.com>
;; URL: https://github.com/BBoal/emacs-config
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons. A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:

(defcustom bb-search-todo-keywords
  (concat "TODO\\|FIXME\\|NOTE\\|REVIEW"
	  "\\|HACK\\|WARNING\\|DEPRECATED\\|BUG")
  "Regexp with search to-do keywords."
  :type 'string
  :group 'bb-search)

(defun bb-search-occur-todo-keywords (&optional context)
  "Produce Occur buffer with `bb-search-todo-keywords'.
With optional numeric prefix argument for CONTEXT, show as many
lines before and after each match.

When called from Lisp CONTEXT must satisfy `natnump'.  A faulty
value is read as 0.

Also see `bb-search-grep-todo-keywords'."
  (interactive "P")
  (let* ((case-fold-search nil)
	 (num (cond
	       (current-prefix-arg
		(prefix-numeric-value current-prefix-arg))
	       (t (if (natnump context) context 0))))
	 (buf-name (format "*keywords in <%s>*" (buffer-name))))
    (occur-1 bb-search-todo-keywords num (list (current-buffer)) buf-name)))

(defvar bb-common-url-regexp
  (concat
   "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
   "[.@]"
   "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)/?\\>")
  "Regular expression to match (most?) URLs or email addresses.")

(defun bb-simple--pos-url-on-line (&optional char)
  "Return position of `bb-common-url-regexp' on line or at CHAR."
  (save-excursion
    (goto-char (or char (line-beginning-position)))
    (re-search-forward bb-common-url-regexp (line-end-position) :noerror)))

;;;###autoload
(defun bb-simple-escape-url-line (&optional char)
  "Escape all URLs or email addresses on the current line.
By default, start operating from `line-beginning-position' to the
end of the current line. With optional CHAR as a buffer
position, operate from CHAR to the end of the line."
  (interactive)
  (when-let ((regexp-end (bb-simple--pos-url-on-line char)))
    (save-excursion
      (goto-char regexp-end)
      (unless (looking-at ">")
	(insert ">")
	(search-backward "\s")
	(forward-char 1)
	(insert "<")))
      (bb-simple-escape-url-line (1+ regexp-end))))

;;;###autoload
(defun bb-simple-escape-url-region (beg end)
  "Apply `bb-simple-escape-url' on a region lines between BEG and END."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (error "There is no region!")))
    (unless (> end beg)
      (cl-rotatef end beg))
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (while (<= beg end)
      (bb-simple-escape-url-line beg)
      (beginning-of-line 2)
      (setq beg (point)))))

;;;###autoload
(defun bb-simple-escape-url-dwim ()
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'bb-simple-escape-url-region
     #'bb-simple-escape-url-line)))

(provide 'bb-simple)
;;; bb-simple.el ends here
