;;; bb-simple.el --- Simple functions and commands -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://github.com/BBoal/emacs-config
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons. A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:


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

;;;###autoload
(defun bb-simple-cycle-display-line-numbers ()
  (interactive)
  (if display-line-numbers
	  (display-line-numbers-mode 'toggle)
	(setq-local display-line-numbers 'visual)))

;;;###autoload
(defun bb-simple-indent-tabs-spaces-rest ()
  (interactive)
  (setq-local whitespace-style '( indentation::space face tabs spaces
								  trailing lines space-before-tab newline
								  empty space-after-tab space-mark tab-mark
								  newline-mark missing-newline-at-eof)
		      tab-width 2)
  (untabify (point-min) (point-max))
  (indent-tabs-mode nil))


(defun bb-simple-add-to-list-elements (receiver-list elements)
  "Add the ELEMENTS to the RECEIVER-LIST"
  (mapc
   (lambda (element)
     (add-to-list receiver-list element))
   elements))

(defun bb-simple--indent (tabs-or-spaces width)
  "Removes values in `'whitespace-style' that would have precedence over user
options. Then, new values are set accordingly TABS-OR-SPACES and WIDTH, adjusting
`tab-width' as well as `indent-tabs-mode'."
  (mapc
   (lambda (element)
     (delete element whitespace-style))
   '(indentation space-after-tab space-before-tab))
  (if (string= tabs-or-spaces "tabs")
      (progn
	    (bb-simple-add-to-list-elements
	     'whitespace-style '(indentation::tab space-after-tab::tab space-before-tab::tab))
	    (indent-tabs-mode 1))
    (bb-simple-add-to-list-elements
     'whitespace-style '(indentation::space space-after-tab::space space-before-tab::space))
    (indent-tabs-mode -1))
       (setq-local tab-width width))

(defun bb-simple--style-prompt()
  "Simple prompt for getting user choice of tabs or spaces"
  (completing-read "Indentation: tabs or spaces? "
		           '(tabs spaces) nil :require-match))

(defun bb-simple--tab-width-prompt()
  "Simple prompt for getting tab-width value"
  (read-number "tab-width: "))

(defun bb-simple-indent-tabs-or-spaces(style width)
  "Indents buffer with user choices STYLE and WIDTH obtained from
helper functions."
  (interactive
   (let ((style (bb-simple--style-prompt)))
     (list
      style
      (if (string= style "tabs")
	      (bb-simple--tab-width-prompt)
	    (default-value 'tab-width)))))
  (bb-simple--indent style width)
  (let ((min (point-min))
	    (max (point-max)))
    (indent-region min max 0)
    (indent-region min max)))


;;;###autoload
(defun bb-simple-keyboard-quit-dwim ()
  "Source: https://git.sr.ht/~protesilaos/dotfiles
Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))


;;;###autoload
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
     ;; Also, if beg is null. There is no previous sexp.
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
    ;; Don't eval the emptyness
    (when (current-word)
      (eval-print-last-sexp))
    (goto-char orig-point-pos)))


(provide 'bb-simple)
;;; bb-simple.el ends here
