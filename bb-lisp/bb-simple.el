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
    (goto-char (or char (pos-bol)))
    (re-search-forward bb-common-url-regexp (pos-eol) :noerror)))

;;;###autoload
(defun bb-simple-escape-url-line (&optional char)
  "Escape all URLs or email addresses on the current line.
By default, start operating from `pos-bol' to the
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
    (setq beg (pos-bol))
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

;;;###autoload
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

;;;###autoload
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


(defun bb-simple-ç-dwim(arg)
  (interactive "p")
  (if (< arg 0)
    (message "This function only searches forward. Ignoring the signed prefix"))
  (bb--simple-ç-or-Ç-worker (abs arg)))

(defun bb-simple-Ç-dwim(arg)
  (interactive "p")
  (if (< arg 0)
      (message "This function only searches backward. Ignoring the signed prefix"))
  (bb--simple-ç-or-Ç-worker (- (abs arg))))

(defun bb--simple-ç-or-Ç-worker(arg)
  "Do-What-I-Mean behavior for the 'ç' or 'Ç' key.

The DWIM behavior of this command is as follows:

- Tries to expand possible yasnippet template before point
- If major-mode is prog-mode derived, jump through special chars defined in
`bb-prog-langs-alist' variable
- In every other case use insert specific character."
  ;; Let's assume a positive arg (forward search)
  (let ((search-bounds 'end-of-paragraph-text)
        (char-number 231)) ; ç
    ;; in case of a wrong assumption
    (if (< arg 0)
      (setq search-bounds 'start-of-paragraph-text
            char-number 199)) ; Ç
  (cond
   ((and (or (yas-minor-mode) (yas-global-mode))
         (yas-expand)))
   ((derived-mode-p 'prog-mode)
    (bb-try-jump-args-direction arg search-bounds))
   (t
    (insert (char-to-string char-number))))))



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


;;;###autoload
(defun bb-delete-blank-lines-dwim(beg end)
  "Delete all blank lines either surrounding point or, between BEG and END."
  (interactive "*r")
  (let ((regexp "^[ \t]*$"))
    (if (region-active-p)
        (flush-lines regexp (region-beginning) (region-end) nil)
      (delete-blank-lines)
      (if (looking-at regexp) (delete-blank-lines)))))


;;;###autoload
(defun bb-simple-kill-current-buffer()
  "Kills the current buffer if saved, otherwise prompts you."
  (interactive)
  (kill-buffer (current-buffer)))



(provide 'bb-simple)
;;; bb-simple.el ends here
