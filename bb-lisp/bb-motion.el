;;; bb-motion.el --- Motion and navegation functions -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;;; Window Management
(defun bb/split-window-right-and-focus()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun bb/split-window-below-and-focus()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun bb/kill-buffer-and-delete-window()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(defun bb/revert-buffer-no-confirm()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun bb/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun bb/jump-to-mark()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun bb/exchange-point-and-mark-no-activate()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; ;; https://www.emacswiki.org/emacs/BackwardDeleteWord
;; (defun delete-word (arg)
;;   "Delete characters forward until encountering the end of a word.
;; With argument, do this that many times."
;;   (interactive "p")
;;   (if (use-region-p)
;;       (delete-region (region-beginning) (region-end))
;;     (delete-region (point) (progn (forward-word arg) (point)))))

(defun bb/kill-beg-line()
  "Kills until the beginning of the text in current line.
If no text exists between point and the start of the line,
kills the text before point."
  (interactive)
  (let ((end (point)))
    (beginning-of-line-text)
    (if (= end (point))
        (kill-line 0)
      (kill-region (point) end))))

(defun bb/insert-newline-below(&optional arg)
  "Inserts a new and indented line after the current one or, with prefix,
after ARG number of lines."
  (interactive "P")
  (when arg
    (forward-line arg))
  (move-end-of-line nil)
  (newline-and-indent))

(defun bb/insert-newline-above(&optional arg)
  "Inserts a new and indented line before the current one or, with prefix,
before ARG number of lines."
  (interactive "P")
  (if arg
      (forward-line (- (1+ arg)))
    (forward-line -1))
  (if (/= (line-number-at-pos) 1)
      (bb/insert-newline-below)
    (newline-and-indent)
    (forward-line -1)))

(defun bb/kill-ring-save-line()
  "TODO save region if selected as argument"
  (interactive)
  (kill-ring-save (pos-bol) (pos-eol)))

(defun bb/duplicate-line()
  "TODO duplicate selected region if present"
  (interactive)
  (let ((dif-end-point
         (- (pos-eol) (point))))
    (bb/kill-ring-save-line)
    (move-end-of-line nil)
    (newline)
    (yank-in-context)
    (cdr kill-ring)
    (goto-char (- (point) dif-end-point))))



(provide 'bb-motion)
;;; bb-motion.el ends here
