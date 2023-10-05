;;; bb-motion.el --- Motion and navegation functions -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;; Window Management
(defun bb-split-window-right-and-focus()
    "Spawn and focus a new window right of the current where most recent
hidden buffer is showed."
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right)
  (switch-to-buffer (other-buffer)))

(defun bb-split-window-below-and-focus()
  "Spawn and focus a new window below the current where most recent
hidden buffer is showed."
  (interactive)
  (split-window-below)
  (windmove-down)
  (switch-to-buffer (other-buffer)))

(defun bb-kill-buffer-and-delete-window()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(defun bb-revert-buffer-no-confirm()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun bb-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun bb-jump-to-mark()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun bb-exchange-point-and-mark-no-activate()
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


(defun bb-kill-beg-line()
  "Kills from the beginning of the text in current line.
If no text exists between point and the start of the line,
kills the text before point."
  (interactive)
  (let ((end (point)))
    (beginning-of-line-text)
    (if (= end (point))
        (kill-line 0)
      (kill-region (point) end))))

(defun bb-kill-ring-save-line()
  "Save region when selected, or line otherwise."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (pos-bol) (pos-eol))))




;;;###autoload
(defun bb-insert-newline-above(arg)
  "Inserts a new line before the current one or, with prefix, ARG number of lines.

With negative prefix calls mirror function `bb-insert-newline-below' passing ARG."
  (interactive "p")
  (if (< arg 0) (bb-insert-newline-below (- arg)))
  (goto-char (pos-bol))
  (while (> arg 0)
    (insert "\n")
    (forward-line -1)
    (setq arg (1- arg)))
  (indent-according-to-mode))

;;;###autoload
(defun bb-insert-newline-below(arg)
  "Inserts a new line after the current one or, with prefix, ARG number of lines.

With negative prefix calls mirror function `bb-insert-newline-above' passing ARG."
  (interactive "p")
  (if (< arg 0) (bb-insert-newline-above (- arg)))
  (goto-char (pos-eol))
  (while (> arg 0)
    (insert "\n")
    (setq arg (1- arg)))
  (indent-according-to-mode))




(defun bb-duplicate-line-above-dwim(arg)
  "Duplicate line or region ARGth times below point/region.
If ARG is nil, do it one time."
  (interactive "p")
  (bb--duplicate-line (abs arg)
                      (if (> arg 0) -1 1)))


(defun bb-duplicate-line-below-dwim(arg)
  "Duplicate line or region ARGth times above point/region.
If ARG is nil, do it one time."
  (interactive "p")
  (bb--duplicate-line (abs arg)
                      (if (> arg 0) 1 -1)))


(defun bb--duplicate-line(count dir)
  "Duplicate line or region COUNTth times in DIRection according to prefix.
A positive prefix leave the duplicates above, a negative, below."

  (let ((start (pos-bol))
        (end (pos-eol))
        (opoint (point))
        (omark (mark))
        diff-eol-mark)

    (when-let (((use-region-p))
               (line-diff-mark-point (1+ (- (line-number-at-pos omark)
                                            (line-number-at-pos opoint)))))

      ;; Point is after Mark (selection downwards)
      (if (> opoint omark)    ; point is at the end. Correct assignment of end.
          (setq start (pos-bol line-diff-mark-point)) ; setting start to pos-bol of the mark
        ;; Point is before Mark (selection upwards)
        ;; point is at the beginning. Correct assignment of start.
        (setq end (pos-eol line-diff-mark-point)))    ;; setting end

      (setq diff-eol-mark (- opoint omark)))    ; distance between point and mark


    (let* ((diff-eol-point (1+ (- end opoint))) ; account for the concated "\n" below
           (deactivate-mark)
           (lines (concat (buffer-substring start end) "\n")))

      (when (> dir 0) ; dups above point
        (goto-char end)
        (forward-line 1)

        (while (> count 0)
          ;; Handle the special case when there isn't a newline as the eob.
          (if (and (eq (point) (point-max))
                   (/= (current-column) 0))
              (insert "\n"))

          (insert lines)
          (setq count (1- count))))


      (when (< dir 0) ; dups below point
        (while (> count 0)
          (goto-char start)
          (forward-line 0)
          (insert lines)
          (setq count (1- count))))

      ;;  either way go to same point location reference initial motion
      (goto-char (- (point) diff-eol-point))
      ;;  if user provided a region, set the mark properly
      (if diff-eol-mark (set-mark (- (point) diff-eol-mark))))))




(defun bb-transpose-words (arg)
  "Transpose words around point or around/after point and mark "
  (interactive "p")
  (transpose-words (if (region-active-p) 0 arg)))


(defun bb--move-line(count)
  "Move line or region COUNTth times in direction according to prefix.
      A positive prefix moves the line(s) below, a negative, above."

  (let ((start (pos-bol))
        (end (pos-eol))
        diff-eol-point
        diff-eol-mark)
    (when-let (((use-region-p))
               (pos (point))
               (mrk (mark))
               (line-diff-mark-point (1+ (- (line-number-at-pos mrk)
                                            (line-number-at-pos pos)))))
      (if (> pos mrk)
          (setq start (pos-bol line-diff-mark-point)) ; pos-bol of where the mark is
        (setq end (pos-eol line-diff-mark-point)))    ; pos-eol of the line where the mark is
      (setq diff-eol-mark (1+ (- end mrk))))          ; 1+ to get the \n
    ;; this is valid for region or a single line
    (setq diff-eol-point (1+ (- end (point))))
    (let* ((max (point-max))
           (end (1+ end))
           (end (if (> end max) max end))
           (deactivate-mark)
           (lines (delete-and-extract-region start end)))
      (forward-line count)
      ;; Handle the special case when there isn't a newline as the eob.
      (if (and (eq (point) max)
               (/= (current-column) 0))
          (insert "\n"))
      (insert lines)
      ;; if user provided a region
      (if diff-eol-mark
          (set-mark (- (point) diff-eol-mark)))
      ;; either way go to same point location reference initial motion
      (goto-char (- (point) diff-eol-point))))
  (indent-according-to-mode))

(defun bb--move-line-user-error (boundary)
  "Return `user-error' with message accounting for BOUNDARY.
      BOUNDARY is a buffer position, expected to be `point-min' or `point-max'."
  (when-let ((bound (line-number-at-pos boundary))
             (string-bound (if (= boundary (point-min))
                               "first line!"
                             "last line!"))
             (scope (cond
                     ;; Region
                     ((and (use-region-p)
                           (or (= (line-number-at-pos (point)) bound)
                               (= (line-number-at-pos (mark)) bound)))
                      "region is ")
                     ;; At last line, with solo "\n" and shifting above
                     ((and (eobp)
                           (= boundary (point-min))
                           (= (current-column) 0))
                      (setq string-bound "EOB! Unable to shift, moving the point up...")
                      (forward-line -1)
                      "")
                     ;; Second to last line with "\n" as eob next and shifting below
                     ((and (= boundary (point-max))
                           (= (line-number-at-pos (point)) (1- bound))
                           (save-excursion
                             (goto-char boundary)
                             (bolp)))
                      (setq string-bound "bottom, with only EOB below!")
                      "")
                     ;; Line
                     ((= (line-number-at-pos (point)) bound)
                      "")
                     ;; No errors
                     (t nil))))
    (user-error (format "WARNING: %salready at the %s" scope string-bound))))


(defun bb-move-line-above-dwim(arg)
  "Move line or region ARGth times up.
      If ARG is nil, do it one time."
  (interactive "p")
  (unless (bb--move-line-user-error (point-min))
    (bb--move-line (- arg))))


(defun bb-move-line-below-dwim(arg)
  "Move line or region ARGth times down.
      If ARG is nil, do it one time."
  (interactive "p")
  (unless (bb--move-line-user-error (point-max))
    (bb--move-line arg)))


(provide 'bb-motion)
;;; bb-motion.el ends here
