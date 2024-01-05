;;; bb-motion.el --- Motion and navegation functions -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
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
;; General Public License foq more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:


;;;; Window Management
(defun bb-split-window-right-and-focus ()
  "Spawn and focus a new window right of the current.
In the new window the most recent hidden buffer is showed."
  (interactive)
  (split-window-right)
  (windmove-right)
  (switch-to-buffer (other-buffer)))

(defun bb-split-window-below-and-focus ()
  "Spawn and focus a new window below the current.
In the new window the most recent hidden buffer is showed."
  (interactive)
  (split-window-below)
  (windmove-down)
  (switch-to-buffer (other-buffer)))

(defun bb-kill-buffer-and-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(defun bb-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun bb-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun bb-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun bb-exchange-point-and-mark-no-activate()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun bb-kill-beg-line ()
  "Kill from the beginning of the text in current line until point.
If no text exists between point and the start of the line,
kills the white-space before point."
  (interactive)
  (let ((end (point)))
    (beginning-of-line-text)
    (if (= (point) end)
        (kill-line 0)
      (kill-region (point) end))))

(defun bb-kill-ring-save-line (arg)
  "Save region or, with prefix, ARG number of lines, backward or forward."
  (interactive "p")
  (if (= arg 0) (user-error "ERROR: Invalid numeric prefix"))
  (if (use-region-p)
      (progn
        (kill-ring-save (use-region-beginning) (use-region-end))
        (message "Region yanked to the kill-ring"))
    (let (beg end real-yanked)
      (if (> arg 0)
          (setq beg (pos-bol)
                end (save-excursion
                      (setq real-yanked (- arg (forward-line (1- arg))))
                      (pos-eol)))

        ;; (< arg 0)
        (setq beg (pos-eol)
              end (save-excursion
                    (setq real-yanked (- (forward-line (1+ arg)) arg))
                    (pos-bol))))
      (kill-ring-save beg end)
      (cond
       ((= real-yanked 1)
        (message "Line yanked to the kill-ring"))
       ((< arg 0)
        (message "The previous %d lines were yanked to the kill-ring" real-yanked))
       ((> arg 0)
        (message "The next %d lines were yanked to the kill-ring" real-yanked))
       (t
        nil)))))




;;;###autoload
(defun bb-insert-newline-above (arg)
  "Insert a new line before the current or, with prefix, ARG number of lines.

With negative prefix calls mirror function `bb-insert-newline-below' passing
ARG."
  (interactive "p")
  (if (< arg 0) (bb-insert-newline-below (- arg)))
  (goto-char (pos-bol))
  (while (> arg 0)
    (insert "\n")
    (forward-line -1)
    (setq arg (1- arg)))
  (indent-according-to-mode))

;;;###autoload
(defun bb-insert-newline-below (arg)
  "Insert a new line after the current or, with prefix, ARG number of lines.

With negative prefix calls mirror function `bb-insert-newline-above' passing
ARG."
  (interactive "p")
  (if (< arg 0) (bb-insert-newline-above (- arg)))
  (goto-char (pos-eol))
  (while (> arg 0)
    (insert "\n")
    (setq arg (1- arg)))
  (indent-according-to-mode))




(defun bb-duplicate-line-above-dwim (arg)
  "Duplicate line or region ARG times below point/region."
  (interactive "p")
  (bb--duplicate-line (abs arg)
                      (if (> arg 0) -1 1)))


(defun bb-duplicate-line-below-dwim (arg)
  "Duplicate line or region ARGth times above point/region.
If ARG is nil, do it one time."
  (interactive "p")
  (bb--duplicate-line (abs arg)
                      (if (> arg 0) 1 -1)))


(defun bb--duplicate-line (count dir)
  "Duplicate line or region COUNT times towards DIR according to prefix.
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
   "Transpose words ARG times from point or between point and mark."
  (interactive "p")
  (transpose-words (if (region-active-p) 0 arg)))


(defun bb--move-line (count)
  "Move line or region COUNT times in direction according to prefix.
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
                     ;; 2023-10-24  TODO => Put there the above condition of not having EOB in the last line. Check 2nd to last line without eob

                     ;; Line
                     ((= (line-number-at-pos (point)) bound)
                      "")
                     ;; No errors
                     (t nil))))
    (user-error (format "WARNING: %salready at the %s" scope string-bound))))


(defun bb-move-line-above-dwim (arg)
  "Move line or region ARG times up.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (bb--move-line-user-error (point-min))
    (bb--move-line (- arg))))


(defun bb-move-line-below-dwim (arg)
  "Move line or region ARG times down.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (bb--move-line-user-error (point-max))
    (bb--move-line arg)))



(provide 'bb-motion)
;;; bb-motion.el ends here
