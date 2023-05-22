;;; bb-misc.el --- Miscellaneous functions and commands -*- lexical-binding: t -*-

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


(defun bb--find-occurrence(char count)
  "Searches for CHAR, COUNTth times. Leaves point in occurrence
 or produces an error.

Under the hood, the `search-forward' function is used."
  (let ((bounds
         (if (>= count 0) (line-end-position) (line-beginning-position))))
    (search-forward (format "%c" char) bounds nil count)
    bounds))


(defun bb--kill-sexp-in-direction(dir)
  "This is a helper function. Usually called by
 `bb-find-occurrences-direction-kill-sexp' or
 `bb-find-occurrences-direction-kill-around-sexp'.

If DIR is a positive number or 0, the kill direction is set forward and
bounded to the end of the line.
If DIR is negative, the direction becomes backward and the kill is bounded
to the beginning of the line.

On both cases, the function does not interfere with different lines, meaning
that the kill is restricted to the same line used during the funcall."
   (let ((direction 1)
             (bounds (line-end-position)))
         (when (< dir 0)
           (setq direction -1
                 bounds (line-beginning-position)))
         (unless (eq (point) bounds)
           (kill-sexp direction))))

;;;###autoload
(defun bb-find-occurrence-direction-kill-sexp(char count)
  "A search is conducted for a specific CHAR, COUNTth times.
If successful, then a sexp is killed in the direction defined by the sign of
COUNT. The kill is restricted to the same line.

See `bb--find-occurrence' and `bb--kill-sexp-in-direction'."
  (interactive "cKill sexp next to char: \np")
  (bb--find-occurrence char count)
  (bb--kill-sexp-in-direction count))

;;;###autoload
(defun bb-find-occurrence-direction-kill-around-sexp(char count)
    "A search is conducted for a specific CHAR, COUNTth times.
If successful, the area to kill is enlarged by means or moving the point
away from the desired kill area. The kill direction is determined by the sign of
the COUNT parameter and the kill command is restricted to the same line.

See `bb--find-occurrence' and `bb--kill-sexp-in-direction'."
  (interactive "cKill sexp around char: \np")
  (bb--find-occurrence char count)
  (if (>= count 0) (forward-char -1) (forward-char 1))
  (bb--kill-sexp-in-direction count))


;;;###autoload
(defun bb-zap-from-char-to-end(char count)
  "If COUNT is positive, tries to find the COUNT'th occurrence of CHAR
searching forward from point. Kills from occurrence to the end of line.

If COUNT is negative, tries to find the COUNT'th occurrence of CHAR
searching backward from point. Kills from occurrence to the beginning of
the line."
  (interactive "cZap from char: \np")
  (bb--find-occurrence char count)
  (kill-region (point) (if (>= count 0)
                           (line-end-position)
                         (line-beginning-position))))


(provide 'bb-misc)
;;; bb-misc.el ends here
