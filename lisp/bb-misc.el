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
  "This is a helper function usually called by:
`bb-find-occurrence-direction-kill-sexp'
`bb-find-occurrence-direction-kill-around-sexp'
`bb-zap-from-char-to-end'

Searches for CHAR, COUNTth times. Leaves point in occurrence
or produces an error. To achieve this effect, the `search-forward'
function is used.

Returns DIR, with possible values of 1, when the search is meant to be
forward; -1, when backward, or 0, when the occurrence matches either
the values of `line-end-position' or `line-beginning-position'."

  (let ((dir 1)
        (bounds (line-end-position)))
    (when (< count 0)
      (setq dir -1
            bounds (line-beginning-position)))
    (search-forward (format "%c" char) bounds nil count)
    (when (= (point) bounds)
      (setq dir 0))
    dir))



(defun bb--kill-sexp-in-direction(dir)
  "This is a helper function. Usually called by
`bb-find-occurrences-direction-kill-sexp' or
`bb-find-occurrences-direction-kill-around-sexp'.

DIR is the return value of `bb--find-occurrence'.

If DIR is 0, then the occurrence matched either `line-end-position' or
`line-beginning-position'. On both cases, the function does not interfere
with different lines, meaning that the kill is restricted to the same line
and nothings gets evaluated.

If DIR is 1, the kill direction is set forward and the next sexp is killed.
Analogously, if DIR is -1, the previous sexp is killed."

  (unless (= dir 0)
    (kill-sexp dir)))



;;;###autoload
(defun bb-find-occurrence-direction-kill-sexp(char count)
  "A search is conducted for a specific CHAR, COUNTth times using the
`bb--find-occurrence' helper function.

If CHAR is found, the return value is then passed to `bb-kill-sexp-in-direction'
for directional processing and possible `kill-sexp' usage."

  (interactive "cKill sexp next to char: \np")
  (bb--kill-sexp-in-direction (bb--find-occurrence char count)))



;;;###autoload
(defun bb-find-occurrence-direction-kill-around-sexp(char count)
  "A search is conducted for a specific CHAR, COUNTth times using the
`bb--find-occurrence' helper function.

If CHAR is found, the area to kill is enlarged by means or moving the point
away from the desired kill area. The possible kill direction is determined
by the return value of `bb--find-occurrence'. Processing is made with the
`bb--kill-sexp-in-direction' function."

  (interactive "cKill sexp around char: \np")
  (let ((kill-dir (bb--find-occurrence char count)))
    (forward-char (- kill-dir))
    (bb--kill-sexp-in-direction kill-dir)))



(defun bb--find-2nd-delimiter(char dir)
  "This is a helper function called by `bb-change-inside-char-pairs' to get
the second pair of a delimiter of a given CHAR.

The delimiter pairs are in an associative list. If CHAR is not present in the
list, it is assumed that CHAR is the closing delimiter for CHAR.

DIR will let you work on both directions with open<->closing as well as
closing<->open delimiters."

  (let* ((pairs '((?\( . ?\))
                  (?\{ . ?\})
                  (?\[ . ?\])
                  (?\` . ?\')
                  (?\< . ?\>)))
         (closing-delim (if (>= dir 0)
                            (cdr (assq char pairs))
                          (car (rassq char pairs)))))
    (bb--find-occurrence (or closing-delim char)
                           dir)))



;;;###autoload
(defun bb-change-inside-char-pairs(char count)
  "The main goal of this function is to kill the \"inside\" of a CHAR delimiter
group.
Helper function `bb--find-occurrence' and `bb--find-2nd-delimiter' are used for
that effect. The first will get point to CHAR and returns the direction of
the search. The latter function gets point to the closing delimiter of CHAR
"
  (interactive "cChange pair: \np")

  (let ((pair-num (cond
                   ((= count 0) 1)
                   (t (abs count))))
        (abs-count (if (>= count 0) 1 -1)))

    (while (> pair-num 1)
      (bb--find-2nd-delimiter char (bb--find-occurrence char abs-count))
      (setq pair-num (1- pair-num)))

    (bb--find-occurrence char abs-count)
    (setq first-char (point))
    (bb--find-2nd-delimiter char abs-count)
    (forward-char (- abs-count))
    (kill-region first-char (point))))



;;;###autoload
(defun bb-change-around-char-pairs(char count)
  "Similiar to `bb-change-inside-char-pairs' but also kills surrounding
delimiters."
  (interactive "cDelete around pair: \np")
  (bb-change-inside-char-pairs char count)
  (delete-forward-char 1)
  (delete-forward-char -1))



;;;###autoload
(defun bb-zap-from-char-to-end(char count)
  "A search is conducted for a specific CHAR, COUNTth times using the
`bb--find-occurrence' helper function.

DIR is the return value of `bb--find-occurrence'.

If DIR is 0, then the occurrence matched either `line-end-position' or
`line-beginning-position'. On both cases, the function does not interfere
with different lines, meaning that the kill is restricted to the same line
and nothings gets evaluated.

If DIR is 1, the kill is made from occurrence to `line-end-position'.
Analogously, if DIR is -1, the kill goes to `line-beginning-position'."

  (interactive "cZap from char: \np")
  (let ((dir (bb--find-occurrence char count)))
    (unless (= dir 0)
      (kill-region (point) (if (= dir 1)
                               (line-end-position)
                             (line-beginning-position))))))



(provide 'bb-misc)
;;; bb-misc.el ends here
