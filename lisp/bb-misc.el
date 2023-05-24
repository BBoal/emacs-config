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



;;;###autoload
(defun bb-change-inside-char-pairs(char count)
  "."
  (interactive "cChange pair: \np")
  (let* ((count-pairs (cond
                      ((> count 0) (- (* count 2) 1))
                      ((< count 0) (+ (* count 2) 1))
                      (t 0)))
         (search-dir (bb--find-occurrence char count-pairs))
         (first-char (point)))
    (bb--find-occurrence char search-dir)
    (forward-char (- search-dir))
    (kill-region first-char (point))))



;;;###autoload
(defun bb-change-around-char-pairs(char count)
  "."
  (interactive "cChange pair: \np")
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
