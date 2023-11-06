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


(defconst wrapping-symbols
  '(?\( ?\) ?\{ ?\} ?\[ ?\] ?\< ?\>
    ?\! ?\" ?\# ?\$ ?\% ?\& ?\+ ?\`
    ?\, ?\- ?\. ?\/ ?\' ?\* ?\- ?\=
    ?\: ?\; ?\= ?\? ?\\ ?\@ ?\^ ?\_
    ?\| ?\~ ?\')
    "List of most common used wrapping symbols. Space and Backspace are used for
specific options in user defined functions")

(defconst symbol-pairs
  '((?\( . ?\))
    (?\{ . ?\})
    (?\[ . ?\])
    (?\` . ?\')
    (?\< . ?\>))
  "Most used open and closing symbol pairs")




;;;###autoload
(defun bb--find-occurrence(char count)
  "This is a helper function usually called by:
`bb-find-occurrence-direction-kill-sexp'
`bb-find-occurrence-direction-kill-around-sexp'
`bb-zap-from-char-to-end'

Searches for CHAR, COUNTth times. Leaves point in occurrence
or produces an error. To achieve this effect, the `search-forward'
function is used.

Returns DIR, with possible values of 1, when the search is meant to be
forward; -1, when backward, or 0, when the occurrence matches neither
the values of `pos-bol' or `pos-eol'."

  (let ((dir 1)
        (bounds (pos-eol)))
    (when (< count 0)
      (setq dir -1
            bounds (pos-bol)))
    (search-forward (format "%c" char) bounds nil count)
    (when (= (point) bounds)
      (setq dir 0))
    dir))



;;;###autoload
(defun bb--find-2nd-delimiter(char dir)
  "This is a helper function called by `bb-change-inside-char-pairs' to get
the second pair of a delimiter of a given CHAR.

The delimiter pairs are in an associative list. If CHAR is not present in the
list, it is assumed that CHAR is the closing delimiter for CHAR.

DIR will let you work on both directions with open<->closing as well as
closing<->open delimiters."

  (let ((closing-delim (if (>= dir 0)
                           (cdr (assq char symbol-pairs))
                         (car (rassq char symbol-pairs)))))
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
    (delete-region first-char (point))))



;;;###autoload
(defun bb-change-around-char-pairs(char count)
  "Similiar to `bb-change-inside-char-pairs' but also kills surrounding
delimiters."
  (interactive "cDelete around pair: \np")
  (bb-change-inside-char-pairs char count)
  (delete-forward-char 1)
  (delete-forward-char -1))




;;;; Wrap region functions
(defun bb-wrap-get-exp-fun()
  "Returns function responsible for region expansion of bb-t"
  (cond
   ((fboundp 'bb-expreg-try-expand-symbol)
    'bb-expreg-try-expand-symbol)
   ((fboundp 'expreg-expand)
    'expreg-expand)
   (t
    'mark-sexp)))


(defun bb-wrap-check-pairs(beg end)
  ""
  (let ((open-pair (char-before beg))
        (close-pair (char-after end)))
    (or (and (eq open-pair close-pair)
             (memq open-pair wrapping-symbols))
        (eq open-pair (car (rassq close-pair symbol-pairs))))))


(defun bb-wrap-clear (beg end)
  ""
  (save-excursion
    (cl-mapc (lambda (pos dir)
               (goto-char pos)
               (delete-char dir))
             `(,end ,beg) '(1 -1))))


(defun bb-wrap-region (beg end)
  "Wraps the selected region or, the region resulting of `expreg-expand' call, if
available. Otherwise signal an error."
  (interactive
   (let ((exp-fun (bb-wrap-get-exp-fun)))
     (cond
      ((use-region-p)
       (unless (bb-wrap-check-pairs (region-beginning) (region-end))
         (user-error "ERR: Selected region doesn't have defined delimiters")))
      (t
       (funcall exp-fun)
       (while (not (bb-wrap-check-pairs (region-beginning) (region-end)))
         (funcall exp-fun))))
     (list (region-beginning) (region-end))))

  (let ((char (read-char-exclusive
               "Surrounding options (Backspace[substitution], Spacebar[clear], Delimiter[wrap]): ")))
    (cond
     ((eq char 32) ;; spacebar (clear)
      (bb-wrap-clear beg end))
     ((eq char 127) ;; backspace (substitution)
      (bb-wrap-clear beg end)
      (bb-wrap-around (region-beginning) (region-end) nil))
     ((eq char 27)
      (keyboard-quit))
     (t
      (bb-wrap-around beg end char)))))


(defun bb-wrap-around (beg end char)
  ""
  (while (not (memq char wrapping-symbols))
    (when char (princ "Char not a member of `wrapping-symbols' list. Try again.")
          (sit-for 1.2 t))
    (setq char (read-char-exclusive "Wrap with: ")))
  (let ((final-pos (point))
        (close-pair (cdr (assq char symbol-pairs)))
        (open-pair (car (rassq char symbol-pairs))))
    (cond
     ;; assume char is the open pair
     (close-pair
      (setq open-pair char
            final-pos (1+ beg)))
     ;; assume char is the closing pair
     (open-pair
      (setq close-pair char
            final-pos (+ 2 end)))
     ;; not in `symbol-pairs' so equal open-close chars
     (t
      (setq open-pair char
            close-pair char
            final-pos (+ 2 end))))
    (cl-mapc (lambda (pos pair)
               (goto-char pos)
               (insert pair))
             `(,end ,beg) `(,close-pair ,open-pair))
    (goto-char final-pos)
    (message "Wrapped region with  %c ... %c" open-pair close-pair)))




;;;###autoload
(defun bb-zap-from-char-to-end(char count)
  "A search is conducted for a specific CHAR, COUNTth times using the
`bb--find-occurrence' helper function.

DIR is the return value of `bb--find-occurrence'.

If DIR is 0, then the occurrence matched either `pos-eol' or
`pos-bol'. On both cases, the function does not interfere
with different lines, meaning that the kill is restricted to the same line
and nothing gets evaluated.

If DIR is 1, the kill is made from occurrence to `pos-eol'.
Analogously, if DIR is -1, the kill goes to `pos-bol'."

  (interactive "cZap from char: \np")
  (let ((dir (bb--find-occurrence char count)))
    (unless (= dir 0)
      (delete-region (point) (if (= dir 1)
                               (pos-eol)
                             (pos-bol))))))



(provide 'bb-misc)
;;; bb-misc.el ends here
