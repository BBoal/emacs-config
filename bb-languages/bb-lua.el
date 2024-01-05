;;; bb-lua.el --- Lua setup -*- lexical-binding: t -*-

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
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Most of these functions and ideas are either from Protesilaos Stavrou config
;; or made with him during his lessons.  A big thanks to Prot for helping me in
;; this wonderful journey to the depths of EmacsLisp.

;;; Code:


(defun bb--lua-custom-variables ()
  "Syntax goodies for Lua Programming Language."
  (setq-local comment-style 'extra-line
              comment-start "--\[\["
              lua-comment-start comment-start
              comment-end "--]]"
              comment-continue (make-string (length comment-start) ?\ )))




;;;; `lua-mode'
(use-package lua-mode
  :hook ((lua-mode . eglot-ensure)
         (lua-mode . bb--lua-custom-variables)))


(provide 'bb-lua)
;;; bb-lua.el ends here
