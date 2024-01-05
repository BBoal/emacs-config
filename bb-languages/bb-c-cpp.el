;;; bb-c-cpp.el --- C and C++ setup -*- lexical-binding: t -*-

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

(require 'setup-langs)
(declare-function derived-mode-map-name "derived")
(declare-function clang-capf "ext:clang-capf")
(declare-function bb-mode-project-find-function "setup-langs")
(declare-function project-find-mode-root "setup-langs")


;;;; GDB
(use-package gdb-mi
  :config
  (setq gdb-many-windows t
        gdb-show-main    t
        gdb-debug-log    nil))


(defun bb-setup-cc-project-root ()
  "Project root setup."
  (interactive)
  (let ((mode major-mode))
    (keymap-set (symbol-value (derived-mode-map-name mode))
                "C-c C-c" #'compile)
    (cond
     ((or (eq mode 'c-ts-mode)
          (eq mode 'c-mode))
      (prot-find-project-root mode "Makefile"))
     ((or (eq mode 'c++-ts-mode)
          (eq mode 'c++-mode))
      (prot-find-project-root mode "CMakeList.txt")))))

;; Setting compile-command
(defun bb-set-compile-command ()
  "Check major-mode and set compile command accordingly."
  (interactive)
  (let ((compile-cmd ""))
    (cond
     ((or (eq major-mode 'c++-ts-mode) (eq major-mode 'c++-mode))
      (unless (file-exists-p "CMakeLists.txt")
        (setq compile-cmd "g++ -g -std=c++20 -Wall -o ")))
     ((or (eq major-mode 'c-ts-mode) (eq major-mode 'c-mode))
      (unless (file-exists-p "Makefile")
        (setq compile-cmd "gcc -g -O -o "))))
    (setq-local compile-command
                (when-let ((buffer-file-name)
                           (filename-bin (file-name-base buffer-file-name)))
                  (concat compile-cmd
                          filename-bin  " "
                          (file-name-nondirectory buffer-file-name)
                          " && ./" filename-bin)))))


(defun bb-func-bundle-cc-modes ()
  "Setting the active hooks and comment options."
  (interactive)
  (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
  (bb-programming-hooks)
  (eglot-ensure)
  (bb-set-compile-command)
  (bb-setup-cc-project-root)
  (setq-local comment-style 'extra-line
              comment-start "/*"
              comment-end "*/"
              comment-padding 2
              comment-continue (make-string (1+ (length comment-start)) ?\ )))




;;;;;; `cc-mode'
(use-package cc-mode
  :config
  (dolist (mode '(c++-mode-hook c-mode-hook))
    (add-hook mode #'bb-func-bundle-cc-modes)))




;;;;;; `c-ts-mode' and `c++-ts-mode'
(use-package c-ts-mode
  :config
  (dolist (mode '(c++-ts-mode-hook c-ts-mode-hook))
    (add-hook mode #'bb-func-bundle-cc-modes)))




;;;;;; `cpp-auto-include'
(use-package cpp-auto-include)




;;;;;; `clang-capf'
(use-package clang-capf
  :after cape
  :hook
  ((c-ts-mode c-mode c++-ts-mode c++-mode objc-mode) . clang-capf-init)
  :config
  (defun clang-capf-init()
    "Local hook of clang-capf."
    (add-hook 'completion-at-point-functions #'clang-capf nil t)))




;;;; `disaster'
(use-package disaster
  :bind (("C-c d" . disaster)))


(provide 'bb-c-cpp)
;;; bb-c-cpp.el ends here
