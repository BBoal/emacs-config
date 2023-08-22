;;; bb-c-cpp.el --- C and C++ setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; GDB
(setq gdb-many-windows t
      gdb-show-main    t
      gdb-debug-log    nil)


(defun bb-setup-cc-project-root()
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
(defun bb-set-compile-command()
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


(defun bb-func-bundle-cc-modes()
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
  (mapc (lambda (hook)
          (add-hook hook #'bb-func-bundle-cc-modes))
        '(c++-mode-hook c-mode-hook)))


;;;;;; `c-ts-mode' and `c++-ts-mode'
(use-package c-ts-mode
  :config
  (mapc (lambda (hook)
          (add-hook hook #'bb-func-bundle-cc-modes))
        '(c++-ts-mode-hook c-ts-mode-hook)))


;;;;;; `cpp-auto-include'
(use-package cpp-auto-include)


;;;;;; `clang-capf'
(use-package clang-capf
  :after cape
  :hook (c-ts-mode c-mode c++-ts-mode c++-mode objc-mode)
  :config
  (add-to-list 'completion-at-point-functions #'clang-capf))


;;;; `disaster'
(use-package disaster
:bind (("C-c d" . disaster)))


(provide 'bb-c-cpp)
;;; bb-c-cpp.el ends here
