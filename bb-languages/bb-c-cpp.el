;;; bb-c-cpp.el --- C and C++ setup -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:


;;;;;; `cc-mode'
(use-package cc-mode
  :hook (((c++-ts-mode c++-mode c-ts-mode c-mode) .
          (lambda()
            (add-hook 'before-save-hook #'bb-eglot-arrange-file :depth :local)
            (bb-programming-hooks)
            (eglot-ensure)
            (bb-set-compile-command)
            (bb-setup-cc-project-root))))
  :bind (("C-c C-c" . compile))

  :config
  (defun bb-setup-cc-project-root()
  (let ((mode major-mode))
    (cond
     ((or (eq mode c-ts-mode)
          (eq mode c-mode))
      (prot-find-project-root mode "Makefile"))
     ((or (eq mode c++-ts-mode)
          (eq mode c++-mode))
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
  (setq gdb-many-windows t
        gdb-show-main    t
        gdb-debug-log    nil))


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
