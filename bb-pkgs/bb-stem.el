;;; bb-stem.el --- Sciences Tecnology Enginnering Math -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;;;; `chemtable'
(use-package chemtable
  :defer 3)

;;;; `gnuplot'
(use-package gnuplot
  :defer 3
  :ensure-system-package gnuplot
  :mode ("\\.gp$\\'" . gnuplot-mode))


(provide 'bb-stem)
;;; bb-stem.el ends here
