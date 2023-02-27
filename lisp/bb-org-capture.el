(setq org-capture-templates
   '(("p" "Personal")
     ("n" "NAV")
     ("f" "Family")
     ("r" "Repairs")
     ("w" "Work")
     ("pj" "Journal entry" entry
      (file+olp+datetree "journal.org")
      "* %U - %^{Activity}")
     ("ft" "Requests as TODOS" entry
      (file+headline "catch_all.org" "Family")
      (file "templates/tpl-todo"))
     ("pb" "This is a catch_all" entry
      (file+headline "catch_all.org" "Some heading")
      "* TODO")
     ("pt" "Private TODOS")
     ("pta" "This is a catch_all" entry
      (file+headline "catch_all.org" "Some heading")
      "* TODO")
     ))

(provide 'bb-org-capture)
;;; bb-org-capture.el ends here
