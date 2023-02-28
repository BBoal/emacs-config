(setq org-capture-templates
   '(("p" "Personal")
     ("f" "Family")
     ("s" "Special Event")
     ("r" "Repairs/Maintenance")
     ("w" "Work")
     ("pj" "Journal entry" entry
      (file+olp+datetree "journal.org")
      (file "templates/p_journal"))
     ("pl" "Linux" entry
      (file+headline "personal.org" "LINUX topic CAPTURE")
      (file "templates/p_linux"))
     ("ft" "Requests with date" entry
      (file+headline "family.org" "TODO family CAPTURE")
      (file "templates/f_todo"))
     ("pb" "This is a catch_all" entry
      (file+headline "catch_all.org" "Some catch all heading")
      "* SOMEDAY")
     ("pt" "TODO with deadline" entry
      (file+heading "personal.org" "TODO personal CAPTURE ")
	    (file "templates/p_todo_deadline"))
     ))

(provide 'bb-org-capture)
;;; bb-org-capture.el ends here
