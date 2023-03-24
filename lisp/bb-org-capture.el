(setq org-capture-templates
   '(("p" "Personal")
     ("f" "Family")
     ("s" "Special Event" entry
      (file+headline "notes.org" "SPECIAL EVENT")
      (file "templates/special_event")
      :empty-lines 1)
     ("r" "Repairs/Maintenance")
     ("w" "Work")
     ("pj" "Journal entry" entry
      (file+olp+datetree "notes.org")
      (file "templates/p_journal"))
     ("ph" "Code snippet" entry
      (file+headline "notes.org" "CODE --> CAPTURE")
      (file "templates/p_code_snippet"))
     ("pt" "Task with deadline" entry
      (file+headline "notes.org" "DEADLINE --> CAPTURE")
      (file "templates/p_todo_deadline"))
     ("pa" "Acquisitions" entry
      (file+headline "notes.org" "PURCHASES --> CAPTURE")
      (file "templates/p_purchases"))
     ("pb" "Borrowed stuff" entry
      (file+headline "notes.org" "BORROWED --> CAPTURE")
      (file "templates/p_borrowed")
      :empty-lines 1)
     ("ft" "Requests with date" entry
      (file+headline "notes.org" "Family related")
      (file "templates/f_todo"))
     ("rt" "Tools and others" entry
      (file+headline "notes.org" "Maintenance")
      (file "templates/r_tools")
      :empty-lines 1)
     ))

(provide 'bb-org-capture)
;;; bb-org-capture.el ends here
