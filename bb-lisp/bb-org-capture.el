;;; bb-org-capture.el --- Personal Org Capture Templates -*- lexical-binding: t -*-

;; Copyright (c) 2023  Bruno Boal <bruno.boal@tutanota.com>
;; Author: Bruno Boal <bruno.boal@tutanota.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:


(defvar org-capture-templates
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
