;;; bb-mail.el --- Mail setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Mainly notmuch package and its dependencies

;;; Code:

;;;; `smtpmail'
(use-package smtpmail
  :defer 1
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-queue-mail nil))


;;;; `sendmail'
(use-package sendmail
  :defer 1
  :config
  (setq send-mail-function 'smtpmail-send-it))


;;;; `notmuch'
(use-package notmuch
  :defer 1
  :load-path "/usr/share/emacs/site-lisp/"
  :config
  (setq notmuch-identities '("Bruno Boal <egomet@bboal.com>")
        notmuch-fcc-dirs   '(("egomet@bboal.com" . "mailbox/Sent"))
        notmuch-show-logo nil))


(provide 'bb-mail)
;;; bb-mail.el ends here
