;;; bb-mail.el --- Mail setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Mainly notmuch package and its dependencies

;;; Code:

;;;; `smtpmail'
(use-package smtpmail
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
  :load-path "/usr/share/emacs/site-lisp/"
  :config
  (setq notmuch-identities '("Bruno Boal <egomet@bboal.com>")
        notmuch-fcc-dirs   '(("egomet@bboal.com" . "mailbox/Sent"))
        notmuch-show-logo nil))


;;;; `notmuch-indicator'
(use-package notmuch-indicator
  :after (notmuch smtpmail)
  :vc (:url "https://git.sr.ht/~protesilaos/notmuch-indicator"
       :rev :newest)
  :config
  (setq notmuch-indicator-args
        '((:terms "tag:unread" :label " U:" :face warning)))
  (setq notmuch-indicator-refresh-count 300)
  (setq notmuch-indicator-hide-empty-counters t)
  (setq notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))
  (notmuch-indicator-mode 1))


(provide 'bb-mail)
;;; bb-mail.el ends here
