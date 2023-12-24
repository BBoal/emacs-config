;;; bb-mail.el --- Mail setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Mainly notmuch package and its dependencies

;;; Code:

(require 'message)


(defun bb-sets-union-apairs (base-set addon-set &rest more-addons)
  "Combines all apairs from BASE-SET and ADDON-SET, updating duplicate entries.
MORE-ADDONS allows for additional sets of elements to be added."
  (let ((minimal-set (if (and base-set (sequencep base-set))
                         (copy-tree base-set)))
        final-set)
    (if more-addons
        (setq addon-set (append addon-set more-addons)))
    (mapc (lambda (addon)
            (let ((key (assoc (car-safe addon) minimal-set)))
              (if key (setq minimal-set (delete key minimal-set)))
              (push addon final-set)))
          addon-set)
    (nconc minimal-set (nreverse final-set))))


(defvar message-cite-style-minimal
  '((message-cite-function 'message-cite-original)
    (message-citation-line-function 'message-insert-formatted-citation-line)
    (message-yank-prefix "> ")
    (message-yank-cited-prefix ">") (message-yank-empty-prefix ">")
    (message-citation-line-format "On %F, %T(%Z), %f wrote:")
    (message-signature-file "~/.mail/.signature"))
  "Message citation style used as a minimal blueprint for other styles.")

(defvar message-cite-style-default-addon
  '((message-cite-reply-position 'traditional))
    "Message citation style to complement minimal style for standard messages.")

(defvar message-cite-style-default
  (bb-sets-union-apairs message-cite-style-minimal message-cite-style-default-addon))


(defun replying-to-newsgroups-p ()
  "Predicate function that is non-nil for emails without the \\='newsletter\\=' tag."
  (member-ignore-case "newsletter" (notmuch-show-get-tags)))



;;;; `smtpmail'
(use-package smtpmail
  :demand t
  :init
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-queue-mail nil
        smtpmail-debug-info t
        smtpmail-debug-verb t))




;;;; `sendmail'
(use-package sendmail
  :init
  ;; Ensuring that `message-send-mail-hook' is run with `send-mail-function'
  (setq send-mail-function 'message-use-send-mail-function))




;;;; `notmuch'
(use-package notmuch
  :demand t
  :functions (notmuch-show-get-tags message-recipients)
  :load-path "/usr/share/emacs/site-lisp/"
  :hook
  (message-send . message-sign-encrypt-if-all-keys-available)
  (message-send . notmuch-mua-empty-subject-check)
  (message-send . notmuch-mua-attachment-check)

  :init
  (defun message-recipients ()
    "Return a list of all recipients in the message, looking at TO, CC and BCC.

Each recipient is in the format of `mail-extract-address-components'."
    (mapcan (lambda (header)
              (let ((header-value (message-fetch-field header)))
                (and header-value
                     (mail-extract-address-components header-value t))))
            '("To" "Cc" "Bcc")))

  (defun message-all-epg-keys-available-p ()
    "Return non-nil if the pgp keyring has a public key for each recipient."
    (require 'epa)
    (let ((context (epg-make-context epa-protocol)))
      (catch 'break
        (dolist (recipient (message-recipients))
          (let ((recipient-email (cadr recipient)))
            (when (and recipient-email (not (epg-list-keys context recipient-email)))
              (throw 'break nil))))
        t)))

  (defun message-sign-encrypt-if-all-keys-available ()
    "Add MML tag to encrypt message when there is a key for each recipient.

Consider adding this function to `message-send-hook' to systematically send
encrypted emails when possible."
    (when (message-all-epg-keys-available-p)
      (mml-secure-message-sign-encrypt)))

  (defun notmuch-mua-empty-subject-check ()
    "Request confirmation before sending a message with empty subject"
    (unless (or (message-field-value "Subject")
                (y-or-n-p "Subject is empty, send anyway? "))
      (user-error "Sending message cancelled: empty subject")))

  :config
  (setq-default notmuch-mua-attachment-regexp
                "\\b\\(attach\\|attachment\\|attached\\|anexo\\|anexado\\)\\b")
  (setq notmuch-identities '("Bruno Boal <egomet@bboal.com>")
        notmuch-fcc-dirs   '(("egomet@bboal.com" . "mailbox/Sent"))
        notmuch-show-process-crypto t
        notmuch-show-logo nil))




;;;; `notmuch-indicator'
(use-package notmuch-indicator
  :demand t
  :functions notmuch-indicator-mode
  :preface
  (setq notmuch-indicator-args
        '((:terms "tag:unread" :label " U:" :face success)
          (:terms "tag:_BB_" :label " BB:" :face success))
        notmuch-indicator-refresh-count 120
        notmuch-indicator-hide-empty-counters t
        notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer)
        notmuch-indicator-add-to-mode-line-misc-info nil)
  :config
  (notmuch-indicator-mode 1))


(provide 'bb-mail)
;;; bb-mail.el ends here
