;;; bb-mail.el --- Mail setup -*- lexical-binding: t -*-

;; Copyright (c) 2023    Bruno Boal <egomet@bboal.com>
;; Author: Bruno Boal <egomet@bboal.com>
;; URL: https://git.sr.ht/~bboal/emacs-config
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Mainly notmuch package and its dependencies

;;; Code:

(require 'message)


(defun bb-sets-union-apairs (base-set addon-set &optional ordered-elems &rest more-elems)
  "Combines all apairs from BASE-SET and ADDON-SET, updating duplicate entries.
If the order of sets is required, optional ORDERED-ELEMS should be non-nil.
MORE-ELEMS allows for additional elements to be added."
  (unless (and (sequencep base-set) (sequencep addon-set))
    (user-error "Both set arguments must be either a list or an array"))
  (let ((minimal-set (if ordered-elems (copy-tree base-set) base-set))
        final-set)
    (if more-elems
        (setq addon-set (append addon-set more-elems)))
    (mapc (lambda (addon)
            (let ((key (assoc (car-safe addon) minimal-set)))
              (if key (setq minimal-set (delete key minimal-set)))
              (push addon final-set)))
          addon-set)
    (if ordered-elems
        (nconc minimal-set (nreverse final-set))
      (nconc final-set minimal-set))))


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
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-queue-mail nil
        smtpmail-debug-info t
        smtpmail-debug-verb t))




;;;; `sendmail'
(use-package sendmail
  :config
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'message-use-send-mail-function))




;;;; `notmuch'
(use-package notmuch
  :demand t
  :functions (notmuch-show-get-tags message-recipients)
  :load-path "/usr/share/emacs/site-lisp/"
  :hook ((message-send . message-sign-encrypt-if-all-keys-available)
         (message-send . notmuch-mua-empty-subject-check)
         (message-send . notmuch-mua-attachment-check))

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
            (unless (and recipient-email (epg-list-keys context recipient-email))
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
  :init
  (setopt notmuch-indicator-args
          '((:terms "tag:unread" :label " U:" :face warning)
            (:terms "tag:_BB_" :label " BB:" :face success))
          notmuch-indicator-refresh-count 120
          notmuch-indicator-hide-empty-counters t
          notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer)
          notmuch-indicator-add-to-mode-line-misc-info nil)
  :config
  (notmuch-indicator-mode 1))



(provide 'bb-mail)
;;; bb-mail.el ends here
