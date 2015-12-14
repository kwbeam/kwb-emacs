;;; kwb-gnus --- kwbeam gnus setup
;;; Commentary:

;; Setup Emacs for email and news

;;; Code:

(setq user-mail-address	"kwbeam@freedommail.ch"
      user-full-name "Kevin Beam")

(defvar gnus-select-method)
(setq gnus-select-method '(nnimap "freedommail.ch"
                                  (nnimap-address "imap.freedommail.ch")
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)))

(defvar message-send-email-function)
(defvar message-send-mail-function)
(defvar smtpmail-smtp-server)
(defvar smtpmail-smtp-service)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.freedommail.ch"
      smtpmail-smtp-service 587)

(defvar mml2015-signers)
(setq mml2015-signers '("00EBB1B5"))

(defvar mml2015-encrypt-to-self)
(setq mml2015-encrypt-to-self t)

(provide 'kwb-gnus)

;;; kwb-gnus.el ends here
