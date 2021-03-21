;; Stuff for Gnus (trimmed down)  (eval-buffer)
;; http://www.stat.ufl.edu/~presnell/Links/gnus-brief.org
(require 'gnus)

(setq gnus-check-new-newsgroups 'ask-server)
(setq gnus-save-killed-list t)
(setq gnus-suppress-duplicates t)
(setq gnus-save-duplicate-list t)
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)
(setq nnmail-expiry-wait 'immediate) ;; delete messages immediately
;; (setq gnus-select-method
;;       '(nntp "news.csh.rit.edu"
;;              (nntp-open-connection-function nntp-open-ssl-stream)
;;              (nntp-port-number "nntps") ;see /etc/services
;;              (nntp-address "news.csh.rit.edu")))
(setq gnus-select-method '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnimap "localhost"
		(nnimap-address "localhost")
		(nnimap-server-port 993)
		(nnir-search-engine imap)
		(nnimap-list-pattern ("INBOX."))
		(remove-prefix "INBOX.")
		(nnimap-stream ssl))))

(setq gnus-message-archive-group "nnimap+localhost:INBOX.Sent"
      gnus-message-archive-method
      '(nnimap "localhost"
	       (nnimap-address "localhost")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(gnus-demon-add-handler 'gnus-demon-add-scanmail 5 nil)
(gnus-demon-init)
(setq gnus-use-demon t)

(setq mail-source-delete-incoming t)
(setq mail-source-delete-old-incoming-confirm nil)

;; SMTP configuration
(setq smtpmail-smtp-server "mail.csh.rit.edu")
;; If you use the default mail user agent.
(setq send-mail-function 'smtpmail-send-it)
;; If you use Message or Gnus.
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("mail.csh.rit.edu" 587 "mfisher" nil)))
;; (setq smtpmail-debug-info t
;;       smtpmail-debug-verb t)
(setq message-mail-alias-type 'abbrev)

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; redundant but helpful when this file is singly-loaded
(setq user-mail-address "mfisher@csh.rit.edu")
(setq display-time-24hr-format t)
(display-time-mode t)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-trailing-newline t)
(setq sentence-end-double-space nil) ;; improve paragraph flowing

;;; bbdb
(setq bbdb-default-area-code 585
      bbdb-default-country "US")
(require 'bbdb-autoloads)
(require 'bbdb)
(bbdb-initialize 'gnus 'message 'sc)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-insinuate-message)
(autoload 'bbdb/gnus-lines-and-from "bbdb-gnus")
(setq gnus-optional-headers 'bbdb/gnus-lines-and-from)
(setq bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
      bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
      ;; NOTE: there can be only one entry per header (such as To, From)
      ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
      
      '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
;; (setq gnus-permanently-visible-groups "INBOX$")

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

;; Try to make editing emails nicer
(add-hook 'message-mode-hook
	  (lambda ()
	    (auto-fill-mode -1)
	    (turn-on-visual-line-mode)))
;; might also want:
;; (define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)

;; Shortcut to mail Maria
(defun maf-mail-maria ()
  "Mail Maria"
  (interactive)
  (gnus-group-mail)
  (message-goto-to)
  (insert "mfisherdoc")
  (message-tab)
  (message-goto-subject))
(define-key gnus-group-mode-map (kbd "C-c m") 'maf-mail-maria)

(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(setq gnus-group-sort-function 'gnus-group-sort-by-score)

;;; (info "(gnus) Group Timestamp")
(setq gnus-permanently-visible-groups ":INBOX\$")
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,8d\n")
