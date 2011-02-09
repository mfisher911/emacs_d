;; Stuff for Gnus
(require 'gnus)

(setq gnus-check-new-newsgroups 'ask-server)
(setq gnus-save-killed-list t)
(setq gnus-suppress-duplicates t)
(setq gnus-save-duplicate-list t)
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)
(setq gnus-select-method
      '(nntp "news.csh.rit.edu"
             (nntp-open-connection-function nntp-open-ssl-stream)
             (nntp-port-number "nntps") ;see /etc/services
             (nntp-address "news.csh.rit.edu")))

;; redundant but helpful when this file is singly-loaded
(menu-bar-mode -1)
