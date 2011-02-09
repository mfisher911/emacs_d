;; Stuff for GNUS
(require 'gnus)

;; (setq gnus-check-new-newsgroups t)
;; (setq gnus-save-killed-list t)
(setq gnus-select-method
      '(nntp "news.csh.rit.edu"
             (nntp-open-connection-function nntp-open-ssl-stream)
             (nntp-authinfo-file "~/.authinfo")
             (nntp-port-number "nntps") ;see /etc/services
             (nntp-address "news.csh.rit.edu")))
