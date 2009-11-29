;; ERC
(require 'erc)
(require 'erc-match)
(require 'erc-join)
(require 'erc-goodies)
(setq erc-modules (quote(autojoin button completion fill irccontrols
                                   keep-place list match menu
                                   move-to-prompt netsplit networks
                                   noncommands readonly ring
                                   scrolltobottom
                                   stamp spelling track)))

(setq erc-keywords '("mfisher" "spudnuts"))
(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
(setq erc-track-exclude-types (append erc-hide-list
                                      (quote("324" "329" "332" "333"
                                             "353" "477"))))


(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))
(add-hook 'erc-text-matched-hook 'my-notify-erc)

;; note: requires mfisher-freenode-nickserv to be defined, ie in
;; custom.el
(add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
    	     (cond
    	      ((string-match "freenode\\.net" SERVER)
    	       (erc-message "PRIVMSG"
                            (format "NickServ identify %s"
                                    mfisher-freenode-nickserv))))))
    
(setq erc-email-userid "mfisher@csh.rit.edu")
(setq erc-nick "mfisher")
(setq erc-prompt-for-password nil)
(setq erc-system-name "sonnycorleone")
(setq erc-user-full-name "Mike Fisher")
(setq erc-user-mode "+iw")
(setq erc-max-buffer-size 20000)
(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (growl (format "%s in %s"
                 ;; Username of sender
                 (car (split-string nickuserhost "!"))
                 ;; Channel
                 (or (erc-default-target) "#unknown"))
         ;; Remove duplicate spaces
         (replace-regexp-in-string " +" " " message)))
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(setq erc-input-line-position -2)

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs")
        ("perl.org" "#catalyst")))
;; (erc :server "chat.us.freenode.net" :port 6667 :nick "mfisher")
