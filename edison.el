;; Stuff for TNT 
(setq load-path (cons "/home/mfisher/tnt" load-path)) 
(load "tnt") 
(setq tnt-default-username "mfuXup") 
;(setq tnt-buggy-idle nil) 
(setq tnt-use-idle-timer nil) 
(setq tnt-email-to-pipe-to nil) 
(setq tnt-email-binary "/usr/bin/mail") 
(setq tnt-use-timestamps t) 
(setq tnt-beep-on-incoming-message 'audible) 
(setq tnt-beep-on-visible-incoming-message 'audible) 
(setq tnt-beep-on-chat-invitation 'audible) 
(setq tnt-beep-on-chat-message 'audible) 
(setq tnt-beep-on-visible-chat-message 'audible) 
(set-face-foreground 'tnt-my-name-face "red") 

;; Stuff for LiveJournal 
(setq load-path (cons "/home/mfisher/emacs/ljupdate/lisp" load-path)) 
;(setq coding-system-p "utf-8") 
(require 'ljupdate) 
(setq lj-default-profile (lj-defprofile 'livejournal "mfisher")) 
(add-to-list 'auto-mode-alist '("\\.lj$" . lj-update-mode))

;; Include version control -- needed for xemacs >= 21 
(require 'vc-hooks) 

