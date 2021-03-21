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

(menu-bar-mode -1)
(server-mode)

;; Stuff for LiveJournal 
(setq load-path (cons "/home/mfisher/emacs/ljupdate/lisp" load-path)) 
;(setq coding-system-p "utf-8") 
(require 'ljupdate) 
(setq lj-default-profile (lj-defprofile 'livejournal "mfisher")) 
(add-to-list 'auto-mode-alist '("\\.lj$" . lj-update-mode))

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xw" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
