;; (erc :server "chat.us.freenode.net" :port 6667 :nick "mfisher")
;; ERC
(require 'erc)
(require 'erc-join)
(require 'erc-goodies)

(use-package erc-hl-nicks
  :ensure t
  :init (erc-hl-nicks-mode))

(setq erc-keywords '("mfisher" "spudnuts"))
(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
(setq erc-track-exclude-types (append erc-hide-list
                                     (quote("324" "329" "332" "333"
                                            "353" "477"))))
;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;;                                 "324" "329" "332" "333" "353" "477"))
(erc-track-mode 1)
(erc-truncate-mode 1)

(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))
;; (add-hook 'erc-text-matched-hook 'my-notify-erc)

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
;; (setq erc-nick "mfisher")
(setq erc-prompt-for-password nil)
(setq erc-user-full-name "Mike Fisher")
(setq erc-user-mode "+iw")
(setq erc-max-buffer-size 20000)
;; (defun my-notify-erc (match-type nickuserhost message)
;;   "Notify when a message is received."
;;   (growl (format "%s in %s"
;;                  ;; Username of sender
;;                  (car (split-string nickuserhost "!"))
;;                  ;; Channel
;;                  (or (erc-default-target) "#unknown"))
;;          ;; Remove duplicate spaces
;;          (replace-regexp-in-string " +" " " message)))
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(setq erc-input-line-position -2)

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#org-mode")
        ))

;; http://www.emacswiki.org/emacs/UnwrapLine
(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))
(define-key erc-mode-map (kbd "M-q") 'unwrap-line)

;; http://www.emacswiki.org/emacs/ErcModeline
(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     (format " %S/%S/%S" ops voices members))))

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
