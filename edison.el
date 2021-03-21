;; Stuff for TNT 
;; (load "tnt") 
(setq tnt-default-username "mfuXup") 
(setq tnt-use-timestamps t) 
; (set-face-foreground 'tnt-my-name-face "red") 

(menu-bar-mode -1)
(server-mode)

; (add-to-list 'load-path "~/el/color-theme-6.6.0/")
;(require 'color-theme)
;(eval-after-load "color-theme"
;  '(progn
;     (color-theme-initialize)
;     (color-theme-renegade)))

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
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xw" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
;; trying to help refresh the twitter timeline
;; (add-hook 'twitter-timeline-view-mode-hook
;;           (lambda ()
;;             (define-key twitter-timeline-view-mode-map (kbd "g")
;;               'twitter-get-friends-timeline)))
;; ;            (local-unset-key "g")
;; ;            (local-set-key "g" 'twitter-get-friends-timeline)))

;; Font lock for (Al)pine/pico buffers:
;; http://snarfed.org/space/emacs%20font-lock%20faces%20for%20composing%20email
(require 'u-vm-color)

(defun maf-clear-msft-sig ()
  "Clear out dumb Hotmail signatures."
  ;;  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^>\\s-*\\)\\{2,\\}$" nil t) 
    (replace-match "> \n"))
  (goto-char (point-min))
  (while (re-search-forward "^\\(>>\\s-+\\)?>> --\\s-+\\(>> .*\\s-\\)+" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "^>\\s-+> _+\\s-> .+\\s-> .+$" nil t)
    (replace-match "\n"))
  nil)

; ugly heuristic to font-lock pine emails automatically
(add-hook 'find-file-hooks
          (lambda ()
            (if (equal "pico." (substring (buffer-name (current-buffer)) 0 5))
                (progn (maf-clear-msft-sig)
                       (u-vm-color-fontify-buffer)))))

;; these match pine's defaults. see M-x list-colors-display for other options
(set-face-foreground 'u-vm-color-citation-1-face "cyan")
(set-face-foreground 'u-vm-color-citation-2-face "lime green")
(set-face-foreground 'u-vm-color-citation-3-face "blue")
(set-face-foreground 'u-vm-color-citation-4-face "dark slate gray")
(set-face-foreground 'u-vm-color-citation-5-face "dark slate blue")
(set-face-foreground 'u-vm-color-signature-face  "red")

;; Jabber.el -- has to be built from source 
;; autoreconf -i
;; ./configure && make
;; sudo make install
(require 'jabber)
(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)
(setq jabber-chat-header-line-format
      '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
        " " (:eval (jabber-jid-resource jabber-chatting-with)) "\t";
        (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                 (propertize
                  (or
                   (cdr (assoc (get buddy 'show) jabber-presence-strings))
                   (get buddy 'show))
                  'face
                  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                      'jabber-roster-user-online))))
        "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
        (:eval (unless (equal "" *jabber-current-show*)
                 (concat "\t You're " *jabber-current-show*
                         " (" *jabber-current-status* ")")))))
;; Message alert hooks
(define-jabber-alert echo "Show a message in the echo area"
  (lambda (msg)
    (unless (minibuffer-prompt)
      (message "%s" msg))))

;; http://yrk.livejournal.com/271911.html
(eval-after-load "jabber-roster"
  '(defun jabber-fix-status (status)
     "Make status strings more readable"
     (when status
       (when (string-match "\n+$" status)
         (setq status (replace-match "" t t status)))
       (when jabber-remove-newlines
         (while (string-match "\n" status)
           (setq status (replace-match " " t t status))))
       (if (> (length status) 32)
           (concat (substring status 0 29) " ...")
         status))))

;; try to set the window title
;; http://www.splode.com/~friedman/software/emacs-lisp/src/xterm-title.el
(when (and
       (not window-system)
       (or
        (string= (getenv "TERM") "dumb")
        (string-match "^xterm" (getenv "TERM"))))
  (require 'xterm-title)
  (xterm-title-mode 1))

;; use magit
(require 'magit)

;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.mkdn" . markdown-mode) auto-mode-alist))
