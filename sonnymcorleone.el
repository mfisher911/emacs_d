(require 'color-theme)
(color-theme-initialize)
; (color-theme-select)
(setq color-theme-is-global t)
;(color-theme-renegade)
(color-theme-billw)
(set-default-font "-apple-bitstream vera sans mono-medium-r-normal--12-120-72-72-m-120-iso10646-1")

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)
;;;(setq mac-command-modifier 'meta)
(setq x-select-enable-clipboard t)
(setq mac-tool-bar-display-mode 'icons)

;; http://emacs-fu.blogspot.com/2009/04/dot-emacs-trickery.html
; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
; Easy buffer switching by holding down shift and press any arrow key.
(windmove-default-keybindings 'shift)

(server-mode 1)

;;; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

(load "~/.emacs.d/org.el" 'noerror)

(require 'growl)
(defun growl-appt-display (min-to-app new-time msg)
  (growl (format "Appointment in %s min." min-to-app)
         (format "Time: %s\n%s" new-time msg)))
(setq appt-disp-window-function (function growl-appt-display))

;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
(setq
  appt-message-warning-time 15 ;; warn 15 min in advance
  appt-display-mode-line t     ;; show in the modeline
  appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...
 ;; update appt each time agenda opened

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; ERC
(require 'erc)
(require 'erc-match)
(setq erc-keywords '("mfisher" "spudnuts"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))
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
(add-hook 'erc-text-matched-hook 'my-notify-erc)
