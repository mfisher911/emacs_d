;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xw" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

(require 'growl)
(defun growl-appt-display (min-to-app new-time msg)
  (growl (format "Appointment in %s min." min-to-app)
         (format "Time: %s\n%s" new-time msg)))
(setq appt-disp-window-function (function growl-appt-display))

;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
(setq
  appt-message-warning-time 12 ;; warn 15 min in advance
  appt-display-mode-line t     ;; show in the modeline
  appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...
 ;; update appt each time agenda opened

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

