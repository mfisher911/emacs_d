;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xw" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

(require 'growl)
(defun growl-appt-display (min-to-app new-time msg)
  (growl (format "Appointment in %s min." min-to-app)
         (format "Time: %s\n%s" new-time msg)))
(setq appt-disp-window-function (function growl-appt-display))

;; Chrome Edit Server -> Emacs
;; (require 'edit-server)
;; (edit-server-start)
;; (setq edit-server-new-frame nil)

;;; Magit
(require 'magit)
(global-set-key (kbd "C-x v \\") 'magit-status)

;; Javascript2 mode.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(server-mode 1)

