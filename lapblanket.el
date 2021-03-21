;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xw" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-read-only/snippets")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)
