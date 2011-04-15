;; enable twitter.el (http://www.busydoingnothing.co.uk/twitter-el/)
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xw" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

(setq user-mail-address "mfisher@csh.rit.edu")

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

;; Haskell mode
(load "~/el/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;; org mode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(load "~/.emacs.d/org.el" 'noerror)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(setq org-agenda-files (quote ("~/Dropbox/org/work.org"
                               "~/Dropbox/org/phone-messages.org"
                               "~/Dropbox/org/movies.org"
                               "~/Dropbox/org/books-read.org"
                               "~/Dropbox/org/books-to-read.org"
                               "~/Dropbox/org/personal.org"
                               )))

(server-mode 1)

;;; package mode
(require 'package)
(package-initialize)
