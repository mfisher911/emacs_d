(setq user-mail-address "mfisher@csh.rit.edu")

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

;; Twitter
(require 'twittering-mode)
(setq twittering-use-master-password t)
