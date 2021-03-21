(setq user-full-name "Mike Fisher"
      user-mail-address "mfisher@csh.rit.edu")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

;; Twitter
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)

(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(use-package gist
  :ensure t)

(load-theme 'leuven)
