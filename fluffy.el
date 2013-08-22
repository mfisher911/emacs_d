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
(load "~/.emacs.d/org.el" 'noerror)

;; Twitter
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)

;;; ESS mode
(load "ESS/lisp/ess-site.el")

;; LaTeX additions
(add-hook 'latex-mode-hook
          (function (lambda () (setq sentence-end-double-space nil))))

(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(require 'gist)

(require 'w3m)
(setq newsticker-html-renderer 'w3m-region)

(load-theme 'deeper-blue t)
