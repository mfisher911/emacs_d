(setq user-full-name "Mike Fisher"
      user-mail-address "mfisher@csh.rit.edu")

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

(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(require 'gist)

(load-theme 'leuven)

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'jedi:setup "jedi" nil t)

(require 'clojure-mode)
(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; PHP mode
(autoload 'php-mode "php" nil t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
