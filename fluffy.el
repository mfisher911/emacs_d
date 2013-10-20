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

(load-theme 'mustang)

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'jedi:setup "jedi" nil t)

;;; publishing for MKT402
(setq org-publish-project-alist
      '(("mkt402-content"
         :base-directory "~/Dropbox/school/mkt402"
         :base-extension "org"
         :publishing-directory "~/Sites/content/school/mkt402"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
        )
        ("mkt402-static"
         :base-directory "~/Dropbox/school/mkt402"
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Sites/content/school/mkt402/"
         :recursive nil
         :publishing-function org-publish-attachment
         )
        ("mkt402" :components ("mkt402-content" "mkt402-static"))))
