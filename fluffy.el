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

;;; publishing for CIS442A
(setq org-publish-project-alist
      '(("cis442a-content"
         :base-directory "~/Dropbox/school/cis442a"
         :base-extension "org"
         :publishing-directory "~/Sites/content/school/cis442a"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
        )
        ("cis442a-static"
         :base-directory "~/Dropbox/school/cis442a"
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Sites/content/school/cis442a/"
         :recursive nil
         :publishing-function org-publish-attachment
         )
        ("cis442a" :components ("cis442a-content" "cis442a-static"))))

(require 'clojure-mode)
(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; PHP mode
(autoload 'php-mode "php" nil t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
