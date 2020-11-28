(setq user-full-name "Mike Fisher"
      user-mail-address "mfisher911@gmail.com")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

;; Twitter
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)

(defalias 'list-buffers 'ibuffer)
(use-package gist
  :ensure t)

(load-theme 'leuven)

(use-package yaml-mode
  :ensure t)

;;; https://gist.github.com/arnested/afd421c89a68b874e1c0
;; Use align-left icon for `auto-fill-mode'.
;; (use-package diminish
;;   :config
;;   (diminish 'auto-fill-function (concat " " [#xF036])))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package pyvenv
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (defalias 'workon 'pyvenv-workon))

;; (setq python-shell-interpreter "frameworkpython"
;;       python-shell-interpreter-args "-m IPython")


;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package apache-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-code-indent-offset 2))

(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  (setq blacken-line-length 78))
