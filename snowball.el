;;; package -- Summary
;;; Commentary:
;;
;; Computer-specific configuration items
;;
;;; Code:
(setq user-full-name "Mike Fisher"
      user-mail-address "mfisher911@gmail.com")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

(defalias 'list-buffers 'ibuffer)

(load-theme 'leuven)

(use-package yaml-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 3))

(use-package pyvenv
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (defalias 'workon 'pyvenv-workon))

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

(provide 'snowball)
;;; snowball.el ends here
