(menu-bar-mode -1)

(setq calendar-location-name "Rochester, NY")
(setq calendar-latitude 43.120031)
(setq calendar-longitude -77.626047)

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi :no-confirm))

(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  (setq blacken-line-length 78))

(use-package python-isort
  :hook (python-mode-hook . python-isort-on-save-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package pet
  :hook (python-mode))

(add-hook 'python-mode-hook 'pet-flycheck-setup)

;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (load-theme 'ef-deuteranopia-dark t))

(require 'erc)
(require 'znc)
