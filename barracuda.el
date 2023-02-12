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

(setq calendar-location-name "Rochester, NY")
(setq calendar-latitude 43.12)
(setq calendar-longitude -77.63)

;; (load-theme 'leuven)

(use-package modus-themes
  :ensure t)

(use-package circadian
  :ensure t
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

(use-package yaml-mode
  :ensure t)

(use-package banner-comment
  :ensure t
  :commands (banner-comment)
  :bind ("C-c h" . banner-comment))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 3))

(use-package pyvenv
  :ensure t)

;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (defalias 'workon 'pyvenv-workon))

;; https://github.com/wyuenho/emacs-pet/
(use-package pet
  :hook (python-mode)
  :config
  (add-hook 'python-mode-hook 'pet-flycheck-setup))

(use-package banner-comment
  :ensure t
  :commands (banner-comment)
  :bind ("C-c h" . banner-comment))

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

;; (mac-auto-operator-composition-mode)

;; (use-package ligature
;;   :load-path "~/el/ligature.el"
;;   :ensure t
;;   :config
;;   ;; Enable the www ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))

;;   ;; Enable ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;;                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;;                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;;                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;;                                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;;                                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;;                                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;;                                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;;                                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;                                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode 't))


(provide 'barracuda)
;;; barracuda.el ends here
