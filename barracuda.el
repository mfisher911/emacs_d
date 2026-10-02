;;; package -- Summary  -*- lexical-binding: t; -*-
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

;; (load-theme 'leuven)

(setq completion-auto-help 'visible)
(setq completion-auto-select t)

(setq package-check-signature nil)

(use-package modus-themes)
(use-package ef-themes
  :config
  (setq ef-themes-disable-other-themes t))

;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
;; (defun my/apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (ef-themes-select 'ef-spring))
;;     ('dark (ef-themes-select 'ef-bio))))

;;     ('dark (load-theme 'modus-vivendi t))))

;; (my/apply-theme ns-system-appearance)
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;;; 2023-02-25 -- disabled in favor of the my/apply-theme choice
;; https://github.com/GuidoSchmidt/circadian.el
;; (use-package circadian
;;   :config
;;   (setq circadian-themes '((:sunrise . modus-operandi)
;;                            (:sunset  . modus-vivendi)))
;;   (setq calendar-latitude 43.12)
;;   (setq calendar-longitude -77.63)
;;   (circadian-setup))

(use-package yaml-mode)

(use-package banner-comment
  :commands (banner-comment)
  :bind ("C-c h" . banner-comment))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 3))

;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  :config
  (elpy-enable))

;; https://github.com/wyuenho/emacs-pet/
(use-package pet
  :hook (python-mode)
  :config
  (add-hook 'python-mode-hook 'pet-flycheck-setup))

(use-package apache-mode)

(use-package csv-mode)

(use-package web-mode
  :config
  (setq web-mode-code-indent-offset 2))

;; (use-package blacken
;;   :config
;;   (add-hook 'python-mode-hook 'blacken-mode)
;;   (setq blacken-line-length 78))

;; 2025-02-22: switch from blacken to ruff
(use-package ruff-format)
;;   (add-hook 'python-mode-hook 'ruff-format-on-save-mode))

;; 2025-02-22: also switch to Apheleia
;; https://docs.astral.sh/ruff/editors/setup/#emacs
;; Replace default (black) to use ruff for sorting import and formatting.
;; As of 2025-02-22, doesn't work over TRAMP.
(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))


;; (mac-auto-operator-composition-mode)

;; (use-package envrc
;;   :config
;;   (envrc-global-mode))

(use-package direnv
  :config
  (direnv-mode))

(defun my-package-recompile()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 2023-02-26
;; https://github.com/xuchunyang/grab-mac-link.el/
;; M-x grab-mac-link
;; (grab-mac-link 'safari 'markdown) ;; or 'org
(use-package grab-mac-link
  :config
  (setq grab-mac-link-dwim-favourite-app 'safari))

;; https://codeberg.org/acdw/titlecase.el
;; M-x titlecase-{region,line,sentence}
(use-package titlecase)

;; 2023-03-02
;; https://github.com/Wilfred/helpful/
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;; 2023-03-03
;; https://github.com/erickgnavar/flymake-ruff
(use-package flymake-ruff)

;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; https://www.reddit.com/r/emacs/comments/10yzhmn/flymake_just_works_with_ruff/
;; 2025-02-22 disable this python-flymake config
;; (add-hook 'python-base-mode-hook 'flymake-mode)
;; (setq python-flymake-command
;;       '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
(setq-default sh-shellcheck-arguments "-x") ; follow sourced libraries
(add-hook 'sh-base-mode-hook 'flymake-mode) ; requires shellcheck

;; note: splits "(use-package" and PACKAGE on different lines :\
;; https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; 2023-12-09 -- LaTeX Preview Pane
;; Refresh Preview (bound to M-p)
;; Open in External Program (Bound to M-P)
(use-package latex-preview-pane
  :init (latex-preview-pane-enable))

(use-package fish-mode
  :ensure t
  :config
  (add-hook 'fish-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        'fish_indent-before-save))))

;; https://github.com/ArthurHeymans/emacs-tramp-rpc
(use-package tramp-rpc
  :after tramp
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
            :rev :newest
            :lisp-dir "lisp"))
;; Access remote files using the rpc method:

;; /rpc:user@host:/path/to/file
;; On first connection, the server binary is automatically obtained and deployed:

;; Download from GitHub Releases (fastest, ~850KB download)
;; Build from source if Rust is installed and download fails
;; The binary is cached locally in ~/.emacs.d/tramp-rpc/ and deployed to ~/.cache/tramp-rpc/ on the remote host.

;; Deployment Commands

;; Command	Description
;; M-x tramp-rpc-deploy-status	Show binary deployment status
;; M-x tramp-rpc-deploy-clear-cache	Clear local binary cache
;; M-x tramp-rpc-deploy-remove-binary	Remove binary from remote



(provide 'barracuda)
;;; barracuda.el ends here
