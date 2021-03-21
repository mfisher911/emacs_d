(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2))
(require 'multi-mode)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-renegade)))
(server-mode 1)
(require 'jpl-reformat)
(global-set-key (kbd "C-S-u") 'jpl-reformat-mark-enclosing-block)
(global-set-key (kbd "\C-c j a") 'jpl-reformat-align-enclosing-block)
(global-set-key (kbd "\C-c j p") 'jpl-reformat-parameter-list-toggle-multiple-single)
(require 'perlcritic)
;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified

;;; YASnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

;;; ipython
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'ipython)
(setq ipython-completion-command-string
      "print(';'.join(__IP.Completer.all_completions('%s')))\n")

;;; Magit
(require 'magit)
