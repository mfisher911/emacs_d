(require 'color-theme)
(color-theme-initialize)
; (color-theme-select)
(setq color-theme-is-global t)
;(color-theme-renegade)
(color-theme-billw)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (lambda() (set-fill-column 65)))

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)