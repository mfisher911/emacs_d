(require 'color-theme)
(color-theme-initialize)
; (color-theme-select)
(setq color-theme-is-global t)
;(color-theme-renegade)
(color-theme-billw)
(set-default-font "-apple-bitstream vera sans mono-medium-r-normal--12-120-72-72-m-120-iso10646-1")

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)
(setq mac-command-modifier 'meta)
(setq x-select-enable-clipboard t)
(setq mac-tool-bar-display-mode 'icons)

;; http://emacs-fu.blogspot.com/2009/04/dot-emacs-trickery.html
; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
; Easy buffer switching by holding down shift and press any arrow key.
(windmove-default-keybindings 'shift)

(server-mode 1)
