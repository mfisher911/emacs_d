;;; darwin.el -- local Darwin (macOS) OS customizations
;;; Commentary:
;;;   Darwin OS customizations.
;;; Code:
(setq color-theme-is-global nil)

(define-key global-map [ns-drag-file] 'ns-find-file)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super) ; make opt key do Super
(global-set-key [s-backspace] 'backward-kill-word) ;; match terminal
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;; http://slashusr.wordpress.com/2009/08/09/using-m-from-switching-emacs-frames-on-osx/
(global-set-key (kbd "M-`") 'other-frame) ; # This sets the key binding
(setq select-enable-clipboard t)
(setq mac-tool-bar-display-mode 'icons)
(setq default-frame-alist (quote ((tool-bar-lines . 0)
                                  (width . 80)
                                  (height . 43))))
(setenv "TMPDIR" "~/.tmp/")
(unless (file-exists-p (getenv "TMPDIR"))
  (progn (make-directory (getenv "TMPDIR"))
         (set-file-modes (getenv "TMPDIR") ?\700)))
(setq temporary-file-directory "~/.tmp/")
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq tramp-temp-name-prefix
      (concat (getenv "TMPDIR") "tramp."))


(defun pbcopy ()
  "Copy the current paste buffer to the Mac."
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  "Copy the Mac's current paste buffer."
  (interactive)
  (call-process-region (point)
                       (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  "Cut the current region."
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

;; stop shocking myself with text resizes
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

  ;;; Useful for https://github.com/dunn/company-emoji
;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
;; not tested with emacs26 (requires a patched Emacs version for multi-color font support)
(if (version< "27.0" emacs-version)
    (set-fontset-font
     "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; TeX Live for Mac
;; (add-to-list 'Info-default-directory-list
;;              "/usr/local/texlive/2019/texmf-dist/doc/info/")
(add-hook 'Info-mode-hook (lambda ()
                            (setq Info-additional-directory-list
                                  Info-default-directory-list)))

(provide 'darwin)
;;; darwin.el ends here

;; Steps to fix the info pages:
;; install_info=$(brew ls -l texinfo| grep 'bin/install-info')
;; cd $(brew ls -l emacs-plus@29| grep 'info/emacs/dir$'| sed -e 's|/dir||')
;; cd /opt/homebrew/Cellar/emacs-plus@29/29.0.50/share/info/emacs
;; for i in *.info; do
;;     $install_info $i --dir-file dir;
;; done
