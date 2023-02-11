;;; init.el --- personal customizations
;;; Commentary:
;;;   Contains my common Emacs customizations and loads additional
;;;   machine-specific customizations.
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(require 'bind-key)

;; https://github.com/jwiegley/use-package 2022-11-30
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(server-start)
(setq save-abbrevs 'silently)

;; turn on the clock
(use-package time
  :config
  (setq display-time-24hr-format t)
  (display-time))

;; Prevent the annoying beep on errors
(setq visible-bell nil)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; disable tool bars and scroll bars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; Stuff for LaTeX
(setq tex-default-mode (quote latex-mode))
(add-hook 'latex-mode-hook 'turn-on-flyspell)
(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;; Stuff for C/C++ programming
(setq c-basic-offset 4)
(setq c-tab-always-indent nil)

;; Default to text mode with auto-fill-mode on.
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; From Peter.Weiss@Informatik.Uni-Oldenburg.DE (Peter Weiss)
;; Sun Nov 12 1995
(defun match-paren (arg)
  "Go to the matching parenthesis for ARG if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(")
         (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)")
         (forward-char 1) (backward-list 1))
        (t
         (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; Make all "yes or no" prompts show "y or n" instead
;; https://github.com/SystemCrafters/crafted-emacs/issues/43#issue-1142056835
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; http://www.hulubei.net/tudor/configuration/.emacs
(setq make-backup-files nil)
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-default-method "sshx")

;; https://superuser.com/a/179608
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; https://emacs.stackexchange.com/a/22305
(setq tramp-copy-size-limit nil)

; disable tab indent
(setq-default indent-tabs-mode nil)

;; improve paragraph flowing
(setq sentence-end-double-space nil)

;; comment-handling
(setq comment-style '(indent multi))

;; Always add a final newline
(setq require-final-newline t)
(setq default-indicate-empty-lines t)
(savehist-mode 1)

(add-hook 'prog-mode-hook #'show-paren-local-mode)
(setq show-paren-style 'mixed)

;; slurp in other code
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/el/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;; make the Mac experience consistent
(when (equal system-type 'darwin)
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


  (add-to-list 'Info-default-directory-list
                "/usr/local/texlive/2019/texmf-dist/doc/info/")
  (add-hook 'Info-mode-hook (lambda ()
                              (setq Info-additional-directory-list
                                    Info-default-directory-list)))


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
    (exec-path-from-shell-initialize)))


(setq inhibit-splash-screen t)
(setq initial-scratch-message
      (format ";; scratch buffer created %s\n;; %s\n;;%s\n;; happy hacking\n\n"
              (format-time-string "%Y-%m-%d at %T")
              (car (split-string (emacs-version) "\n"))
              (car (cdr (split-string (emacs-version) "\n")))))
(defun create-scratch-buffer nil
  "Create a customized scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (insert initial-scratch-message))

;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode))

;; conditional loads; try to get "~/.emacs.d/" + hostname + ".el"
;; (hostname is trimmed after first dot)
;; Silently continues if the local file doesn't exist.
(let ((conf-file (concat "~/.emacs.d/"
                         (car (split-string
                               (downcase (system-name))
                               "\\."))
                         ".el"))
      (hostname (system-name)))
  (cond
   ((string-match "vpn-client-.*" hostname)
    (load "~/.emacs.d/sonm17mfisher4.el" 'noerror))
   (t (load conf-file 'noerror))
))


;; specific imports
(mapc (lambda (conf)
        (load conf 'noerror))
      '("perl.el" "python.el"))
;; others: gnus.el, gpg.el, latex.el

;;; Magit
(use-package magit
  :ensure t
  :bind ("C-x v \\" . magit-status)
  :config
  (setq magit-push-always-verify nil)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(setq compilation-ask-about-save nil)

;;; http://whattheemacsd.com/appearance.el-01.html
(defmacro rename-modeline (package-name mode new-name)
  "Rename modeline entries for PACKAGE-NAME's MODE as NEW-NAME."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Javascript2 mode.
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (rename-modeline "js2-mode" js2-mode "JS2"))

(if (boundp 'mode-line-compact)  ;; requires Emacs 28.1+
    (setq mode-line-compact t))

;;; http://superuser.com/a/604264/14385
(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region (BEG..END) with ASCII quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "``")
                            ("\x201D" . "''")
                            ("\x2018" . "`")
                            ("\x2019" . "'")
                            ("’" . "'")
                            ("“" . "``")
                            ("”" . "''")
                            ("–" . "--"))
                          nil beg end))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook 'turn-on-flyspell))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq default-directory "~/")

;; HTTP Error Messages
(use-package httpcode
  :ensure t)

(provide 'init)
(put 'narrow-to-region 'disabled nil)
;;; init.el ends here

;; Steps to fix the info pages:
;; install_info=$(brew ls -l texinfo| grep 'bin/install-info')
;; cd $(brew ls -l emacs-plus@29| grep 'info/emacs/dir$'| sed -e 's|/dir||')
;; cd /opt/homebrew/Cellar/emacs-plus@29/29.0.50/share/info/emacs
;; for i in *.info; do
;;     $install_info $i --dir-file dir;
;; done
