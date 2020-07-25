;;; init.el --- personal customizations
;;; Commentary:
;;;   Contains my common Emacs customizations and loads additional
;;;   machine-specific customizations.
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

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
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; From http://www.perlmonks.org/?node_id=650413
;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\(cgi\\|t\\)\\$" . cperl-mode))
(add-hook 'cperl-mode-hook (setq show-trailing-whitespace t))

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

;; http://www.perlmonks.org/?abspart=1;displaytype=displaycode;node_id=516539;part=1
(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (let ((orig-point (point)))
    (shell-command-on-region
     (point-min) (point-max)
     "perltidy" nil t shell-command-default-error-buffer)
    (goto-char (if (<= orig-point (point-max))
                   orig-point
                 (point-max)))))

;; Got this from Kai, who may have gotten it from Tom Christiansen
(defun perldoc (man-args)
  "Launch perldoc for MAN-ARGS."
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (manual-entry man-args)))

;; Make all "yes or no" prompts show "y or n" instead
;; http://www.dotemacs.de/dotfiles/AlexanderMikhailian.emacs.html
(fset 'yes-or-no-p 'y-or-n-p)

;; http://www.hulubei.net/tudor/configuration/.emacs
(setq make-backup-files nil)
(setq tramp-backup-directory-alist backup-directory-alist)

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
(setq-default show-trailing-whitespace nil)
(setq default-indicate-empty-lines t)
(savehist-mode 1)

;; always enable paren mode
(show-paren-mode t)
(setq show-paren-style 'mixed)

;;; force the local org-mode
(add-to-list 'load-path "~/el/org-mode/lisp") (require 'org-install)

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
  (setenv "PATH" (concat "~/bin:/usr/local/bin:"
                         (getenv "PATH")
                         ":/Library/TeX/texbin"))
  (push "/usr/local/bin" exec-path)
  (push "/usr/texbin" exec-path)

  ;; HTTP Error Messages
  (use-package httpcode
    :ensure t)

  ;;; ESS mode
;;   (use-package ess
;;      :ensure t
;;      :config
;;      (load "ess-site"))

  ;; LaTeX additions
  (add-hook 'latex-mode-hook
            (function (lambda () (setq sentence-end-double-space nil))))

  ;; full-screen mode
  (global-set-key (kbd "M-F") 'toggle-frame-fullscreen)

  (add-to-list 'Info-default-directory-list
                "/usr/local/texlive/2015/texmf-dist/doc/info/")
  (add-hook 'Info-mode-hook (lambda ()
                              (setq Info-additional-directory-list
                                    Info-default-directory-list)))

  (defun load-gpg-agent-info ()
    "Load the GPG Agent's info from disk."
    (interactive)
    (let ((oldbuf (current-buffer)))
      (save-current-buffer
        (view-file "~/.gpg-agent-info")
        (setenv "GPG_AGENT_INFO"
                (buffer-substring (search-forward "=")
                                  (line-end-position)))
        (kill-buffer))))

  ;(load-gpg-agent-info)

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

  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji")
                    nil 'prepend)

  ;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
  (setq
   appt-message-warning-time 15 ;; warn 15 min in advance
   appt-display-mode-line t)    ;; show in the modeline
  (appt-activate 1))

;; And for FreeBSD -- if needed
;; (when (equal system-type 'berkeley-unix))

(use-package jedi
  :ensure t
  :init
  (setq jedi:setup-keys t)
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (jedi:setup))

;; python stuff from http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(add-hook 'python-mode-hook 
          '(lambda ()
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             (setq show-trailing-whitespace t)))
(defun py-next-block ()
  "Go to the next block.  Cf. `forward-sexp' for 'lisp-mode'."
  (interactive)
  (py-mark-block nil 't)
  (back-to-indentation))

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
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t))

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
   ((equal hostname "SONNYCORLEONE")
    (load "~/.emacs.d/sonnycorleone.el" 'noerror))
   (t (load conf-file 'noerror))
))

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

;;; Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x v \\") 'magit-status)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (setq magit-git-global-arguments
        (nconc magit-git-global-arguments
               '("-c" "color.ui=false"
                 "-c" "color.diff=false"))))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  "Make Magit have a full-screen window."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;;
;; TeXcount setup for TeXcount version 2.3
;;
(defun texcount-setup ()
  "Make a container function for doing LaTeX word counting."
  (defun latex-word-count ()
    (interactive)
    (let*
      ( (this-file (buffer-file-name))
        (enc-str (symbol-name buffer-file-coding-system))
        (enc-opt
          (cond
            ((string-match "utf-8" enc-str) "-utf8")
            ((string-match "latin" enc-str) "-latin1")
            ("-encoding=guess")
        ) )
        (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "/usr/texbin/texcount"
                            nil t nil "-0" enc-opt this-file)
      ) ) ) )
      (message word-count)
  ) )
  (define-key latex-mode-map (kbd "C-c C-w") 'latex-word-count)
)
(add-hook 'latex-mode-hook 'texcount-setup t)
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

(use-package autoinsert
  :config
  (define-auto-insert
    '("\\.md\\'" . "Markdown skeleton")
    '("Pelican blog headers: "
      "Title: " _ "\n"
      "Date: " (format-time-string "%Y-%m-%d %H:%M") "\n"
      "Tags: " "\n"
      "Slug: " (file-name-sans-extension (buffer-name)) "\n"
      "Category: " "\n"
      "Author: " (user-full-name) "\n\n")))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq default-directory "~/")
