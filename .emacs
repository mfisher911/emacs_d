;; This needs to live in ~/.emacs .
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(setq require-final-newline t)

;; Include version control -- needed for xemacs >= 21 
(cond 
 ((string-match "XEmacs" emacs-version)
  (require 'vc-hooks) 
  ))

(server-mode 1)

;; turn on the clock
(load "time")
(setq display-time-24hr-format t)
(display-time)

;; Prevent the annoying beep on errors 
(setq visible-bell t)

;; Display line and column numbers 
(setq line-number-mode t)
(setq column-number-mode t)

;; disable tool bars and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode 0)

;; Stuff for LaTeX
(setq tex-default-mode (quote latex-mode))
(setq tex-dvi-view-command 
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;; Stuff for C/C++ programming
(setq c-basic-offset 4)
(setq c-tab-always-indent nil)

;; Default to text mode with auto-fill-mode on.
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; From http://www.perlmonks.org/?node_id=650413
;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.cgi" . cperl-mode))

;; http://www.perlmonks.org/?abspart=1;displaytype=displaycode;node_id=516539;part=1
(defun perltidy-buffer ()
  "Runs an entire buffer through perltidy."
  (interactive)
  (let ((orig-point (point)))
    (shell-command-on-region
     (point-min) (point-max)
     "perltidy -pbp" nil t shell-command-default-error-buffer)
    (goto-char (if (<= orig-point (point-max))
                   orig-point
                 (point-max)))))

;; Use 4 space indents via cperl mode
(custom-set-variables
  '(cperl-close-paren-offset -4)
  '(cperl-continued-statement-offset 0)
  '(cperl-indent-level 4)
  '(cperl-indent-parens-as-block t)
  '(cperl-tab-always-indent t)
)

;; Got this from Kai, who may have gotten it from Tom Christiansen
(defun perldoc (man-args)
  "Launches perldoc for a given item."
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (manual-entry man-args)))

;; Set up W3 for SSL?
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-host" host "-port" service "-verify"
                                   "0" "-CApath" "/usr/lib/ssl/certs" "-quiet"))

;; Make all "yes or no" prompts show "y or n" instead
;; http://www.dotemacs.de/dotfiles/AlexanderMikhailian.emacs.html
(fset 'yes-or-no-p 'y-or-n-p)

;; http://www.hulubei.net/tudor/configuration/.emacs
(setq make-backup-files nil)
(setq tramp-backup-directory-alist backup-directory-alist)

; disable tab indent
(setq-default indent-tabs-mode nil)

;; improve paragraph flowing
(setq sentence-end-double-space nil)

;; comment-handling
(setq comment-style '(indent multi))

;; set up URL browsing to be more like Terminal.app
(global-set-key (kbd "<M-S-mouse-1>") 'browse-url-at-mouse)

;; http://slashusr.wordpress.com/2009/08/09/using-m-from-switching-emacs-frames-on-osx/
(global-set-key [?\M-`] 'other-frame) ; # This sets the key binding

;; Always add a final newline
(setq require-trailing-newline t)
(savehist-mode 1)

;; always enable paren mode
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; slurp in other code
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/el/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;; make the Mac experience consistent
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq mac-tool-bar-display-mode 'icons)
  (setq default-frame-alist (quote ((tool-bar-lines . 0)
                                    (width . 80)
                                    (height . 43))))
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path)
  (push "/opt/local/bin" exec-path)

  (require 'package)
  (package-initialize)

  ;; Use org-mode from default external install location if possible
  (if (file-exists-p "/usr/local/share/emacs/site-lisp/")
      (setq load-path (cons "/usr/local/share/emacs/site-lisp/" load-path)))

  ;; Send appointment notices through Growl
  (require 'growl)
  (defun growl-appt-display (min-to-app new-time msg)
    (growl (format "Appointment in %s min." min-to-app)
           (format "Time: %s\n%s" new-time msg)))
  (setq appt-disp-window-function (function growl-appt-display))

  ;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
  (setq
   appt-message-warning-time 15 ;; warn 15 min in advance
   appt-display-mode-line t     ;; show in the modeline
   appt-display-format 'window) ;; use our func
  (appt-activate 1))

;; And for FreeBSD -- if needed
;; (when (equal system-type 'berkeley-unix))

;; python stuff from http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(defun py-next-block ()
  "go to the next block.  Cf. `forward-sexp' for lisp-mode"
  (interactive)
  (py-mark-block nil 't)
  (back-to-indentation))

(setq inhibit-splash-screen t)
(setq initial-scratch-message
      (format ";; scratch buffer created %s\n;; happy hacking\n\n"
              (format-time-string "%Y-%m-%d at %T")))
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (insert initial-scratch-message))

;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified

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
   ((equal hostname "edison.homeunix.org")
    (setq erc-system-name "edison.homeunix.org")
    (load "~/.emacs.d/edison.el" 'noerror))
   ((equal hostname "SONNYCHIBA")
    (load "~/emacs_d/sonnychiba.el" 'noerror))
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
(require 'magit)
(global-set-key (kbd "C-x v \\") 'magit-status)

;;; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")
