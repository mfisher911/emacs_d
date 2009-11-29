;; This needs to live in ~/.emacs .
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Include version control -- needed for xemacs >= 21 
(cond 
 ((string-match "XEmacs" emacs-version)
  (require 'vc-hooks) 
  ))

;;; try to version sniff a bit
(cond
 ((string-match "23.1.50.1" emacs-version)
  (setq mac-command-modifier 'meta)
  (setq default-frame-alist (quote ((tool-bar-lines . 0)
                                    (menu-bar-lines . 1)
                                    (width . 80)
                                    (height . 43))))
  (when (equal system-type 'darwin)
    (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
    (push "/usr/local/bin" exec-path)
    (push "/opt/local/bin" exec-path))
  ))
(cond
 ((and (boundp 'aquamacs-version-id)
       (> aquamacs-version-id 200))
  (setq text-mode-hook (quote (auto-detect-wrap smart-spacing-mode)))
  (setq default-frame-alist (quote ((tool-bar-lines . 0)
                                    (menu-bar-lines . 1)
                                    (width . 80)
                                    (height . 40))))
  (if (fboundp 'tabbar)
      (setq tabbar-mode nil nil (tabbar)))
))

;; turn on the clock
(load "time")
(setq display-time-24hr-format t)
(display-time)

;; Prevent the annoying beep on errors 
(setq visible-bell t)

;; Display line and column numbers 
(setq line-number-mode t)
(setq column-number-mode t)

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

;; slurp in other code
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/el/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

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

;; conditional hostname stuff modeled from
;; http://www.ibm.com/developerworks/cn/linux/l-plset/emacs.dat 
;; could probably make this easier
(let ((hostname (system-name)))
  (cond
   ((equal hostname "edison.homeunix.org")
    (load "~/.emacs.d/edison.el" 'noerror)
    )
   ((equal hostname "lapblanket.local")
    (load "~/.emacs.d/lapblanket.el" 'noerror)
    )
   ((equal hostname "MacDVR.local")
    (load "~/.emacs.d/macdvr.el" 'noerror)
    )
   ((equal hostname "sonhouse.son.rochester.edu")
    (load "~/.emacs.d/sonhouse.el" 'noerror)
    )
   ((equal hostname "SONNYCORLEONE")
    (load "~/.emacs.d/sonnycorleone.el" 'noerror)
    )
   ((equal hostname "sonnymcorleone.urmc-sh.rochester.edu")
    (load "~/.emacs.d/sonnymcorleone.el" 'noerror)
    )
   (t (message "Unknown host %s" hostname))
   )
  )
