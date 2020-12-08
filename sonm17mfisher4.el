;;; sonm17mfisher4 --- Personal customizations
;;;
;;; Commentary:
;;;
;;; Code:
;;;
(setq user-mail-address "Michael_Fisher@URMC.Rochester.edu")

(setq calendar-location-name "Rochester, NY")
(setq calendar-latitude 43.120031)
(setq calendar-longitude -77.626047)
;; (change-theme 'solarized-light 'solarized-dark)
;; (load-theme 'leuven)
;; (load-theme 'sexy-monochrome)

;; https://protesilaos.com/modus-themes/
(load-theme 'modus-operandi t t)
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (enable-theme 'modus-operandi)))

;; Dark at sunset
(load-theme 'modus-vivendi t t)
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (enable-theme 'modus-vivendi)))

(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-bold-constructs t
           modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
           modus-%1$s-theme-syntax 'alt-syntax ; {nil,faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match nil
           modus-%1$s-theme-links 'faint ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
           modus-%1$s-theme-no-mixed-fonts nil
           modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
           modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}
           modus-%1$s-theme-headings  ; Read further below in the manual for this one
           '((1 . section)
             (2 . line)
             (t . highlight))
           modus-%1$s-theme-variable-pitch-headings nil
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (load-theme 'modus-%1$s t))
   theme))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))


;; Add flyspell mode for "itsalltext" buffers.
(add-hook 'find-file-hooks
          (lambda ()
            (if (and (>= (length buffer-file-name) 94)
                     (equal "itsalltext"
                            (substring buffer-file-name 84 94)))
                (flyspell-mode))))

(add-hook 'find-file-hooks
          (lambda ()
            (when (and (>= (length buffer-file-name) 117)
                       (or (equal "rt.son.rochester.edu"
                                  (substring buffer-file-name 95 115))
                           (equal "wiki.son.rochester.edu"
                                  (substring buffer-file-name 95 117))))
              (auto-fill-mode -1)
              (visual-line-mode t)
              (local-set-key (kbd "C-c C-z") 'maf-delete-to-sigdashes)
              (local-set-key (kbd "C-c C-c") 'maf-close-iat))))

;; Eshell + sudo + tramp help
;; https://emacs.stackexchange.com/a/5619
;; at some point, need to do: ```alias sudo 'eshell/sudo $*'```
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)
(require 'em-tramp)
(require 'em-alias)
(setq eshell-prefer-lisp-functions t)
(setq password-cache t)
(setq password-cache-expiry 600)

;; Add flyspell pychecker for Python.
;; http://www.plope.com/Members/chrism/flymake-mode
;; (require 'flymake)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init () ; Make sure it's not a remote buffer or flymake would not work
;;     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;              (local-file (file-relative-name
;;                           temp-file
;;                           (file-name-directory buffer-file-name))))
;;         (list "pyflakes" (list local-file)))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "epylint" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ansible
  :ensure t)

(use-package ansible-doc
  :ensure t)


(use-package sqlup-mode
  :ensure t
  :config
  ;; Set a global keyword to use sqlup on a region
  ;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-w64")))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 3))
; (add-hook 'markdown-mode-hook #'flycheck-mode)
; (add-hook 'text-mode-hook #'flycheck-mode)

;; http://emacs-fu.blogspot.com/2009/04/dot-emacs-trickery.html
; Make scripts executable on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

;; Work around RT noise and throw out email reply threads.
(defun maf-delete-to-sigdashes ()
  "Clear out signatures and noise in RT tickets."
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (re-search-forward "^-- " nil t)
      (backward-char 3)
      (delete-region beg (point)))
     (insert "\n\n\n"))
  (forward-char 1))

;; Make it easy to get rid of an Its-All-Text buffer.
(defun maf-close-iat ()
  "Close an It's All Text buffer."
  (interactive)
  (save-buffer)
  (kill-buffer)
  (delete-frame))

;; Language/Config specific modes
(use-package visual-basic-mode
  :config
  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                   visual-basic-mode)) auto-mode-alist)))

(use-package apache-mode
  :ensure t)

(autoload 'nagios-mode "nagios-mode" nil t)

(autoload 'icinga2-mode "icinga2-mode" nil t)

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'")

;; (define-key csv-mode-map (kbd "C-M-n") 'next-logical-line)
;; (define-key csv-mode-map (kbd "C-M-p") 'previous-logical-line)

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.[Dd][Oo][Tt]\\'"
  :config
  (setq graphviz-dot-preview-extension "pdf"))

;; omit files <http://www.20seven.org/journal/2008/11/emacs-dired-directory-management.html>
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq bol "svn" eol)           ;; svn dirs
              (seq ".pyc" eol)
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)

;; (setq flymake-perlcritic-command
;;       "~/el/emacs-flymake-perlcritic/bin/flymake_perlcritic")
;; (setq flymake-perlcritic-severity 1)
;; (setq flymake-run-in-place nil)
;; (require 'flymake-perlcritic)

(defun pinentry-curses-send-command (process command)
  "Copied this from somewhere for PGP interaction with PROCESS and COMMAND."
  (with-current-buffer
    (erase-buffer)
    (process-send-string process command)
    (while (and (eq (process-status process) 'run)
		(not (progn
		       (goto-char (point-max))
		       (looking-back "^\\(OK\\|ERR .*\\)\n" nil))))
      (accept-process-output process 0.1))))

(defun pinentry-curses-test ()
  "Test to see whether pinentry (curses) is able to process."
  (interactive)
  (unwind-protect
      (let* ((buffer (generate-new-buffer "pinentry"))
	     (process (start-process "pinentry" buffer "pinentry-curses"))
	     (inhibit-redisplay t))
	(pinentry-curses-send-command process
				      (format "OPTION ttyname=%s\n"
					      (terminal-name)))
	(pinentry-curses-send-command process
				      (format "OPTION ttytype=%s\n"
					      (tty-type)))
	(pinentry-curses-send-command process "GETPIN\n")
	(kill-process process))
    (redraw-frame (selected-frame))))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))
;;   (defalias 'workon 'pyvenv-workon))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (setq web-mode-code-indent-offset 2))

(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  (setq blacken-line-length 78))

;; (use-package isortify
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'isortify-mode)
;;   (setq isortify-line-width 78))

; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))


;;; sonm17mfisher4.el ends here
