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

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-load-themes))

(use-package circadian
  :ensure t
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

;; (change-theme 'solarized-light 'solarized-dark)
;; (load-theme 'leuven)
;; (load-theme 'sexy-monochrome)

(use-package logview
  :ensure t)

(use-package mermaid-mode
  :ensure t)

;; this seems neat: https://github.com/Boruch-Baum/emacs-crossword

(use-package banner-comment
  :ensure t
  :commands (banner-comment)
  :bind ("C-c h" . banner-comment))

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init
  (if after-init-time
      (edit-server-start)
    (add-hook 'after-init-hook
              #'(lambda() (edit-server-start))))
  :config
  (setq edit-server-new-frame-alist
        '((name . "Edit with Emacs Frame"))))

;; Add flyspell mode for "itsalltext" buffers.
(add-hook 'edit-server-start-hook
          (lambda ()
            (when (equal 0 (string-match "^wiki.son.rochester.edu"
                                       (buffer-name)))
                (maf-edit-dokuwiki))))

;; Add flyspell mode for "itsalltext" buffers.
(add-hook 'edit-server-start-hook
          (lambda ()
            (when (equal 0 (string-match "^rt.son.rochester.edu"
                                         (buffer-name)))
              (auto-fill-mode -1)
              (visual-line-mode t)
              (flyspell-mode)
              (local-set-key (kbd "M-]") "\t")
              (local-set-key (kbd "C-c C-z") 'maf-delete-to-sigdashes)
              (local-set-key (kbd "C-c C-c") 'maf-close-iat))))

;; https://github.com/purcell/emacs-shfmt/
(use-package shfmt
  :ensure t
  :hook (sh-mode-hook . shfmt-on-save-mode))

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

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))
  ;(setq lsp-ui-doc-delay 15))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package py-isort
  :ensure t)

;; (global-eldoc-mode -1)

(use-package python-pytest
  :ensure t)

(defun eldoc-docstring-format-sym-doc (a b c)
  "Get rid of errors by ignoring this function (and A, B, C).")


;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "python")
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pyls-remote))

(use-package ansible
  :ensure t)

(use-package ansible-doc
  :ensure t)

(use-package dokuwiki-mode
  :ensure t)

(use-package dokuwiki
  :ensure t
  :config
  (setq dokuwiki-xml-rpc-url
        "https://wiki.son.rochester.edu/lib/exe/xmlrpc.php")
  (setq dokuwiki-login-user-name "mfisher4"))

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
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-w64")))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 3)
  (flycheck-add-next-checker 'python-flake8 '(warning . python-pylint))
  :hook
  ;; override the LSP checker
  (python-mode . ((flycheck-checker . python-flake8))))
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

(defun maf-edit-dokuwiki ()
  "Set up a buffer for editing a Dokuwiki page."
  (interactive)
  (auto-fill-mode -1)
  (visual-line-mode t)
  (local-set-key (kbd "C-c C-c") 'maf-close-iat)
  (dokuwiki-mode)
  (flyspell-mode))

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
  :custom
  (graphviz-dot-preview-extension "pdf"))

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

;; https://gitlab.com/semente/dotfiles/-/blob/master/emacs/.emacs
;; bury certain buffers instead of killing them
(progn
  (defvar maybe-bury-buffer-list '("*scratch*" "*Messages*" "*notmuch-hello*"
                                   "*Flycheck error messages*" "*Help*"))
  (defun maybe-bury-kill-buffer-query-function ()
    "Bury buffers listed in `maybe-bury-buffer-list' instead of
killing them."
    (if (member (buffer-name (current-buffer)) maybe-bury-buffer-list)
        (bury-buffer)
      t))
  (add-hook 'kill-buffer-query-functions
            'maybe-bury-kill-buffer-query-function))

(highlight-indentation-mode -1)

(defun refresh-rt ()
  "Perform the RT Ticket refresh."
  (interactive)
  (save-excursion
    (let (start end shell-command-dont-erase-buffer)
      (setq shell-command-dont-erase-buffer t)
      (search-forward "RT Tickets:\n")
      (setq start (point-marker))
      (forward-line 4)
      (shell-command "rt_summary_load.py" (current-buffer))
      (forward-line 4)
      (setq end (point-marker))
      (kill-region start end nil)
      (shell-command "rt_history.py" (current-buffer)))))

(defun weekend-update ()
  "Grab the new details of RT tickets and make a summary comparison."
  (interactive)
  (refresh-rt)
  (save-excursion
    (let (start end)
      (search-forward "RT Tickets:\n")
      (setq start (point-marker))
      (forward-line 4)
      (setq end (point-marker))
      (copy-region-as-kill start end nil)))
  (save-excursion
    (org-capture nil "w")
    (org-capture-finalize))
  (save-excursion
    (search-forward "RT Tickets:\n")
    (yank)))

(defun update-attendance ()
  "Update the attendance list."
  (interactive)
  (save-excursion
    (let (start end)
      (org-beginning-of-item)
      (setq start (point-marker))
      (org-end-of-item)
      (setq end (point-marker))
      (call-process-region start end "sort_attendees.py" nil t)
      (kill-region start end nil)))
  (org-fill-paragraph t t))

(provide 'sonm17mfisher)
;;; sonm17mfisher4.el ends here
