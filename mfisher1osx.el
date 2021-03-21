(setq user-mail-address "Michael_Fisher@URMC.Rochester.edu")

(setq calendar-location-name "Rochester, NY")
(setq calendar-latitude 43.120031)
(setq calendar-longitude -77.626047)
;; (require 'theme-changer)
;; (change-theme 'solarized-light 'solarized-dark)
(load-theme 'leuven)

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

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

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
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)

(autoload 'apache-mode "apache-mode" nil t)

(autoload 'puppet-mode "puppet-mode" "Puppet manifests-editing mode")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(autoload 'nagios-mode "nagios-mode" nil t)

(autoload 'csv-mode "csv-mode" "Major mode for editing CSV files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; (define-key csv-mode-map (kbd "C-M-n") 'next-logical-line)
;; (define-key csv-mode-map (kbd "C-M-p") 'previous-logical-line)

(load "ESS/lisp/ess-site.el")

(autoload 'graphviz-dot-mode "graphviz-dot-mode"
  "Major mode for editing Graphviz DOT files." t)
(add-to-list 'auto-mode-alist '("\\.[Dd][Oo][Tt]\\'" . graphviz-dot-mode))
(setq graphviz-dot-preview-extension "pdf")

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
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (process-send-string process command)
    (while (and (eq (process-status process) 'run)
		(not (progn
		       (goto-char (point-max))
		       (looking-back "^\\(OK\\|ERR .*\\)\n"))))
      (accept-process-output process 0.1))))

(defun pinentry-curses-test ()
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

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'jedi:setup "jedi" nil t)

(setq w3m-default-display-inline-images t)
