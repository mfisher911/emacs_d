(setq user-mail-address "Michael_Fisher@URMC.Rochester.edu")

(require 'color-theme)
(color-theme-initialize)
; (color-theme-select)
(setq color-theme-is-global t)
(color-theme-billw)

;; Add flyspell mode for "itsalltext" buffers.
(add-hook 'find-file-hooks
          (lambda ()
            (if (and (>= (length buffer-file-name) 94)
                     (equal "itsalltext"
                            (substring buffer-file-name 84 94)))
                (flyspell-mode))))

;; Add flyspell pychecker for Python.
;; http://www.plope.com/Members/chrism/flymake-mode
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; http://emacs-fu.blogspot.com/2009/04/dot-emacs-trickery.html
; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(require 'google-weather)
(require 'org-google-weather)
(setq org-agenda-files (quote ("~/Dropbox/org/work.org.gpg"
                               "~/Dropbox/org/phone-messages.org.gpg"
                               "~/Dropbox/org/refile.org"
                               "~/Dropbox/org/misc.org.gpg"
                               "~/Dropbox/org/weather.org"
                               )))

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
(global-set-key (kbd "C-c \\") 'maf-delete-to-sigdashes)

;; RT Liberation
(require 'rt-liberation)
(require 'rt-liberation-update)
(setq rt-liber-rt-binary "~/rt-3.8.9/bin/rt"
      rt-liber-rt-version "3.8.9"
      rt-liber-username "mfisher"
      rt-liber-base-url "https://rt.son.rochester.edu/"
      rt-liber-update-default-queue "SONHelp")

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
