(require 'color-theme)
(color-theme-initialize)
; (color-theme-select)
(setq color-theme-is-global t)
;(color-theme-renegade)
(color-theme-billw)

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)
;;;(setq mac-command-modifier 'meta)
(setq x-select-enable-clipboard t)
(setq mac-tool-bar-display-mode 'icons)

;; Add flyspell mode for "itsalltext" buffers.
(add-hook 'find-file-hooks
          (lambda ()
            (if (and (>= (length buffer-file-name) 94)
                     (equal "itsalltext"
                            (substring buffer-file-name 84 94)))
                (flyspell-mode))))

;; Add flyspell pychecker for Python.
;; http://www.plope.com/Members/chrism/flymake-mode
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; http://emacs-fu.blogspot.com/2009/04/dot-emacs-trickery.html
; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
; Easy buffer switching by holding down shift and press any arrow key.
(windmove-default-keybindings 'shift)

(server-mode 1)

;;; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/el/yasnippet-read-only/snippets")

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
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...
 ;; update appt each time agenda opened

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

;; Disable the Chrome Edit server
;; (require 'edit-server)
;; (edit-server-start)
;; (setq edit-server-new-frame nil)

;;; Magit
(require 'magit)
(global-set-key (kbd "C-x v \\") 'magit-status)