(setq user-full-name "Mike Fisher"
      user-mail-address "mfisher@csh.rit.edu")

;;; org mode
(load "~/.emacs.d/org.el" 'noerror)

;; Twitter
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)

(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(use-package gist
  :ensure t)

(load-theme 'leuven)

;;; https://gist.github.com/arnested/afd421c89a68b874e1c0
;; Use align-left icon for `auto-fill-mode'.
(use-package diminish
  :config
  (diminish 'auto-fill-function (concat " " [#xF036])))

;; Use thumbs-up / thumbs-down for flymake status.
;; We need to reimplement `flymake-report-status' to make this happen.
(eval-after-load 'flymake
  '(defun flymake-report-status (e-w &optional status)
     "Show status in mode line."
     (when e-w
       (setq flymake-mode-line-e-w e-w))
     (when status
       (setq flymake-mode-line-status status))
     (let* ((mode-line " "))
       (if (> (length flymake-mode-line-e-w) 0)
           (setq mode-line (concat mode-line [#xF165] flymake-mode-line-e-w))
         (setq mode-line (concat mode-line [#xF164])))
       (setq mode-line (concat mode-line flymake-mode-line-status))
       (setq flymake-mode-line mode-line)
       (force-mode-line-update))))
