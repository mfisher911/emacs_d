;;; perl.el -- local customizations related to Perl
;;; Commentary:
;;;   Perl language customizations.
;;; Code:
;;;
;; From http://www.perlmonks.org/?node_id=650413
;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\(cgi\\|t\\)\\$" . cperl-mode))
(add-hook 'cperl-mode-hook (setq show-trailing-whitespace t))

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

(provide 'perl)
;;; perl.el ends here
