;;; gpg.el -- local customizations related to GnuPG
;;; Commentary:
;;;   GPG customizations.
;;; Code:
;; Do not use gpg agent when running in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  "Check for a graphic display and only use EPG if we have one."
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

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

(provide 'gpg)
;;; gpg.el ends here
