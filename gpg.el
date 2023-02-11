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


(provide 'gpg)
;;; gpg.el ends here
