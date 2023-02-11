;; ERC
(require 'erc)
(require 'erc-join)
(require 'erc-goodies)
(require 'erc-track)
(require 'erc-truncate)

(use-package erc-hl-nicks
  :ensure t
  :init (erc-hl-nicks-mode))

(setq erc-keywords '("mfisher" "spudnuts"))
(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
(setq erc-track-exclude-types (append erc-hide-list
                                     (quote("324" "329" "332" "333"
                                            "353" "477"))))
;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;;                                 "324" "329" "332" "333" "353" "477"))

(erc-track-mode 1)
(erc-truncate-mode 1)

(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (setq erc-fill-column (- (window-width) 2))))

(setq erc-email-userid "mfisher@shell.fisher.one")
;; (setq erc-nick "mfisher")
(setq erc-prompt-for-password nil)
(setq erc-user-full-name "Mike Fisher")
(setq erc-user-mode "+iw")
(setq erc-max-buffer-size 262144)

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(setq erc-input-line-position -1)

;; http://www.emacswiki.org/emacs/UnwrapLine
(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))
(define-key erc-mode-map (kbd "M-q") 'unwrap-line)

;; http://www.emacswiki.org/emacs/ErcModeline
(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     (format " %S/%S/%S" ops voices members))))

(setq erc-keep-place-indicator t)
(setq erc-keep-place-indicator-follow t)

(defun erc-cmd-MOP ()
  "Op all non-opped users in a channel."
  (let ((to-op '())
        (k nil)
        (v nil))
    (maphash (lambda (k v)
               (if (not (erc-channel-user-op-p k))
                   (setq to-op (add-to-list 'to-op k))))
             erc-channel-users)
    (mapcar 'erc-cmd-OP to-op)))

(defun erc-cmd-SHRUG ()
  "Send a shrug visualization."
  (erc-cmd-SAY "¯\\_(ツ)_/¯"))

(add-hook 'erc-mode-hook
          (function (lambda ()
                      (ncm-mode)
                      (turn-on-flyspell)
                      (erc-keep-place-mode t))))


;;; ignores
(defcustom erc-foolish-content '("^<.*?> \\?" "^<.*?> , *rr"
                                 "\\*CLICK\\*" "\\*BANG\\*"
                                 "^<cat.*> .*: you"
                                 "^<cat.*> .* !!+"
                                 "hate america?")
  "Regular expressions to identify foolish content.
    Usually what happens is that you add the bots to
    `erc-ignore-list' and the bot commands to this list."
  :group 'erc
  :type '(repeat regexp))

(defun erc-foolish-content (msg)
  "Check whether MSG is foolish."
  (erc-list-match erc-foolish-content msg))

(add-hook 'erc-insert-pre-hook
          (lambda (s)
            (when (erc-foolish-content s)
              (setq erc-insert-this nil))))

