;;; http://doc.norang.ca/org-mode.html
;;;
;;; Org Mode
;;;
(require 'org-install)
(require 'org-checklist)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
(add-hook 'org-mode-hook
	(lambda()
         ;; flyspell mode to spell check everywhere
          (flyspell-mode 1)))
;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))
;; Coerce the Org Agenda to Appt mode (mainly for Growl notices).
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(setq org-agenda-files (quote ("~/Dropbox/org/work.org.gpg"
                               "~/Dropbox/org/phone-messages.org.gpg"
                               "~/Dropbox/org/movies.org"
                               "~/Dropbox/org/personal.org"
                               )))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/from-mobile.org")
(setq org-mobile-files (quote ("~/Dropbox/org/work.org.gpg"
                               "~/Dropbox/org/work-notes.org.gpg"
                               "~/Dropbox/org/misc.org.gpg"
                               "~/Dropbox/org/movies.org"
                               "~/Dropbox/org/notes.org"
                               "~/Dropbox/org/personal.org"
                               "~/Dropbox/org/books-read.org"
                               "~/Dropbox/org/books-to-read.org"
                               "~/Dropbox/org/tandberg-install.org")))
(setq org-mobile-force-id-on-agenda-items t)
(setq org-directory "~/Dropbox/org")
(setq org-publish-timestamp-directory "~/Dropbox/org/.org-timestamps/")
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-log-done 'note)
(setq org-log-into-drawer t)

;;;  Load Org Remember Stuff
(org-remember-insinuate)
(define-key global-map "\C-cr" 'org-remember)

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'bh/start-clock-if-needed 'append)

(defun bh/start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

;; Keep clocks running
;; (setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3.1 TODO Keywords
(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
 (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold))))

;; 3.2 Fast Todo Selection
(setq org-use-fast-todo-selection t)

;; 3 remember templates for TODO tasks, Notes, and Phone calls
;;; http://phunculist.wordpress.com/2008/12/18/adding-a-journal-entry-using-org-mode/

(setq org-remember-templates
      '(
        ("todo" ?t
         "* TODO %?
  %u
  %a"
         nil bottom nil)
        ("note" ?n
         "* %?                                        :NOTE:
  %u
  %a"
         nil bottom nil)
        ("phone" ?p
         "** PHONE %^{name} - %^{company|University of Rochester} -                :PHONE:
  Contact Info: %^{phone}
  %u
  :CLOCK-IN:
  %?"
         "phone-messages.org.gpg" "Phone Messages" nil)
        ("book" ?b
         "** TODO %^{Short Title}
  :PROPERTIES:
  :Full-Title: %^{Full-Title}p
  :Author:     %^{Author}p
  :Year:       %^{Year}p
  :Notes:
  :Rating:
  :Review:
  :END:

"
         "books-to-read.org" "Books to Read" nil)
        ("ticket" ?k
         "** TODO %^{Ticket}\n   :CLOCK-IN:\n   %?"
         "work.org.gpg" "Request Tracker Tickets" nil)
        ("misc" ?m
         "* %^{event}\n   :CLOCK-IN:\n   %?"
         "misc.org.gpg" bottom)
        ("Journal" ?j
         ;; "* %U %?\n\n  %i\n  %a"
         "* %U %? %^g\n\n   %x"
         "journal.org" 'date-tree)))

(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5)
                                 ("~/org/misc.org.gpg" :maxlevel . 5)
                                 (nil :maxlevel . 5))))
(setq org-refile-use-outline-path t)

;; 7.5 editing clock entries
(setq org-time-stamp-rounding-minutes (quote (1 15)))

;; 8.1.1 verify clock data's more correct
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; 15.3.5
;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
        
;; 15.7.4 -- add new tasks without disturbin the context
(setq org-insert-heading-respect-content t)

;; 15.7.5 -- don't fire off Firefox by hitting enter on a link
(setq org-return-follows-link nil)

;; 15.8 -- attachment ID management
(setq org-id-method (quote uuidgen))

;; 15.9 -- deadlines
(setq org-deadline-warning-days 30)

;; http://www.emacswiki.org/emacs-en/mobileorg (2010-01-27)
(defun my-org-convert-incoming-items ()
  (interactive)
  (with-current-buffer (find-file-noselect org-mobile-inbox-for-pull)
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (goto-char (match-beginning 0))
      (insert ?*)
      (forward-char 2)
      (insert "TODO ")
      (goto-char (line-beginning-position))
      (forward-line)
      (insert
       (format
        " SCHEDULED: %s
:PROPERTIES:
:ID: %s :END:
"
        (with-temp-buffer (org-insert-time-stamp (current-time)))
        (shell-command-to-string "uuidgen"))))
    (let ((tasks (buffer-string)))
      (erase-buffer)
      (save-buffer)
      (kill-buffer (current-buffer))
      (with-current-buffer (find-file-noselect "~/Dropbox/todo.txt")
        (save-excursion
          (goto-char (point-min))
          (search-forward "* CEG")
          (goto-char (match-beginning 0))
          (insert tasks))))))
 
;; (add-hook 'org-mobile-post-pull-hook 'my-org-convert-incoming-items)

(setq org-mobile-use-encryption t)
