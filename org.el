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

;; Coerce the Org Agenda to Appt mode (mainly for Growl notices).
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(setq org-agenda-files (quote ("~/Dropbox/org/work.org.gpg"
                               "~/Dropbox/org/misc.org.gpg"
                               "~/Dropbox/org/phone-messages.org.gpg"
                               "~/Dropbox/org/personal.org"
                               "~/Dropbox/org/emp-hlth.org.gpg"
                               "~/Dropbox/org/caps.org.gpg"
                               )))

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/from-mobile.org")
(setq org-mobile-files (nconc '("~/Dropbox/org/work-notes.org.gpg"
                                "~/Dropbox/org/status-updates.org"
                                "~/Dropbox/org/notes.org")
                              org-agenda-files))
(setq org-mobile-force-id-on-agenda-items t)
(setq org-directory "~/Dropbox/org")
(setq org-publish-timestamp-directory "~/Dropbox/org/.org-timestamps/")
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-log-done 'note)
(setq org-log-into-drawer t)

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

;; 6.1 Capture Templates
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates '(("t" "todo" entry
  (file "refile.org")
  "* TODO %?\n  %u\n  %a")
 ("n" "note" entry
  (file "refile.org")
  "* %?                                        :NOTE:\n  %u\n  %a")
 ("p" "phone" entry
  (file+headline "phone-messages.org.gpg" "Phone Messages")
  "\n** PHONE %^{name} - %^{company|University of Rochester} -                :PHONE:\n  Contact Info: %^{phone}\n  %u\n\n  %?\n" 
  :clock-in t :clock-resume t)
 ("k" "ticket" entry
  (file+headline "work.org.gpg" "Request Tracker Tickets")
  "\n** TODO %^{Ticket}\n\n    %?\n" :clock-in t :clock-resume t)
 ("m" "misc" entry
  (file "misc.org.gpg")
  "\n* %^{event}\n\n   %?\n" :clock-in t :clock-resume t)
 ("M" "movie" table-line
  (file "movies.org")
  "| %<%Y-%m-%d> | %^{Title} | %^{Theatre} | %^{Rating|3} | %^{New} |"
  :kill-buffer t)))

(setq org-refile-targets (quote ((nil :maxlevel . 5)
                                 (org-agenda-files :maxlevel . 5)
                                 ("work-notes.org.gpg" :maxlevel . 5)
                                 )))
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
(setq org-export-latex-href-format "\\url{%s}")

(setq org-export-with-smart-quotes t)

;; 17.38 Remove Multiple State Change Log Details From The Agenda
(setq org-agenda-skip-additional-timestamps-same-entry t)

;; Automatic Org Mobile Pull/Push
;; http://permalink.gmane.org/gmane.emacs.orgmode/71467
;; (add-hook 'after-init-hook 'org-mobile-pull)
;; (add-hook 'kill-emacs-hook 'org-mobile-push) 

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; Load Org Babel mode for Graphviz.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t) (dot . t) (sql . t) (python . t))) ; this line activates dot

;; Minted -- better LaTeX source listings
(setq org-latex-pdf-process (quote ("latexmk -g -pdf %f")))
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; de-mysql-table
(defun mysql-to-org-table ()
  "Convert a MySQL result table (bounded in +---+) to be an
   Org table (bounded in |---|)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\( +\\)\\+---" nil t)
      (replace-match "\\1|---" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "---\\+$" nil t)
      (replace-match "---|" nil nil))))

;; Use MathJax from their CDN instead of the old version on the Org
;; servers.
(setq org-html-mathjax-options
      '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100")(align "center")(indent "2em")(mathml t)))
