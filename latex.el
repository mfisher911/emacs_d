;;; latex.el -- local customizations related to LaTeX
;;; Commentary:
;;;   LaTeX customizations.
;;; Code:

;; LaTeX additions
(add-hook 'latex-mode-hook
          (function (lambda () (setq sentence-end-double-space nil))))

;;
;; TeXcount setup for TeXcount version 2.3
;;
(defun texcount-setup ()
  "Make a container function for doing LaTeX word counting."
  (defun latex-word-count ()
    (interactive)
    (let*
      ( (this-file (buffer-file-name))
        (enc-str (symbol-name buffer-file-coding-system))
        (enc-opt
          (cond
            ((string-match "utf-8" enc-str) "-utf8")
            ((string-match "latin" enc-str) "-latin1")
            ("-encoding=guess")
        ) )
        (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "/usr/texbin/texcount"
                            nil t nil "-0" enc-opt this-file)
      ) ) ) )
      (message word-count)
  ) )
  (define-key latex-mode-map (kbd "C-c C-w") 'latex-word-count)
)
(add-hook 'latex-mode-hook 'texcount-setup t)

(provide 'latex)
;;; latex.el ends here
