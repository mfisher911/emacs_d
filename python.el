;;; python.el -- local customizations related to Python
;;; Commentary:
;;;   Python language customizations.
;;; Code:
;; python stuff from http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(add-hook 'python-mode-hook
          #'(lambda ()
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             (setq show-trailing-whitespace t)))

(defun py-next-block ()
  "Go to the next block.  Cf. `forward-sexp' for 'lisp-mode'."
  (interactive)
  (py-mark-block nil 't)
  (back-to-indentation))

(provide 'python)
;;; python.el ends here
