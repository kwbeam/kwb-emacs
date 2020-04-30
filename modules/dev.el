;;; dev --- basic set of dev
;;; Commentary:
;;; Code:

;; -------------------------------------
;; Common dev mode setup
;; -------------------------------------

(defun kwb-dev-hook ()
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (smartparens-mode))
(mapc
 (lambda (hook) (add-hook hook 'kwb-dev-hook))
 '(emacs-lisp-mode-hook
   js2-mode-hook
   python-mode-hook))

(provide 'dev)

;;; dev.el ends here
