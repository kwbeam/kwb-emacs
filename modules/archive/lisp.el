;;; lisp --- basic set of lisp
;;; Commentary:
;;; Code:

;; -------------------------------------
;; Lisp
;; https://common-lisp.net/project/slime/
;; https://github.com/emacsmirror/slime-company
(use-package slime
  :ensure t
  :pin melpa-stable
  :after lisp-mode
  :defer t
  :init
  (setq slime-lisp-implementations
      '((sbcl ("sbcl"))))
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :pin melpa-stable
  :defer t
  :after (company slime))

(provide 'lisp)

;;; lisp.el ends here
