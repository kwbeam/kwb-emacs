;;; scheme --- basic set of scheme
;;; Commentary:
;;; Code:

;; -------------------------------------
;; Scheme
(use-package geiser
  :ensure t
  :pin melpa-stable
  :defer t
  :after (scheme)
  :init
  (setq geiser-active-implementations '(mit guile)))

(provide 'scheme)

;;; scheme.el ends here
