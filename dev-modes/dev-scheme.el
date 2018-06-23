;;; dev-scheme --- Setup emacs for Scheming!
;;; Commentary:
;; -----------------
;; Scheme
;; -----------------
;; Prerequisite Emacs packages:
;;   * geiser
;; Prerequisite Scheme installs:
;;   * MIT/GNU Scheme 9.1.1 or better
;;   * Racket 6.0 or better
;;; Code:

(require 'geiser-impl)

(setq geiser-active-implementations '(mit racket))
(add-hook 'scheme-mode-hook 'run-dev-hook)

(provide 'dev-scheme)

;;; dev-scheme.el ends here
