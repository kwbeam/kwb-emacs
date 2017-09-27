;;; dev-scheme --- Setup emacs for Scheming!
;;; Commentary:
;; -----------------
;; Scheme
;; -----------------
;; Prerequisite Emacs packages:
;;   * ac-geiser
;;   * geiser
;; Prerequisite Scheme installs:
;;   * Racket 6.0 or better
;;   * Guile 2.0.9 or better
;;   * MIT/GNU Scheme 9.1.1 or better
;;; Code:

(require 'geiser-impl)

(setq geiser-active-implementations '(guile mit racket))

(provide 'dev-scheme)

;;; dev-scheme.el ends here
