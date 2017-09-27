;;; dev-lisp --- Setup emacs for uncommonly-great Common Lisp
;;; Commentary:
;; -----------------
;; Lisp
;; -----------------
;; Prerequisite Emacs packages:
;;   * slime
;; Prerequisite Language installs:
;;   * SBCL
;;; Code:

(setq inferior-lisp-program "sbcl")

(setq slime-contribs '(slime-fancy))

(provide 'dev-lisp)

;;; dev-lisp.el ends here
