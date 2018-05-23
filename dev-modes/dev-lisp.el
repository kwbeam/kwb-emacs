;;; dev-lisp --- Setup emacs for uncommonly-great Common Lisp
;;; Commentary:
;; -----------------
;; Lisp
;; -----------------
;; Prerequisite Emacs packages:
;;   * slime
;;   * slime-company
;; Prerequisite Language installs:
;;   * SBCL
;;; Code:

(require 'slime)
(require 'slime-company)

(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (clisp ("clisp"))))

(setq slime-contribs '(slime-fancy))

(provide 'dev-lisp)

;;; dev-lisp.el ends here
