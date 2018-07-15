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
      '((sbcl ("clisp"))
        (clisp ("sbcl"))))

(setq slime-contribs '(slime-fancy))
(add-hook 'lisp-mode-hook 'run-dev-hook)

(provide 'dev-lisp)

;;; dev-lisp.el ends here
