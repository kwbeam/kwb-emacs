;;; dev-purescript --- Setup emacs for Purescript hackage
;;; Commentary:
;; -----------------
;; Purescript
;; -----------------
;; Prerequisite Emacs packages:
;;   * purescript-mode
;;   * psc-ide
;;   * repl-toggle
;;   * psci
;; Prerequisite Language installs:
;;   * nvm
;;   * nvm install --lts
;; Starting a project:
;;   * mkdir foo && cd foo
;;   * npm init -y
;;   * npm install -S purescript pulp bower
;;   * npx pulp init
;;; Code:

(require 'purescript-mode)
(require 'psc-ide)
(require 'repl-toggle)
(require 'psci)

(setq psc-ide-use-npm-bin t)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)
    (turn-on-purescript-decl-scan)
    (inferior-psci-mode)))
(add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

(eval-after-load "purescript-mode"
  '(progn
     (define-key purescript-mode-map (kbd "C-,") 'purescript-move-nested-left)
     (define-key purescript-mode-map (kbd "C-.") 'purescript-move-nested-right)))

(provide 'dev-purescript)

;;; dev-purescript.el ends here
