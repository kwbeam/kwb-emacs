;;; purescript --- basic set of purescript
;;; Commentary:
;;; Code:

;; -------------------------------------
;; PureScript
;; https://github.com/purescript-emacs/purescript-mode
;; https://github.com/purescript-emacs/psc-ide-emacs
(use-package purescript-mode
  :ensure t)

(use-package psc-ide
  :ensure t
  :hook (purescript-mode . (lambda ()
                             (psc-ide-mode)
                             (add-node-modules-path)
                             (setq psc-ide-use-npm-bin t)
                             (turn-on-purescript-indentation))))

(provide 'purescript)

;;; purescript.el ends here
