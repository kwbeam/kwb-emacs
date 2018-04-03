;;; dev-haskell --- Setup emacs for Haskell hackage
;;; Commentary:
;; -----------------
;; Haskell
;; -----------------
;; Prerequisite Emacs packages:
;;   * intero
;; Prerequisite Language installs:
;;   * Stack / GHC
;;; Code:

(add-hook 'haskell-mode-hook 'intero-mode)

(provide 'dev-haskell)

;;; dev-haskell.el ends here
