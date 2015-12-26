;;; dev-haskell --- kwbeam Haskell setup
;;; Commentary:

;; Setup Emacs for Haskell

;;; Code:

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(provide 'dev-haskell)

;;; dev-haskell.el ends here
