;;; dev-clojure --- Setup emacs for Clojure
;;; Commentary:
;; -----------------
;; Clojure
;; -----------------
;;; Code:

(require 'smartparens-config)
(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
(sp-use-paredit-bindings)

(provide 'dev-clojure)
;;; dev-clojure.el ends here
