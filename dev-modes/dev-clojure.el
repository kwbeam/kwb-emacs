;; -----------------
;; Clojure
;; -----------------
;; clojure-mode
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'run-dev-hook)

;; cider
(setq nrepl-hide-special-buffers t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-test-show-report-on-success t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-mode-hook 'run-dev-hook)

(provide 'dev-clojure)