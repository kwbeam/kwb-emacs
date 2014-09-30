;; -----------------
;; Clojure
;; -----------------
(setq nrepl-hide-special-buffers t)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'enable-paredit-mode)
(add-hook 'cider-mode-hook 'run-dev-hook)

(provide 'dev-clojure)
