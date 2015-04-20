;; -----------------
;; JavaScript
;; -----------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
(add-hook 'js2-mode-hook 'skewer-mode)

(setq-default js2-basic-offset 4)

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-j")

;; use node.js as a repl
(setq inferior-js-program-command "node")
(setenv "NODE_NO_READLINE" "1")

(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;; Add tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(provide 'dev-javascript)