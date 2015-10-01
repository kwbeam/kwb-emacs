;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisites:
;;   $ sudo npm install -g babel eslint babel-eslint eslint-plugin-react tern
;; See:
;;   http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;;; Code:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
;;(add-hook 'js2-mode-hook 'skewer-mode)

(setq-default js2-basic-offset 4)

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-j")

;; use node.js with babel as a repl
(setq inferior-js-program-command "babel-node")
(setenv "NODE_NO_READLINE" "1")
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))

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
;;; dev-javascript.el ends here
