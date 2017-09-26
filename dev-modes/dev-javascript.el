;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisites:
;; ES5:
;;   * nvm: node.js
;;   * npm install eslint tern --save-dev
;; ES6:
;;   * npm install babel-cli babel-preset-es2015 --save-dev
;; React:
;;   * npm install eslint-plugin-react --save-dev
;;   * npm install babel-preset-react --save-dev
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)

(add-hook 'js2-mode-hook 'run-dev-hook)

(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
                            (local-set-key "\C-cr" 'nodejs-repl-send-region)
                            (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
                            (local-set-key "\C-cl" 'nodejs-repl-load-file)
                            (local-set-key "\C-c\C-z" 'nodejs-repl-switch-to-repl)))

;; make sure the project's node_modules is always on the path
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'projectile-after-switch-project-hook #'add-node-modules-path)

(setq-default js2-basic-offset 2)

;; Cleanup the node and babel repl output
(require 'comint)
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))

(provide 'dev-javascript)
;;; dev-javascript.el ends here
