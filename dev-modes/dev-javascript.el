;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisites:
;;   * nvm: node.js
;;   * eslint
;;   * babel
;;
;;   $ npm install -g eslint eslint-plugin-react
;;   $ npm install --save-dev babel-cli babel-preset-es2015 babel-preset-react
;;
;; See:
;;   http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
(setq-default js2-basic-offset 4)
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
                            (local-set-key "\C-cr" 'nodejs-repl-send-region)
                            (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
                            (local-set-key "\C-cl" 'nodejs-repl-load-file)
                            (local-set-key "\C-c\C-z" 'nodejs-repl-switch-to-repl)))

(provide 'dev-javascript)
;;; dev-javascript.el ends here
