;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisites:
;;   * nvm: node.js
;;   * tern
;;   * eslint
;;   * babel
;;
;;   $ npm install -g tern
;;   $ npm install -g eslint eslint-plugin-react
;;   $ npm install --save-dev babel-cli babel-preset-es2015 babel-preset-react
;;
;; See:
;;   http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(setq-default js2-basic-offset 4)

(provide 'dev-javascript)
;;; dev-javascript.el ends here
