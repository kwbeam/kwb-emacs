;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisites:
;; ES5:
;;   * nvm: node.js
;;   * npm install eslint --save-dev
;; ES6:
;;   * npm install babel-cli babel-preset-es2015 --save-dev
;; React:
;;   * npm install eslint-plugin-react --save-dev
;;   * npm install babel-preset-react --save-dev
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-hook 'js2-mode-hook 'run-dev-hook)

(setq-default js2-basic-offset 2)

(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
                            (local-set-key "\C-cr" 'nodejs-repl-send-region)
                            (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
                            (local-set-key "\C-cl" 'nodejs-repl-load-file)
                            (local-set-key "\C-c\C-z" 'nodejs-repl-switch-to-repl)))

;; Use the locally-installed eslint for flycheck so we aren't forced
;; to install it globally. See:
;; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun kwb/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint
               (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'js2-mode-hook #'kwb/use-eslint-from-node-modules)
(add-hook 'projectile-after-switch-project-hook 'kwb/use-eslint-from-node-modules)

;; Use babel repl for es6
;; TODO: generify this function and use to set eslint and babel vars
;; (defun kwb/use-local-babel-node ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (babel-node (and root
;;                           (expand-file-name "node_modules/babel-cli/bin/babel-node.js"
;;                                             root))))
;;     (when (and babel-node
;;                (file-executable-p babel-node))
;;       (setq babel-repl-cli-program babel-node))))
;; (add-hook 'js2-mode-hook #'kwb/use-local-babel-node)
;; (add-hook 'projectile-after-switch-project-hook #'kwb/use-local-babel-node)

;; Cleanup the node and babel repl output
;; (require 'comint)
;; (add-to-list 'comint-preoutput-filter-functions
;;              (lambda (output)
;;                (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))

(provide 'dev-javascript)
;;; dev-javascript.el ends here
