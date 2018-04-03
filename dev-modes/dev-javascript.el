;;; dev-javascript --- Setup emacs for JavaScript
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisite Emacs packages:
;;   * add-node-modules-path
;;   * company
;;   * company-tern
;;   * exec-path-from-shell
;;   * flycheck
;;   * js2-mode
;;   * nodejs-repl
;;   * tern
;; Prerequisite JS stuff:
;;   * ES5:
;;     * nvm
;;     * nvm install --lts
;;     * mkdir foo && cd foo
;;     * npm init -y
;;     * npm install --save-dev eslint tern
;;     * npx eslint --init
;;   * ESnext:
;;     * ES5, and
;;     * npm install --save-dev babel-cli babel-preset-env
;;     * echo "{'presets': ['env']}" > .babelrc
;;     * npm install --save babel-polyfill
;;   * React:
;;     * ES5 or ES6, and
;;     * npm install eslint-plugin-react --save-dev
;;     * npm install babel-preset-react --save-dev
;; Notes:
;;   When using babeljs for ES6, set a dir local variable so you
;;   can use the nodejs-repl via C-c C-z or M-x nodejs-repl.  The
;;   project's .dir-locals.el file should be at the top of the
;;   project and contain something like this:
;;     ((js2-mode (nodejs-repl-command
;;                 .
;;                 "/home/kwbeam/labs/js-lab/node_modules/.bin/babel-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * set nodejs-repl-command by finding it in the project's directory tree!
;;; Code:

(require 'company)

(add-to-list 'projectile-project-root-files "package.json")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'run-dev-hook)

;; Use tern for completions, etc.
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'nodejs-repl-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)

;; Setup key mappings for nodejs-repl.
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
                            (local-set-key "\C-cr" 'nodejs-repl-send-region)
                            (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
                            (local-set-key "\C-cl" 'nodejs-repl-load-file)
                            (local-set-key "\C-c\C-z" 'nodejs-repl-switch-to-repl)))

;; Make sure the project's node_modules is always on the path (for
;; eslint, tern, and possibly other things).
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'projectile-after-switch-project-hook #'add-node-modules-path)

(setq-default js2-basic-offset 2)

(provide 'dev-javascript)
;;; dev-javascript.el ends here
