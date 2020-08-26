;;; js-ts --- basic set of javascript
;;; Commentary:
;;; Code:

;; 1. Install [nvm](https://github.com/creationix/nvm) to install and manage NodeJS versions.

;; 2. Install and use a version of Node:

;;     ```
;;     $ nvm install <version>
;;     $ nvm alias default <version>
;;     $ nvm use default
;;     ```

;; 3. Create and setup an ES5 project:

;;     ```
;;     $ mkdir foo && cd foo
;;     $ npm init -y
;;     $ npm install -D eslint
;;     $ npx eslint --init
;;     ```

;; 4. (Optional) Create and setup an ES.next project:

;;     ```
;;     $ # ES5 steps above, and:
;;     $ npm install -D babel-cli babel-preset-env
;;     $ echo "{'presets': ['env']}" > .babelrc
;;     $ npm install -S babel-polyfill
;;     ```
;; Language Servers
;; Theia-ide (Preferred): https://github.com/theia-ide/typescript-language-server
;;   `$ npm i -D typescript-language-server`
;; Sourcegraph: https://github.com/sourcegraph/javascript-typescript-langserver
;;   `$ npm i -D javascript-typescript-langserver`

;; https://github.com/mooz/js2-mode/
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode ("\\.js\\'" . js2-mode))
  ;; :config
  ;; (setq-default js-indent-level 2)
  ;; (setq-default js2-basic-offset 2))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t)

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :defer t
  :after (javascript-mode js2-mode purescript-mode typescript-mode)
  :init
  (add-hook 'js2-mode-hook #'add-node-modules-path))

(defun nodejs-repl-config ()
  (define-key typescript-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
  (define-key typescript-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
  (define-key typescript-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key typescript-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
  (define-key typescript-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key typescript-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

(use-package nodejs-repl
  :ensure t
  :hook ((typescript-mode) . nodejs-repl-config)
  :init
  (setq nodejs-repl-command "npx ts-node"))

(provide 'js-ts)

;;; js-ts.el ends here
