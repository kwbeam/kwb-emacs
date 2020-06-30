;;; javascript --- basic set of javascript
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

;; 4. To open a REPL, `M-x indium-run-node`.

;; 5. (Optional) Create and setup an ES.next project:

;;     ```
;;     $ # ES5 steps above, and:
;;     $ npm install -D babel-cli babel-preset-env
;;     $ echo "{'presets': ['env']}" > .babelrc
;;     $ npm install -S babel-polyfill
;;     ```

;; 6. To open a REPL:

;;     ```
;;     $ npx babel-node --inspect
;;     ```

;; Connect to it in Emacs with `M-x indium-connect-to-nodejs` and specify
;; the IP address, port, and URL path on which the babel-node process is
;; listening.

;; -------------------------------------
;; JavaScript [Indium]
;; https://github.com/mooz/js2-mode/
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :defer t
  :after (javascript-mode js2-mode purescript-mode typescript-mode)
  :init
  (add-hook 'js2-mode-hook #'add-node-modules-path))

;; https://github.com/NicolasPetton/indium
;; https://indium.readthedocs.io/en/latest/index.html
;; (use-package indium
;;   :ensure t
;;   :pin melpa-stable
;;   :defer t
;;   :hook (js2-mode . indium-interaction-mode))

(provide 'javascript)

;;; javascript.el ends here
