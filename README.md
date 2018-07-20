# My Emacs Configuration

In the beginning...

"Emacs outshines all other editing software in approximately the
same way that the noonday sun does the stars.  It is not just bigger
and brighter; it simply makes everything else vanish."
-Neal Stephenson, "In the Beginning was the Command Line"

# Development Notes

## Haskell

TODO

## JavaScript

Packages used:

* [js2-mode](https://github.com/mooz/js2-mode/)
* [add-node-modules-path](https://github.com/codesuki/add-node-modules-path)
* [indium](https://indium.readthedocs.io/en/latest/index.html)

1. Install [nvm](https://github.com/creationix/nvm) to install and manage NodeJS versions.

2. Install and use a version of Node:

    ```
    $ nvm install <version>
    $ nvm alias default <version>
    $ nvm use default

3. Create and setup an ES5 project:

    ```
    $ mkdir foo && cd foo
    $ npm init -y
    $ npm install -D eslint
    $ npx eslint --init
    ```

3a. To open a REPL, `M-x indium-run-node`.

4. (Optional) Create and setup an ES.next project:

    ```
    $ # ES5 steps above, and:
    $ npm install -D babel-cli babel-preset-env
    $ echo "{'presets': ['env']}" > .babelrc
    $ npm install -S babel-polyfill
    ```

4a. To open a REPL:

Start a Babel NodeJS process:

    ```
    $ npx babel-node --inspect
    ```

Connect to it in Emacs with `M-x indium-connect-to-nodejs` and specify
the IP address, port, and URL path on which the babel-node process is
listening.

## Lisp

TODO

## Python

Packages used:

* python (*built-in*)
* [elpy](https://elpy.readthedocs.io/en/latest/index.html)
* [pipenv](https://github.com/pwalsh/pipenv.el)
* [company-jedi](https://github.com/syohex/emacs-company-jedi)
* [ein](http://millejoh.github.io/emacs-ipython-notebook/)

1. Install [pyenv](https://github.com/pyenv/pyenv) to manage Python installs.

2. Install a Python:

    ```
    $ pyenv install <python version>
    ```

3. Create & setup the project:

    ```
    $ mkdir foo && cd foo
    $ pyenv local <python version>
    $ pipenv install jedi flake8
    ```

## Scheme

TODO

## TypeScript

Packages used:

* [typescript-mode](https://github.com/ananthakumaran/typescript.el)
* [tide](https://github.com/ananthakumaran/tide)
* [web-mode](http://web-mode.org/)
* [ts-comint](https://github.com/josteink/ts-comint)

1. Install [nvm](https://github.com/creationix/nvm) to install and manage NodeJS versions.

2. Install and use a version of Node:

    ```
    $ nvm install <version>
    $ nvm alias default <version>
    $ nvm use default

3. Create and setup a TypeScript project:

    ```
    $ mkdir foo && cd foo
    $ npm init -y
    $ npm install -D typescript ts-lint ts-node @types/node
    $ npx tsc --init
    ```

;; Notes:
;;   When using a repl via ts-comint's M-x run-ts, set a dir local
;;   variable to point to the correct ts-node executable.  The project's
;;   .dir-locals.el file should be at the top of the project and
;;   contain something like this:
((typescript-mode (ts-comint-program-command
                   .
                   "/home/kwbeam/projects/mvpreact/node_modules/.bin/ts-node"))
 (web-mode (ts-comint-program-command
            .
            "/home/kwbeam/projects/mvpreact/node_modules/.bin/ts-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * Set the ts-comint-program-command by finding it in the
;;       project's directory tree!



## Web (HTML, CSS)

;; Web
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-enable-auto-pairing t)
;;   (setq web-mode-enable-auto-pairing nil)
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
;; (add-hook 'web-mode-hook 'run-dev-hook)

;; (defun sp-web-mode-is-code-context (id action context)
;;   (and (eq action 'insert)
;;        (not (or (get-text-property (point) 'part-side)
;;                 (get-text-property (point) 'block-side)))))
;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
;; Use eslint
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
