# My Emacs Configuration

*In the beginning...*

> "Emacs outshines all other editing software in approximately the
> same way that the noonday sun does the stars.  It is not just bigger
> and brighter; it simply makes everything else vanish."
>
> -Neal Stephenson, "In the Beginning was the Command Line"

# Base Packages

* [Theme](https://github.com/purcell/color-theme-sanityinc-tomorrow)
* [Multiple Cursors](https://github.com/magnars/multiple-cursors.el)
* [Smartparens](https://github.com/Fuco1/smartparens)
* [Git Timemachine](https://gitlab.com/pidu/git-timemachine)
* [Magit](https://magit.vc/)
* [Company](https://company-mode.github.io/)
* [Flycheck](http://www.flycheck.org/en/latest/)
* [Markdown](https://github.com/jrblevin/markdown-mode/tree/b6de08a0f8517509ca2a08b0f9351c63eed4737d)
* [Org](https://orgmode.org/)

# Programming Language Notes

## Elm

Packages used:

* [elm-mode](https://github.com/jcollard/elm-mode)

## Haskell

Packages used:

* [haskell-mode](https://github.com/haskell/haskell-mode)
* [intero](https://commercialhaskell.github.io/intero/)

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
    ```

3. Create and setup an ES5 project:

    ```
    $ mkdir foo && cd foo
    $ npm init -y
    $ npm install -D eslint
    $ npx eslint --init
    ```

4. To open a REPL, `M-x indium-run-node`.

5. (Optional) Create and setup an ES.next project:

    ```
    $ # ES5 steps above, and:
    $ npm install -D babel-cli babel-preset-env
    $ echo "{'presets': ['env']}" > .babelrc
    $ npm install -S babel-polyfill
    ```

6. To open a REPL:

    ```
    $ npx babel-node --inspect
    ```

Connect to it in Emacs with `M-x indium-connect-to-nodejs` and specify
the IP address, port, and URL path on which the babel-node process is
listening.

## Julia

* [julia-mode](https://github.com/JuliaEditorSupport/julia-emacs)
* [julia-repl](https://github.com/tpapp/julia-repl)

## Lisp

Packages used:

* [lisp](*built-in*)
* [slime](https://common-lisp.net/project/slime/)
* [slime-company](https://github.com/anwyn/slime-company)

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

Packages used:

* [scheme](*built-in*)
* [geiser](http://www.nongnu.org/geiser/)

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
    ```

3. Create and setup a TypeScript project:

    ```
    $ mkdir foo && cd foo
    $ npm init -y
    $ npm install -D typescript ts-lint ts-node @types/node
    $ npx tsc --init
    ```

4. To open a REPL, first add a `.dir-locals.el` file at the root of
the project with:

    ```
    ((typescript-mode (ts-comint-program-command .
                       "<project_dir>/node_modules/.bin/ts-node"))
     (web-mode (ts-comint-program-command .
                "<project_dir>/node_modules/.bin/ts-node")))
    ```

Then start the TypeScript REPL with `M-x run-ts`.
