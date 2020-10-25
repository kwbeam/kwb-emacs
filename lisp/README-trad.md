  # JavaScript & TypeScript

TRAD:

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

  # Python

TRAD:

1. Install [pyenv](https://github.com/pyenv/pyenv),
    [pyenv-installer](https://github.com/pyenv/pyenv-installer), and
    [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)
    to install and manage Python versions and virtualenvs.
2. Create & setup the project:
    ```
     $ mkdir foo && cd foo
     $ pyenv virtualenv 3.7.3 foo
     $ pyenv activate foo
     $ pip install autopep8 flake8 ipython jedi
     ```
3. Add a dir-local variable to activate the virtualenv. `M-x
    add-dir-local-variable`. For `python-mode` set `pyvenv-activate` to
    the name of the virtualenv.
