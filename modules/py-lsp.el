;;; py --- basic setup for python development
;;; Commentary:
;;; Code:

;; 1. Install [pyenv](https://github.com/pyenv/pyenv),
;;    [pyenv-installer](https://github.com/pyenv/pyenv-installer), and
;;    [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)
;;    to install and manage Python versions and virtualenvs.

;; 2. Create & setup the project:

;;      $ mkdir foo && cd foo
;;      $ pyenv virtualenv 3.7.3 foo
;;      $ pyenv activate foo
;;      $ pip install autopep8 flake8 ipython jedi "ptvsd>=4.2"

;; 3. Install the Palantir Python Language Server:
;;
;;      $ pip install python-language-server
;;
;;    and optionally:
;;
;;      $ pip install pyls-mypy
;;      $ pip install pyls-isort
;;      $ pip install pyls-black

;; 4. Add a dir-local variable to activate the virtualenv. `M-x
;;    add-dir-local-variable`. For `python-mode` set `pyvenv-activate` to
;;    the name of the virtualenv. E.g., in the root of your project,
;;    there will be a .dir-locals.el with:
;;
;;      ((python-mode
;;        (pyvenv-activate . "/home/kbeam/.pyenv/versions/py-lab")))


(provide 'py)

;;; py.el ends here
