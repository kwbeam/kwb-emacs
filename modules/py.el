;;; py --- basic set of python
;;; Commentary:
;;; Code:

;; 1. Install [pyenv](https://github.com/pyenv/pyenv),
;;    [pyenv-installer](https://github.com/pyenv/pyenv-installer), and
;;    [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)
;;    to install and manage Python versions and virtualenvs.

;; 2. Create & setup the project:

;;     ```
;;     $ mkdir foo && cd foo
;;     $ pyenv virtualenv 3.7.3 foo
;;     $ pyenv activate foo
;;     $ pip install autopep8 flake8 ipython jedi ptvsd>=4.2
;;     ```

;; 3. Add a dir-local variable to activate the virtualenv. `M-x
;;    add-dir-local-variable`. For `python-mode` set `pyvenv-activate` to
;;    the name of the virtualenv.

;; Language servers:
;; Palantir: https://github.com/palantir/python-language-server
;; Microsoft Pyright: https://emacs-lsp.github.io/lsp-pyright/
;; Microsoft Old: https://emacs-lsp.github.io/lsp-python-ms/

;; Palantir
;; pip install ‘python-language-server[all]’

;; Microsoft New
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Microsoft Old
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))

(provide 'py)

;;; py.el ends here
