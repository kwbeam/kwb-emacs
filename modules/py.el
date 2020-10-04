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
;;     $ pip install autopep8 flake8 ipython jedi
;;     ```

;; 3. Add a dir-local variable to activate the virtualenv. `M-x
;;    add-dir-local-variable`. For `python-mode` set `pyvenv-activate` to
;;    the name of the virtualenv.

;; -------------------------------------
;; Python
(use-package elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

; (use-package blacken
;   :ensure t
;   :pin melpa
;   :hook (python-mode . blacken-mode))

;; (use-package company-jedi
;;   :ensure t
;;   :pin melpa-stable
;;   :hook (python-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))

;; http://millejoh.github.io/emacs-ipython-notebook/
;(use-package ein
;  :ensure t
;  :pin melpa
;  :config
;  (require 'ein-notebook)
;  (require 'ein-subpackages)
;  (setq ein:enable-keepalive t)
;  (setq ein:notebooklist-keepalive-refresh-time 0.25)
;  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;  ;;(setq ein:completion-backend 'ein:use-company-backend)
;  )

(provide 'py)

;;; py.el ends here
