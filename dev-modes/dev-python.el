;;; dev-python --- Setup emacs for Python wrangling
;;; Commentary:
;; -----------------
;; Python
;; -----------------
;; Prerequisite Emacs packages:
;;   * company
;;   * company-jedi (not jedi!)
;;   * ein
;;   * elpy
;;   * flycheck
;;   * nose
;;   * pipenv.el
;; Prerequisite language installs:
;;   * pyenv
;;   * pyenv install <python version>
;;   * pyenv (global|local) <python version>
;;   * pip install pipenv
;; Project setup:
;;   $ pipenv install jedi flake8
;;; Code:

(require 'company)
(require 'nose)
(require 'python)
(require 'pyvenv)

(elpy-enable)

(add-to-list 'projectile-project-root-files "Pipfile")

(defun kwb/python-mode-hook ()
  "Setup all my Python stuff when we enter python mode."
  (pipenv-mode)
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  (pyvenv-mode -1)
  (smartparens-mode)
  (add-to-list 'company-backends 'company-jedi)
  (setq python-indent-guess-indent-offset-verbose nil)
  (local-set-key "\C-ca" 'nosetests-all)
  (local-set-key "\C-cm" 'nosetests-module)
  (local-set-key "\C-c." 'nosetests-one)
  (local-set-key "\C-cpa" 'nosetests-pdb-all)
  (local-set-key "\C-cpm" 'nosetests-pdb-module)
  (local-set-key "\C-cp." 'nosetests-pdb-one))

(add-hook 'python-mode-hook 'kwb/python-mode-hook)

(add-hook 'python-mode-hook 'run-dev-hook)

;; don't use flymake (elpy default), use flycheck
;; from: https://github.com/jorgenschaefer/elpy/issues/137
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ein - emacs ipython notebook
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(add-to-list 'company-backends 'ein:company-backend)

(provide 'dev-python)

;;; dev-python.el ends here
