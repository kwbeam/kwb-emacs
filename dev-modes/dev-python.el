;;; dev-python --- Setup emacs for Python wrangling
;;; Commentary:
;; -----------------
;; Python
;; -----------------
;; Prerequisite Emacs packages:
;;   * autopair
;;   * company
;;   * company-jedi (not jedi!)
;;   * ein
;;   * elpy (installs pyvenv)
;;   * flycheck
;;   * nose
;; Prerequisite language installs:
;;   * miniconda3
;; Notes:
;;   Project setup:
;;     * With no project, create a 'default' conda env and install jedi and flake8
;;     * With any specific project, create a conda env for it and make sure
;;       jedi and flake8 are installed in it.  Then M-x pyvenv-workon <envname>
;;       to make sure Emacs is using its environment.
;;     * To automate this, create a .dir-locals.el for the project:
;;         ((nil . ((pyvenv-workon . "environmentname"))))
;;; Code:

(require 'autopair)
(require 'company)
(require 'nose)
(require 'python)
(require 'pyvenv)

(elpy-enable)

(defun kwb/python-mode-hook ()
  "Setup all my Python stuff when we enter python mode."
  (autopair-mode)
  (setenv "WORKON_HOME" "/home/kwbeam/miniconda3/envs")
  (setq pyvenv-workon "default")
  (setq pyvenv-tracking-mode 't)
  (add-to-list 'company-backends 'company-jedi)
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
