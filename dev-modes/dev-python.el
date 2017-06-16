;; -----------------
;; Python
;; -----------------
(require 'python)
(add-hook 'python-mode-hook 'run-dev-hook)

(elpy-enable)

(setenv "WORKON_HOME" "/home/kwbeam/miniconda3/envs")
(setq pyvenv-activate (expand-file-name "/home/kwbeam/miniconda3/envs/default"))
(setq pyvenv-tracking-mode 't)
;; project setup:
;; * install jedi and flake8
;; * in .dir-locals.el: ((nil . ((pyvenv-workon . "environmentname"))))

;; don't use flymake (elpy default), use flycheck
;; from: https://github.com/jorgenschaefer/elpy/issues/137
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook #'(lambda ()
                                (autopair-mode)
                                (elpy-use-ipython)))

(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))

;; Emacs IPython Notebook mode config
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(add-hook 'company-backends 'ein:company-backend)

(provide 'dev-python)
