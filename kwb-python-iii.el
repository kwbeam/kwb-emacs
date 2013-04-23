;; ***********************************************
;; Python II
;; ***********************************************

;; We're gonna need us a Python mode
(require 'python)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)

;; Run autopair when doing Python
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;; All the Python things live here
(setq virtualenv-root "~/.virtual_envs/")

;; Be able to run nose tests with various keybindings
(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))

;; Use the Python force, my young padawan learner
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

(provide 'kwb-python-iii)
