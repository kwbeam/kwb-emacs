;; -----------------
;; Python
;; -----------------
(require 'python)
(add-hook 'python-mode-hook 'run-dev-hook)

(elpy-enable)

(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))

(provide 'dev-python)
