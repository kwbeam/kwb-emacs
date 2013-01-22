;; ***********************************************
;; Python I
;; ***********************************************

;; We're gonna need us a Python mode
(require 'python-mode)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)

;; Move around inside of names
(eval-after-load 'python-mode
  '(progn
     (define-key python-mode-map (kbd "M-f") 'py-forward-into-nomenclature)
     (define-key python-mode-map (kbd "M-b") 'py-backward-into-nomenclature)))

;; flymake & flake8
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
(require 'flymake-cursor)

;; Activate ropemacs
(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil))
(global-set-key "\C-xpl" 'load-ropemacs)

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

(provide 'kwb-python-i)
