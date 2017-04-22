;;; dev-purescript --- kwbeam PureScript setup
;;; Commentary:

;; Setup Emacs for PureScript

;;; Code:

;; This will change when I start using PureScript >= 0.11.0
(setq psc-ide-use-purs nil)

(setq psc-ide-use-npm-bin t)

;; Necessary for M-x psci
(defun kwb/use-local-psci ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (psci (and root
                    (expand-file-name "node_modules/.bin/psci"
                                      root))))
    (when (and psci
               (file-executable-p psci))
      (setq psci/file-path psci))))
(add-hook 'purescript-mode-hook #'kwb/use-local-psci)

;; Allows C-c C-z to toggle between buffer and repl
(require 'repl-toggle)
(add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

;; Setup flycheck
;; This doesn't work b/c it assumes psc is installed globally (!)
;; (eval-after-load 'flycheck
;;  '(flycheck-purescript-setup))

(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (turn-on-purescript-indentation)
            (company-mode)
            (company-complete)))

(provide 'dev-purescript)

;;; dev-purescript.el ends here
