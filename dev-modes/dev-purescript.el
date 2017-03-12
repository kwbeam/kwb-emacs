;;; dev-purescript --- kwbeam PureScript setup
;;; Commentary:

;; Setup Emacs for PureScript

;;; Code:

(setq psc-ide-use-npm-bin t)

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

(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (turn-on-purescript-indentation)
            (company-mode)
            (company-complete)))

(provide 'dev-purescript)

;;; dev-purescript.el ends here
