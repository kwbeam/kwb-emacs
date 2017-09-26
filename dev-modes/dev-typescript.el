;;; dev-typescript --- kwbeam TypeScript setup
;;; Commentary:

;; sample config - from tide README

;;; Code:

(require 'tide)

;; Basic setup for TS files
(defun setup-tide-mode ()
  "Setup tide mode for TS files."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)
(setq tide-tsserver-executable "node_modules/.bin/tsserver")
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'run-dev-hook)

;; TSX: Tide & web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook 'run-dev-hook)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

(defun kwb-use-tide-for-js ()
  "Setup things so that tide is used for JS and JSX files."
  (interactive)

  ;; JS
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  ;; JSX
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(provide 'dev-typescript)

;;; dev-typescript.el ends here
