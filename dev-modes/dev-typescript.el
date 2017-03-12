;;; dev-typescript --- kwbeam TypeScript setup
;;; Commentary:

;; sample config - from tide README

;;; Code:

(add-hook 'typescript-mode-hook 'run-dev-hook)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook 'run-dev-hook)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; Use Tide for JS and JSX too
(add-hook 'js2-mode-hook #'setup-tide-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; Finally, let's use the local tsserver
;;(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

(provide 'dev-typescript)

;;; dev-typescript.el ends here
