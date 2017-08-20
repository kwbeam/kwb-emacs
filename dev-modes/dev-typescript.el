;;; dev-typescript --- kwbeam TypeScript setup
;;; Commentary:

;; sample config - from tide README

;;; Code:



(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)


(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'run-dev-hook)

;; Tide can be used along with web-mode to edit tsx files
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook 'run-dev-hook)
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; Use Tide for JS
;; (add-hook 'js2-mode-hook #'setup-tide-mode)

;; Use Tide for JSX
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; Finally, let's use the local tsserver
(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

(provide 'dev-typescript)

;;; dev-typescript.el ends here
