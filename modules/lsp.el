;;; lsp --- basic set of lsp
;;; Commentary:
;;; Code:

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :hook ((js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (python-mode . lsp-deferred))
         ;; (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
;; (use-package dap-mode
;;   :ensure t)
;; (use-package dap-javascript
;;   :ensure t)
;; (use-package which-key
;;   :config
;;   (which-key-mode))

(provide 'lsp)

;;; lsp.el ends here
