;;; lsp --- basic setup for the Language Server Protocol
;;; Commentary:
;;; Code:

;; set prefix for lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :hook ((js2-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp)
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-idle-delay 0.500))

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-firefox)
  (require 'dap-node)
  (require 'dap-python))

(provide 'lsp)

;;; lsp.el ends here
