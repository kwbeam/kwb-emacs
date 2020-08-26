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
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-idle-delay 0.500
        lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(use-package dap-mode
  :ensure t)
  ;; :config
  ;; (require 'dap-firefox)
  ;; (require 'dap-node)
;; (require 'dap-python))

(use-package dap-firefox)
(use-package dap-node)
(use-package dap-python)

(provide 'lsp)

;;; lsp.el ends here
