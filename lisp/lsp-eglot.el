;;; lsp-eglot --- basic setup for the Language Server Protocol
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t
  :hook ((js-mode typescript-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

(provide 'lsp-eglot)

;;; lsp-eglot.el ends here
