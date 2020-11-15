;;; dev-lsp --- Dev modes setup -- lsp
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------
;; LSP - lsp-mode
;; ----------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024)
        lsp-pyls-plugins-pydocstyle-enabled t
        lsp-pyls-plugins-yapf-enabled t
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil
        lsp-idle-delay 0.500))
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-include-signature nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(use-package dap-mode
  :ensure t
  :config
  (setq dap-python-debugger 'debugpy)
  (require 'dap-python)
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-node))

;; ----------------------------------------------------------
;; Haskell
;; ----------------------------------------------------------
(use-package haskell-mode
  :ensure t)

;; ----------------------------------------------------------
;; JavaScript & TypeScript
;; ----------------------------------------------------------
;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :hook (js-mode typescript-mode))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t)

(provide 'dev-lsp)

;;; dev-lsp.el ends here
