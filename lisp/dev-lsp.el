;;; dev-lsp --- Dev modes setup -- lsp
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------
;; LSP - lsp-mode
;; ----------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
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
  (require 'dap-python))
  ;; (require 'dap-firefox)
  ;; (require 'dap-node)

;; ----------------------------------------------------------
;; JavaScript & TypeScript
;; ----------------------------------------------------------
;; https://github.com/mooz/js2-mode/
;; (use-package js2-mode
;;   :ensure t
;;   :pin melpa-stable
;;   :hook (js-mode . js2-minor-mode))

;; https://github.com/codesuki/add-node-modules-path
;; (use-package add-node-modules-path
;;   :ensure t
;;   :pin melpa-stable
;;   :hook (js-mode typescript-mode))

;; (defun nodejs-repl-config ()
;;   (define-key typescript-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
;;   (define-key typescript-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
;;   (define-key typescript-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
;;   (define-key typescript-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
;;   (define-key typescript-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
;;   (define-key typescript-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

;; (use-package nodejs-repl
;;   :ensure t
;;   :hook (js-mode typescript-mode)
;;   :config (nodejs-repl-config))

;; https://github.com/emacs-typescript/typescript.el
;; (use-package typescript-mode
;;   :ensure t)

;; ----------------------------------------------------------
;; Python
;; ----------------------------------------------------------
;; None

(provide 'dev-lsp)

;;; dev-lsp.el ends here
