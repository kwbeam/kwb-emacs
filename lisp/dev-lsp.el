;;; dev-lsp --- Dev modes setup -- lsp
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------
;; LSP - eglot
;; ----------------------------------------------------------
(use-package eglot
  :ensure t
  :hook ((js-mode typescript-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

;; ----------------------------------------------------------
;; LSP - lsp-mode
;; ----------------------------------------------------------
;; set prefix for lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :init
  (add-node-modules-path)
  :hook ((javascript-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp)
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

;; TODO
  ;; :config
  ;; (setq lsp-idle-delay 0.500
  ;;       lsp-ui-doc-enable t
  ;;       lsp-ui-doc-use-childframe t
  ;;       lsp-ui-doc-position 'top
  ;;       lsp-ui-doc-include-signature t
  ;;       lsp-ui-sideline-enable nil
  ;;       lsp-ui-flycheck-enable t
  ;;       lsp-ui-flycheck-list-position 'right
  ;;       lsp-ui-flycheck-live-reporting t
  ;;       lsp-ui-peek-enable t
  ;;       lsp-ui-peek-list-width 60
  ;;       lsp-ui-peek-peek-height 25))

(use-package dap-mode
  :ensure t)

;; TODO
  ;; :config
  ;; (require 'dap-firefox)
  ;; (require 'dap-node)
;; (require 'dap-python))

;; (use-package dap-firefox)
;; (use-package dap-node)
;; (use-package dap-python)

;; ----------------------------------------------------------
;; JavaScript & TypeScript
;; ----------------------------------------------------------
;; https://github.com/mooz/js2-mode/
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :hook (js-mode . js2-minor-mode))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :hook (js-mode typescript-mode))

(defun nodejs-repl-config ()
  (define-key typescript-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
  (define-key typescript-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
  (define-key typescript-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key typescript-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
  (define-key typescript-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key typescript-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

;; (use-package nodejs-repl
;;   :ensure t
;;   :hook (js-mode typescript-mode)
;;   :config (nodejs-repl-config))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t)

;; ----------------------------------------------------------
;; Python
;; ----------------------------------------------------------
;; None

(provide 'dev-lsp)

;;; dev-lsp.el ends here
