;;; dev-typescript --- kwbeam TypeScript setup
;;; Commentary:
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisite Emacs packages:
;;   * company
;;   * exec-path-from-shell
;;   * flycheck
;;   * tide
;;   * ts-comint
;;   * web-mode
;; Prerequisite NPM packages:
;;   * @types/node (workaround for ts-node -- check again)
;;   * ts-node
;;   * typescript
;; Notes:
;;   When using a repl via ts-comint's M-x run-ts, set a dir local
;;   variable to point to the correct ts-node executable.  The project's
;;   .dir-locals.el file should be at the top of the project and
;;   contain something like this:
;;     ((typescript-mode (ts-comint-program-command
;;                        .
;;                        "/home/kwbeam/labs/ts-lab/node_modules/.bin/ts-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * Set the ts-comint-program-command by finding it in the
;;       project's directory tree!
;;; Code:

(require 'company)
(require 'tide)
(require 'ts-comint)

;; Basic setup for TS files
(defun setup-tide-mode ()
  "Setup tide mode for TS files."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-executable "node_modules/.bin/tsserver"))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'typescript-mode-hook 'run-dev-hook)

;; TSX: Use tide & web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook 'run-dev-hook)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; Setup key mappings for ts-comint repl.
(add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-c r") 'ts-send-region-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

;; Optional -- for the (rare) case when I might need to use tide
;; for just boring ole JS files.
(defun kwb/use-tide-for-js ()
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
