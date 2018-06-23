;; -----------------
;; Web Mode
;; -----------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-pairing nil)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook 'run-dev-hook)

(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

(add-to-list 'load-path "~/tools/HTML5-YASnippet-bundle")
;; some time after yasnippet has been loaded execute this
(require 'html5-snippets)

;; Use eslint
(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'dev-web)
