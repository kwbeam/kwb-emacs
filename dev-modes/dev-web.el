;; -----------------
;; Web Mode
;; -----------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-to-list 'load-path "~/tools/HTML5-YASnippet-bundle")
;; some time after yasnippet has been loaded execute this
(require 'html5-snippets)

;; Use eslint
(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'dev-web)
