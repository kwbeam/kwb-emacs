(require 'puppet-mode)
(add-hook 'puppet-mode-hook 'run-dev-hook)

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(add-hook 'puppet-mode-hook '(lambda ()
			       (autopair-mode)
			       (flycheck-mode)))

(provide 'dev-puppet)
