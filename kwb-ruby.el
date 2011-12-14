;; Ruby is a dev mode
(add-hook 'ruby-mode-hook 'run-dev-hook)

;; RVM
(rvm-use-default)

;; nicely indent things as we go
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; Make sure we're in ruby mode for various files
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

;; activate flymake for ruby
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(provide 'kwb-ruby)
