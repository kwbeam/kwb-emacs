;; ***********************************************
;; General settings for development
;; ***********************************************

;; show me the line numbers in source
(defun add-line-numbers ()
  (linum-mode 1))

;; auto wrap, but only in comments
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (run-hooks 'dev-hook))

;; Setup yasnippets
(yas-global-mode 1)


;; ***********************************************
;; Python
;; ***********************************************

;;; Electric Pairs
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without surrounding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))


;; ***********************************************
;; Ruby
;; ***********************************************

;; Scoop me up some RVM!
(rvm-use-default)

;; Ruby is a dev mode
(add-hook 'ruby-mode-hook 'run-dev-hook)

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
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; activate flymake for ruby
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;; ***********************************************
;; Feature
;; ***********************************************

;; Make sure we're in feature mode for the right files
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))


;; ***********************************************
;; Scheme
;; ***********************************************

;; set the scheme to use
(setq scheme-program-name "mit-scheme")


;; ***********************************************
;; Markdown
;; ***********************************************

;; stuff ending with .md is also markdown
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; ***********************************************
;; JavaScript
;; ***********************************************

(setq js-indent-level 2)

(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-mode)


;; ***********************************************
;; CoffeeScript
;; ***********************************************
(setq coffee-tab-width 2)


(provide 'kwb-dev)
