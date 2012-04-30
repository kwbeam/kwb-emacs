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


;; ***********************************************
;; Ruby mode
;; ***********************************************

;; Scoop me up some RVM!
(require 'rvm)
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

;; activate flymake for ruby
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;; ***********************************************
;; Scheme mode
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


;; ***********************************************
;; SCSS
;; ***********************************************
(setq exec-path (cons (expand-file-name "/home/elrond/.rvm/gems/ruby-1.9.2-p290/bin/sass") exec-path))


(provide 'kwb-dev)
