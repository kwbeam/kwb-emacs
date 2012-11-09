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

;; Always pair
(autopair-global-mode)


;; ***********************************************
;; Python
;; ***********************************************

;; We're gonna need us a Python mode
(require 'python-mode)

(add-hook 'python-mode-hook 'flymake-mode)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)

;; Activate default virtualenv
(virtualenv-activate "~/.virtual_envs/default")

;; flymake & pyflakes
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; flymake & pep8
(add-hook 'python-mode-hook 'flymake-python-pep8-load)

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


;; ***********************************************
;; IDL
;; ***********************************************
(setq load-path (cons "~/tools/idlwave" load-path))
(autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
(autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
(setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))

(custom-set-variables
 '(idlwave-block-indent 3)
 '(idlwave-completion-case (quote ((routine . preserve) (keyword . downcase) (class . preserve) (method . preserve))))
 '(idlwave-completion-force-default-case t)
 '(idlwave-completion-show-classes 10)
 '(idlwave-continuation-indent 3)
 '(idlwave-do-actions t)
 '(idlwave-end-offset -3)
 '(idlwave-expand-generic-end t)
 '(idlwave-indent-to-open-paren nil)
 '(idlwave-init-rinfo-when-idle-after 2)
 '(idlwave-main-block-indent 3)
 '(idlwave-max-extra-continuation-indent 60)
 '(idlwave-pad-keyword t)
 '(idlwave-query-class (quote ((method-default) (keyword-default) ("INIT" . t) ("CLEANUP" . t) ("SETPROPERTY" . t) ("GETPROPERTY" . t))))
 '(idlwave-reserved-word-upcase nil nil nil "Want pro rather than PRO")
 '(idlwave-shell-automatic-electric-debug t)
 '(idlwave-shell-automatic-start t)
 '(idlwave-shell-debug-modifiers (quote (super)))
 '(idlwave-shell-electric-stop-color "darkviolet")
 '(idlwave-shell-reset-no-prompt t)
 '(idlwave-shell-separate-examine-output nil)
 '(idlwave-shell-show-commands (quote (run breakpoint debug misc)))
 '(idlwave-shell-use-dedicated-frame nil)
 '(idlwave-shell-use-dedicated-window nil)
 '(idlwave-surround-by-blank t))

(add-hook 'idlwave-mode-hook
	  (lambda ()

            ;; Skip over Underbars when word forward
            ;; (modify-syntax-entry ?_ "w")
            ;; 2011-09-16: <mhs>  I didn't like that. </mhs>
            ;; These insert spaces around the character.
            ;; alpha[beta] --> alpha[ beta ]
	    (idlwave-action-and-binding "["  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding "]"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "("  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding ")"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "*"  '(idlwave-surround -1 'nil 1))))

(defun my-common-idlwave-hook ()
  (local-set-key [(meta .)] 'idlwave-find-module)
  (local-set-key [(super z)] 'mhs-idlwave-shell-reset)
  (local-set-key [(meta s)] 'idlwave-complete)
  (local-set-key (kbd "C-M-?") 'idlwave-context-help))

(add-hook 'idlwave-mode-hook 'my-common-idlwave-hook 't)
(add-hook 'idlwave-shell-mode-hook 'my-common-idlwave-hook 't)

(provide 'kwb-dev)
