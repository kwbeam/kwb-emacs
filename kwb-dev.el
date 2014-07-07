;;; init --- kwbeam emacs development configuration
;;; Commentary:

;; Setup Emacs for all the programming modes I use.

;;; Code:

;; -------------------------------------
;; software development
;; -------------------------------------
;; -----------------
;; general stuff
;; -----------------
(defun add-line-numbers ()
  (linum-mode 1))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(require 'auto-complete)
(defun add-auto-complete ()
  (auto-complete-mode 1))

(defun dev-before-save-hook ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)
(add-hook 'dev-hook 'add-auto-complete)
(add-hook 'dev-hook 'dev-before-save-hook)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (interactive)
  (run-hooks 'dev-hook))

(yas-global-mode 1)

(require 'vc-svn)
(require 'dsvn)

;; Use projectile for everything
(projectile-global-mode)

;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (js . t)
   (lisp . t)
   (python . t)
   (ruby . t)
   (scheme . t)
   ))

;; -----------------
;; Web Mode
;; -----------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; -----------------
;; Python
;; -----------------
(require 'python)
(add-hook 'python-mode-hook 'run-dev-hook)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtual_envs/")
(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (venv-workon project-venv-name)))

(setq eshell-prompt-function
    (lambda ()
      (concat venv-current-name " $ ")))

(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq jedi:setup-keys t)
;; (setq ein:use-auto-complete f)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; -----------------
;; Ruby
;; -----------------
(add-hook 'ruby-mode-hook 'run-dev-hook)

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; -----------------
;; Feature
;; -----------------
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; -----------------
;; Scheme
;; -----------------
(setq scheme-program-name "racket")

;; -----------------
;; Lisp
;; -----------------
(setq inferior-lisp-program "clisp")

;; -----------------
;; Markdown
;; -----------------
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(cond ((eq system-type 'darwin)
       (setq markdown-open-command "/Applications/Mou.app/Contents/MacOS/Mou")))

;; -----------------
;; JavaScript
;; -----------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
(require 'js2-refactor)
(setq-default js2-basic-offset 4)

;; use node.js as a repl
(setq inferior-js-program-command "node")
(setenv "NODE_NO_READLINE" "1")

(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;; Add tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; Setup skewer hooks
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

;; -----------------
;; CoffeeScript
;; -----------------
(setq coffee-tab-width 2)

;; -----------------
;; SCSS
;; -----------------
(setq scss-compile-at-save nil)

;; -----------------
;; Clojure
;; -----------------
(setq nrepl-hide-special-buffers t)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'enable-paredit-mode)
(add-hook 'cider-mode-hook 'run-dev-hook)


;; -----------------
;; JSON
;; -----------------
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))


;; -----------------
;; IDL
;; -----------------
(setq idlwave-shell-explicit-file-name "idl82")
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

;;; kwb-dev.el ends here
