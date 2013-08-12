;;; init --- kwbeam emacs configuration
;;; Commentary:

;; In the beginning...
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:

;; -------------------------------------
;; load all the things
;; -------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar kwb-packages
  '(auto-complete
    autopair
    clojure-mode
    clojure-test-mode
    coffee-mode
    dsvn
    ein
    exec-path-from-shell
    expand-region
    feature-mode
    flycheck
    flycheck-color-mode-line
    google-this
    inf-ruby
    jedi
    js2-mode
    js2-refactor
    magit
    markdown-mode
    multiple-cursors
    nose
    org
    paredit
    rinari
    rspec-mode
    ruby-electric
    rvm
    scss-mode
    solarized-theme
    virtualenv
    web-mode
    yasnippet))

(mapc #'(lambda (package)
         (unless (package-installed-p package)
           (package-install package)))
      kwb-packages)


;; -------------------------------------
;; i got your dotfiles right here
;; -------------------------------------
(setq dotfiles-dir (file-name-directory (or (buffer-file-name)
                                            load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)


;; -------------------------------------
;; blue-collar emacs stuff
;; -------------------------------------
(server-start)

(exec-path-from-shell-initialize)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell t)

(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(height . 45))
        (add-to-list 'default-frame-alist '(width . 180)))
      ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(height . 50))
        (add-to-list 'default-frame-alist '(width . 160)))
      (t
       (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 120))))

(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Inconsolata-12"))
      ((eq system-type 'darwin)
       (set-frame-font "Inconsolata-16")))

(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

(blink-cursor-mode 1)

(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)

;; Mac Specific Stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(column-number-mode t)

(setq x-select-enable-clipboard t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 1)

(set-default 'indicate-empty-lines t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(setq echo-keystrokes 0.1)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; new line above and below
;; hat-tip: http://blog.peepcode.com/blog/2012/commanding-your-text-editor/
(defun newline-previous ()
  "Insert a blank line above the cursor and move the cursor up one line."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))
(defun newline-next ()
  "Insert an indented newline after the current line and move the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(setq dired-use-ls-dired nil)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(setq erc-nick "kwbeam")


;; -------------------------------------
;; key bindings
;; -------------------------------------
(windmove-default-keybindings)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "M-p") 'newline-previous)
(define-key global-map (kbd "M-n") 'newline-next)

(global-set-key (kbd "C-!") 'er/expand-region)


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

;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
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
(setq virtualenv-root "~/.virtual_envs/")

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
(setq jedi:setup-keys t)

;; -----------------
;; Ruby
;; -----------------
(add-hook 'ruby-mode-hook 'run-dev-hook)

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

(rvm-use-default)
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
(setq scheme-program-name "mit-scheme")

;; -----------------
;; Markdown
;; -----------------
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; -----------------
;; JavaScript
;; -----------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'run-dev-hook)
(require 'js2-refactor)
;; TODO: Do I want 2 or 4 spaces for JS?
(setq-default js2-basic-offset 2)

;; -----------------
;; CoffeeScript
;; -----------------
(setq coffee-tab-width 2)

;; -----------------
;; SCSS
;; -----------------
(setq scss-compile-at-save nil)

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

;;; init.el ends here
