;; -------------------------------------
;; Bootstrap everything with package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Use use-package for all other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; time your emacs init
;;   M-x benchmark-init/show-durations-tree
;;   M-x benchmark-init/show-durations-tabulated
(use-package benchmark-init :ensure t)

;; -------------------------------------
;; Minimalist Emacs Configuration
(require 'server)
(if (not (server-running-p))
    (server-start))
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(column-number-mode t)
(setq select-enable-clipboard t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-frame-font "Inconsolata-12")
(setq uniquify-buffer-name-style 'forward)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(set-default 'indicate-empty-lines t)
(set-default 'indent-tabs-mode nil)
(setq tab-width 2)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1)
(setq create-lockfiles nil)
(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)
(random t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq custom-file (locate-user-emacs-file ".custom.el"))
(load custom-file t t)
(setq browse-url-browser-function 'eww-browse-url)

;; Use ido mode
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode t)

;; keybindings
(windmove-default-keybindings)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

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
(define-key global-map (kbd "M-p") 'newline-previous)
(define-key global-map (kbd "M-n") 'newline-next)

;; Setup the exec-path based on the shell PATH
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; -------------------------------------
;; Define packages
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :pin melpa-stable
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :defer t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :config
  (require 'smartparens-config)
  (show-paren-mode t))

(use-package git-timemachine
  :ensure t
  :pin melpa-stable
  :defer t)

(use-package magit
  :ensure t
  :pin melpa-stable
  :defer t
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

(use-package company
  :ensure t
  :pin melpa-stable
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (global-set-key (kbd "<C-tab>") 'company-complete))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :init (global-flycheck-mode))

(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.md\\'")

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (haskell . t)
     (js . t)
     (lisp . t)
     (python . t)
     (scheme . t))))

;; Elm
(use-package elm-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.elm\\'" . elm-mode)
  :after (company)
  :config
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

;; Haskell
(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.hs\\'" . haskell-mode))

(use-package intero
  :ensure t
  :pin melpa-stable
  :after (haskell-mode)
  :hook haskell-mode)

;; JavaScript
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :after (js2-mode)
  :hook js2-mode)

(use-package indium
  :ensure t
  :pin melpa-stable
  :hook (add-node-modules-path . indium-interaction-mode))

;; Lisp
(use-package slime
  :ensure t
  :pin melpa-stable
  :after lisp-mode
  :defer t
  :init
  (setq slime-lisp-implementations
      '((sbcl ("clisp"))
        (clisp ("sbcl"))))
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :pin melpa-stable
  :defer t
  :after (company slime))

;; Python
(use-package elpy
  :ensure t
  :pin melpa-stable
  :hook (python-mode . elpy-mode)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Don't use flymake (elpy default), use flycheck:
  ;; https://github.com/jorgenschaefer/elpy/issues/137
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (pyvenv-mode -1))

(use-package company-jedi
  :ensure t
  :pin melpa-stable
  :hook python-mod
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package ein
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  (add-to-list 'company-backends 'ein:company-backend))

;; Scheme
(use-package geiser
  :ensure t
  :pin melpa-stable
  :defer t
  :after (scheme)
  :init
  (setq geiser-active-implementations '(mit racket)))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.ts\\'")

(defun kwb-tide-setup ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-tsserver-executable "node_modules/.bin/tsserver"))

(use-package tide
  :ensure t
  :pin melpa-stable
  :after (company flycheck typescript-mode)
  :hook (typescript-mode . kwb-tide-setup))

(use-package web-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.tsx\\'"
  :init
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :config
  (add-hook 'web-mode-hook 'kwb-tide-setup))

(use-package ts-comint
  :ensure t
  :after (tide)
  :defer t
  :bind (("C-x C-e" . 'ts-send-last-sexp)
         ("C-c r" . 'ts-send-region-and-go)
         ("C-c b" . 'ts-send-buffer)
         ("C-M-x" . 'ts-send-last-sexp-and-go)
         ("C-c C-b" . 'ts-send-buffer-and-go)
         ("C-c l" . 'ts-load-file-and-go)))

;; Setup all dev modes the same
(defun kwb-dev-hook ()
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (smartparens-mode))
(mapc
 (lambda (hook) (add-hook hook 'kwb-dev-hook))
 '(emacs-lisp-mode-hook
   elm-mode-hook
   haskell-mode-hook
   lisp-mode-hook
   js2-mode-hook
   python-mode-hook
   scheme-mode-hook
   typescript-mode-hook
   web-mode-hook))
