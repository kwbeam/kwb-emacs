;; -------------------------------------
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; -------------------------------------

(setq gc-cons-threshold (* 128 1000 1000))

;; -------------------------------------
;; Bootstrap everything with package
;; -------------------------------------

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
;; -------------------------------------

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
(set-frame-font "Hack-12")
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

;; -------------------------------------
;; General-purpose packages
;; -------------------------------------

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :pin melpa-stable
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

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

(use-package exec-path-from-shell
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

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

(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :defer t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package markdown-mode
  :ensure t
  :pin melpa
  :defer t
  :mode "\\.md\\'")

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (js . t)
     (python . t))))

(use-package restclient
  :defer t
  :ensure t)

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (require 'smartparens-config)
  (show-paren-mode t))

;; -------------------------------------
;; Language Setup
;; -------------------------------------

;; -------------------------------------
;; Haskell
(use-package haskell-mode
  :ensure t)

;; -------------------------------------
;; JavaScript [LSP]
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :defer t
  :after (js2-mode)
  :hook (js2-mode))

;; -------------------------------------
;; PureScript
(use-package purescript-mode
  :ensure t)

(use-package psc-ide
  :ensure t
  :hook (purescript-mode . (lambda ()
                             (psc-ide-mode)
                             (setq psc-ide-use-npm-bin t)
                             (turn-on-purescript-indentation))))

;; -------------------------------------
;; Python [LSP]
;; Unknown:
;; + pyvenv
;; + blacken
;; + ein

;; -------------------------------------
;; TypeScript [LSP]
(use-package typescript-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode "\\.ts\\'")

;; -------------------------------------
;; Language Server Protocol
;; -------------------------------------

(use-package lsp-mode
  :ensure t
  :hook (js2-mode . lsp)
  :hook (python-mode . lsp)
  :hook (typescript-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package dap-python)

;; -------------------------------------
;; Common dev mode setup
;; -------------------------------------

(defun kwb-dev-hook ()
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (smartparens-mode))
(mapc
 (lambda (hook) (add-hook hook 'kwb-dev-hook))
 '(emacs-lisp-mode-hook
   haskell-mode-hook
   js2-mode-hook
   purescript-mode-hook
   python-mode-hook
   typescript-mode-hook))

;; -------------------------------------
;; Make gc pauses faster by decreasing the threshold.
;; -------------------------------------

(setq gc-cons-threshold (* 2 1000 1000))
