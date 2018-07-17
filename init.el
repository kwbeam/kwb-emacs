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
;; Setup package loading
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
;; Minimal Emacs Customization
;; -------------------------------------
(server-start)
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
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
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
;; Define packages
;; -------------------------------------
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :pin melpa-stable
  :config
  (load-theme 'sanityinc-tomorrow-blue t))

(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
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
  :pin melpa-stable)

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

(use-package company
  :ensure t
  :pin melpa-stable
  :config
  (global-company-mode)
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0)
  (global-set-key (kbd "<C-tab>") 'company-complete))

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

;; TODO: Use this in all the dev modes
(defun kwb-dev-hook ()
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Haskell
;; Prerequisite Language installs:
;;   * Stack / GHC
(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.hs\\'" . haskell-mode))
(use-package intero
  :ensure t
  :pin melpa-stable
  :after (haskell-mode)
  :hook haskell-mode)

;; Lisp
;; Prerequisite Language installs:
;;   * CLISP, SBCL
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

;; Scheme
;; Prerequisite Scheme installs:
;;   * MIT/GNU Scheme 9.1.1 or better
;;   * Racket 6.0 or better
(use-package geiser
  :ensure t
  :pin melpa-stable
  :defer t
  :after (scheme)
  :init
  (setq geiser-active-implementations '(mit racket)))

;; JavaScript
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js2-basic-offset 2))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.tsx?\\$")
