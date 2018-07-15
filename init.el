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

;; -------------------------------------
;; Define packages
;; -------------------------------------
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

(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.hs\\'" . haskell-mode))
(use-package intero
  :ensure t
  :pin melpa-stable
  :after (haskell-mode)
  :hook haskell-mode)
  

;; (defvar kwb-packages
;;   '(add-node-modules-path
;;     auctex
;;     color-theme-sanityinc-tomorrow
;;     company
;;     company-jedi
;;     company-tern
;;     ein
;;     elpy
;;     exec-path-from-shell
;;     flycheck
;;     geiser
;;     js2-mode
;;     markdown-mode
;;     multiple-cursors
;;     nodejs-repl
;;     nose
;;     pipenv
;;     projectile
;;     slime
;;     slime-company
;;     smartparens
;;     tern
;;     tide
;;     ts-comint
;;     web-mode))

;; -------------------------------------
;; HERE BE LIZARDS
;; -------------------------------------
;; (add-to-list 'safe-local-variable-values '(lexical-binding . t))
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)

;; -------------------------------------
;; HERE BE DRAGONS
;; -------------------------------------

;; -------------------------------------
;; i got your dotfiles right here
;; -------------------------------------
;; (setq dotfiles-dir (file-name-directory (or (buffer-file-name)
;;                                             load-file-name)))
;; (add-to-list 'load-path (concat dotfiles-dir "dev-modes"))
;; (setq custom-file (concat dotfiles-dir "custom.el"))
;; (load custom-file 'noerror)


;;(exec-path-from-shell-initialize)

;; (setq backup-directory-alist `(("." . ,(expand-file-name
;;                                         (concat dotfiles-dir "backups")))))

;; (load-theme 'sanityinc-tomorrow-blue t)

;; (add-to-list 'default-frame-alist '(height . 45))
;; (add-to-list 'default-frame-alist '(width . 180))
;; (set-frame-font "Inconsolata-10")

;; (show-paren-mode 1)
;; (require 'smartparens-config)

;; (setq ido-enable-flex-matching t)
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-everywhere t)
;; (setq ido-max-directory-size 100000)
;; (ido-mode t)

;; new line above and below
;; hat-tip: http://blog.peepcode.com/blog/2012/commanding-your-text-editor/
;; (defun newline-previous ()
;;   "Insert a blank line above the cursor and move the cursor up one line."
;;   (interactive)
;;   (beginning-of-line)
;;   (newline)
;;   (previous-line)
;;   (indent-according-to-mode))
;; (defun newline-next ()
;;   "Insert an indented newline after the current line and move the point to it."
;;   (interactive)
;;   (end-of-line)
;;   (newline-and-indent))

;; (setq dired-use-ls-dired nil)

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; -------------------------------------
;; key bindings
;; -------------------------------------
;; (windmove-default-keybindings)

;; (define-key global-map (kbd "C-+") 'text-scale-increase)
;; (define-key global-map (kbd "C--") 'text-scale-decrease)



;;(define-key global-map (kbd "RET") 'newline-and-indent)

;; (define-key global-map (kbd "M-p") 'newline-previous)
;; (define-key global-map (kbd "M-n") 'newline-next)



;; -------------------------------------
;; eww - browse in emacs by default
;; -------------------------------------
;; (setq browse-url-browser-function 'eww-browse-url)
;; (defun eww-more-readable ()
;;   "Makes eww more pleasant to use. Run it after eww buffer is loaded."
;;   (interactive)
;;   (setq eww-header-line-format nil)               ;; removes page title
;;   (setq mode-line-format nil)                     ;; removes mode-line
;;   (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
;;   (redraw-display)                                ;; apply mode-line changes
;;   (eww-reload 'local))                            ;; apply eww-header changes


;; -------------------------------------
;; Autocompletion
;; -------------------------------------
;; (global-company-mode)
;; (setq company-tooltip-idle-delay 0)
;; (setq company-idle-delay 0)
;; (global-set-key (kbd "<C-tab>") 'company-complete)


;; -------------------------------------
;; Multiple cursors FTW
;; -------------------------------------
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((js . t)
;;    (matlab . t)
;;    (octave . t)
;;    (python . t)
;;    (R . t)
;;    (scheme . t)
;;    ))

;; -------------------------------------
;; Setup all the development modes
;; -------------------------------------
;(require 'kwb-dev)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit git-timemachine use-package benchmark-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
