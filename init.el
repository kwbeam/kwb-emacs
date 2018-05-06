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
;; Basic security setup
;; -------------------------------------
;; From: https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)
(setq tls-program
      '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
        "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3"
        "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof"))

;; -------------------------------------
;; load all the things
;; -------------------------------------
(require 'package)

;; make sure we use https for everything
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar kwb-packages
  '(add-node-modules-path
    auctex
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    company
    company-jedi
    company-tern
    dockerfile-mode
    ein
    elpy
    exec-path-from-shell
    flycheck
    flycheck-color-mode-line
    geiser
    git-timemachine
    intero
    js2-mode
    json-mode
    less-css-mode
    magit
    markdown-mode
    multiple-cursors
    nodejs-repl
    nord-theme
    nose
    org
    pipenv
    projectile
    psc-ide
    psci
    purescript-mode
    repl-toggle
    slime
    slime-company
    smartparens
    tern
    tide
    ts-comint
    web-mode
    yaml-mode
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
(add-to-list 'load-path (concat dotfiles-dir "dev-modes"))
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

(load-theme 'nord t)
(setq nord-comment-brightness 15)

(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 180))
(set-frame-font "Inconsolata-10")

(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

(setq create-lockfiles nil)

(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)

(setq multi-term-program "/bin/bash")

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(column-number-mode t)

(setq select-enable-clipboard t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1)
(require 'smartparens-config)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode t)

(set-default 'indicate-empty-lines t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(setq echo-keystrokes 0.1)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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


;; -------------------------------------
;; key bindings
;; -------------------------------------
(windmove-default-keybindings)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "M-p") 'newline-previous)
(define-key global-map (kbd "M-n") 'newline-next)

(global-set-key (kbd "C-c g") 'magit-status)


;; -------------------------------------
;; eww - browse in emacs by default
;; -------------------------------------
(setq browse-url-browser-function 'eww-browse-url)
(defun eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload 'local))                            ;; apply eww-header changes


;; -------------------------------------
;; Autocompletion
;; -------------------------------------
(global-company-mode)
(setq company-tooltip-idle-delay 0)
(setq company-idle-delay 0)
(global-set-key (kbd "<C-tab>") 'company-complete)


;; -------------------------------------
;; Multiple cursors FTW
;; -------------------------------------
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; -------------------------------------
;; Setup all the development modes
;; -------------------------------------
(require 'kwb-dev)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
