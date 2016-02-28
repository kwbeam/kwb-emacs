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
;; Mac OSX requires: (pip|conda) install certifi
(setq tls-checktrust t)
(cond ((eq system-type 'gnu/linux)
       (setq tls-program
             '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
               "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3"
               "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof")))
      ((eq system-type 'darwin)
       (let ((mac-cert-file "/Users/kbeam/miniconda/envs/default/lib/python2.7/site-packages/certifi/cacert.pem"))
         (setq tls-program
               (list (format "gnutls-cli --x509cafile %s -p %%p %%h" mac-cert-file))))))

"/Users/kbeam/miniconda/envs/default/lib/python2.7/site-packages/certifi/cacert.pem"

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
  '(autopair
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    company
    dockerfile-mode
    dsvn
    ein
    elpy
    exec-path-from-shell
    expand-region
    flycheck
    flycheck-color-mode-line
    gist
    git-timemachine
    google-this
    haskell-mode
    inf-ruby
    js2-mode
    less-css-mode
    magit
    markdown-mode
    multiple-cursors
    nodejs-repl
    nose
    org
    projectile
    psci
    puppet-mode
    purescript-mode
    rbenv
    request
    rinari
    robe
    rspec-mode
    rubocop
    ruby-electric
    scss-mode
    skewer-mode
    smartparens
    solarized-theme
    tern
    web-mode
    websocket
    yaml-mode
    yasnippet
    zenburn-theme))

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

(load-theme 'solarized-dark t)

(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(height . 45))
        (add-to-list 'default-frame-alist '(width . 180)))
      ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(height . 70))
        (add-to-list 'default-frame-alist '(width . 240)))
      (t
       (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 120))))

(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Inconsolata-10"))
      ((eq system-type 'darwin)
       (set-frame-font "Inconsolata-14")))

(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

(setq create-lockfiles nil)

(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)

(setq multi-term-program "/bin/bash")

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

(setq erc-nick "kwbeam")

;; Check to see if we're running darwin (osx)
(defvar running-macos
  (or (string-match "darwin" (prin1-to-string system-type))
      (memq (window-system) '(mac ns)))
  "Boolean to determine if we are running on a macintosh laptop" )

;; Work around for bug in macosx
(when running-macos
(cd (getenv "HOME")))


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
(global-set-key (kbd "C-c g") 'magit-status)


;; -------------------------------------
;; Autocompletion
;; -------------------------------------
(global-company-mode)


;; -------------------------------------
;; Parens FTW
;; -------------------------------------
(require 'smartparens-config)
(sp-use-smartparens-bindings)


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
