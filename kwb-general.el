;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************
;; startup the server so we can edit stuff with emacsclient
(server-start)

;; take away distracting stuff
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no bells!
(setq visible-bell t)

;; set geometry
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 220))

;; set the default font
(set-default-font "Inconsolata-12")

;; set the color theme
(require 'color-theme)
(require 'color-theme-railscasts)

;; expand tabs into spaces
(set-default 'indent-tabs-mode nil)

;; blink the cursor so I can see it
(blink-cursor-mode 1)

;; automatically sync up external changes to files
(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)

;; Mac Specific Stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; ERC
(setq erc-nick "kwbeam")

;; Better frame titles
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; Encodings
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Turn on ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;; Show empty lines at the end of the file
(set-default 'indicate-empty-lines t)

;; Auto fill in text
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Automatically check spelling
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; y or n is okay
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Don't clutter up directories with backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Show multi-key commands quickly
(setq echo-keystrokes 0.1)

;; Put a newline in
(setq require-final-newline t)

;; lexical binding is better
(add-to-list 'safe-local-variable-values '(lexical-binding . t))

(provide 'kwb-general)
