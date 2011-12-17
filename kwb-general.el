;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************
;; startup the server so we can edit stuff with emacsclient
(server-start)

;; take away distracting stuff
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; no bells!
(setq visible-bell t)

;; set geometry
(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(height . 70))
        (add-to-list 'default-frame-alist '(width . 240)))
      ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(height . 50))
        (add-to-list 'default-frame-alist '(width . 180)))
      (t
       (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 120))))

;; set the default font
(cond ((eq system-type 'gnu/linux)
       (set-default-font "Inconsolata-10"))
      ((eq system-type 'darwin)
       (set-default-font "Inconsolata-14")))

;; set the color theme
(require 'color-theme)
(require 'color-theme-railscasts)

;; deal with tabs correctly
(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

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

;; better frame titles
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; show me the column number
(column-number-mode t)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; encodings
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; turn on ido-mode
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

;; show empty lines at the end of the file
(set-default 'indicate-empty-lines t)

;; auto fill in text
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; automatically check spelling
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; y or n is okay
(defalias 'yes-or-no-p 'y-or-n-p)

;; seed the random-number generator
(random t)

;; don't clutter up directories with backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; show multi-key commands quickly
(setq echo-keystrokes 0.1)

;; put a newline in
(setq require-final-newline t)

;; lexical binding is better
(add-to-list 'safe-local-variable-values '(lexical-binding . t))

(provide 'kwb-general)
