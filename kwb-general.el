;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************
;; startup the server so we can edit stuff with emacsclient
(server-start)

;; use the right exec-path
(exec-path-from-shell-initialize)

;; take away distracting stuff
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; no bells!
(setq visible-bell t)

;; set geometry
(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(height . 60))
        (add-to-list 'default-frame-alist '(width . 220)))
      ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(height . 50))
        (add-to-list 'default-frame-alist '(width . 160)))
      (t
       (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 120))))

;; set the default font
(cond ((string= system-name "kbeam.dev.int.nsidc.org")
       (progn
         (set-default-font "Courier 10 Pitch")
         (set-face-attribute 'default nil :height 120)))
      ((eq system-type 'darwin)
       (set-default-font "Inconsolata-16")))

;; load the theme
(require 'solarized-dark-theme)

;; add expand-region binding
(global-set-key (kbd "C-!") 'er/expand-region)

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
;; (setq require-final-newline t)

;; lexical binding is better
(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; delete trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

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
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; don't use --dired option to ls
(setq dired-use-ls-dired nil)

(provide 'kwb-general)
