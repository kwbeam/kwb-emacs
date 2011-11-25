;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************
;; startup the server so we can edit stuff from the command line in
;; existing emacs window
(server-start)

;; take away distracting stuff
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set geometry
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 200))

;; set the default font
(set-default-font "Inconsolata")
(set-face-attribute 'default nil :height 120)

;; set the color theme
(require 'color-theme)
(require 'color-theme-railscasts)

;; tabs are two spaces
(setq default-tab-width 2)
(setq tab-width 2)

;; blink the cursor so I can see it
(blink-cursor-mode 1)

;; automatically sync up external changes to files
(global-auto-revert-mode t)

;; Mac Specific Stuff
;; Fix up the Mac so the option key is nothing and command is meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; ERC
(setq erc-nick "kwbeam")

;; Better frame titles
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Default to unified diffs
(setq diff-switches "-u -w")

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; **************************************************************
;; GROK and organize the stuff below.  Because I don't yet.
;; **************************************************************
;; Encodings (?)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(defalias 'auto-revert-tail-mode 'tail-mode)

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(delete 'try-complete-file-name-partially hippie-expand-try-functions-list)
(delete 'try-complete-file-name hippie-expand-try-functions-list)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; What?
(setq echo-keystrokes 0.1
      require-final-newline t
      save-place-file (concat dotfiles-dir "places"))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(provide 'kwb-general)
