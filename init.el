;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(require 'kwb-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load up starter kit customizations
(require 'kwb-defuns)
(require 'kwb-bindings)
(require 'kwb-misc)
(require 'kwb-ruby)

(regen-autoloads)
(load custom-file 'noerror)

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

;; ***************************************************************************
;; Markdown
;; ***************************************************************************
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ***************************************************************************
;; Ruby
;; ***************************************************************************
(require 'rvm)
(rvm-use-default)

;; add line numbers
(add-hook 'kwb-code-modes-hook
          (lambda () (linum-mode 1)))
(add-hook 'ruby-mode-hook
          (lambda () (run-hooks 'kwb-code-modes-hook)))

;; ***************************************************************************
;; Scheme
;; ***************************************************************************
(setq scheme-program-name "mit-scheme")

;; ***************************************************************************
;; Clojure
;; ***************************************************************************
(require 'clojure-mode)

;; ***************************************************************************
;; ERC
;; ***************************************************************************
(setq erc-nick "kwbeam")

;; ***************************************************************************
;; Slime
;; ***************************************************************************
(require 'slime)
;; unknown failure in slime unless this is defined
(setq warning-suppress-types nil)

;; ***************************************************************************
;; User and System specific customizations
;; ***************************************************************************
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
