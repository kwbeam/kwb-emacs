;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Package manager
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; set and load custom file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Load up customizations
(require 'kwb-bindings)
(require 'kwb-misc)
(require 'kwb-dev)
(require 'kwb-ruby)

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
;; Mac Specific Stuff
;; ***************************************************************************
;; Fix up the Mac so the option key is nothing and command is meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
