;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
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

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'kwb-defuns)
(require 'kwb-bindings)
(require 'kwb-misc)
(require 'kwb-lisp)
(require 'kwb-ruby)

(regen-autoloads)
(load custom-file 'noerror)

;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************
;; startup the server so we can edit stuff from the command line in
;; existing emacs window
(server-start)

;; set the default font
(set-default-font "Inconsolata")
(set-face-attribute 'default nil :height 120)

;; tabs are two spaces
(setq default-tab-width 2)
(setq tab-width 2)

;; blink the cursor so I can see it
(blink-cursor-mode 1)

;; add color theme!
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-zenburn)))

;; setup shell path
;; see:
;;   http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command/2566945#2566945
;;
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; automatically sync up external changes to files
(global-auto-revert-mode t)


;; ***************************************************************************
;; Cool Modes
;; ***************************************************************************

;; Twittering mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/twittering-mode"))
(require 'twittering-mode)


;; ***************************************************************************
;; General Programming Stuff
;; ***************************************************************************

;; yasnippet
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yas"))
(require 'yasnippet-bundle)


;; ***************************************************************************
;; Ruby on Rails
;; ***************************************************************************

;; setup rinari
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

;; ***************************************************************************
;; Ruby
;; ***************************************************************************

;; add line numbers
(add-hook 'kwb-code-modes-hook
          (lambda () (linum-mode 1)))
(add-hook 'ruby-mode-hook
          (lambda () (run-hooks 'kwb-code-modes-hook)))

;; rvm emacs integration
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rvm.el"))
(require 'rvm)
(rvm-use-default)


;; ***************************************************************************
;; Scheme
;; ***************************************************************************

(setq scheme-program-name "mit-scheme")


;; ***************************************************************************
;; ERC
;; ***************************************************************************

(setq erc-nick "kwbeam")


;; ***************************************************************************
;; Slime
;; ***************************************************************************

;; unknown failure in slime unless this is defined
(setq warning-suppress-types nil)


;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;; init.el ends here
