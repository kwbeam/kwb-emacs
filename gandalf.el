;; ***************************************************************************
;; General Emacs Stuff
;; ***************************************************************************

;; Fix up the Mac so the option key is nothing and command is meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; startup the server so we can edit stuff from the command line in
;; existing emacs window
(server-start)

;; set geometry
(add-to-list 'default-frame-alist '(height . 53))
(add-to-list 'default-frame-alist '(width . 180))

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
