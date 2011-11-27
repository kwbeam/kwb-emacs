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
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)

;; set and load custom file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Load up specific customizations
(require 'kwb-general)
(require 'kwb-bindings)
(require 'kwb-dev)
(require 'kwb-ruby)
(require 'kwb-scheme)
(require 'kwb-clojure)
(require 'kwb-lisp)
(require 'kwb-markdown)
