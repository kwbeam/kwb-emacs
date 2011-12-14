;; In the beginning...
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; add to load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; setup the ELPA Package manager
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

;; set and load custom file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; load specific customizations
(require 'kwb-general)
(require 'kwb-personal)
(require 'kwb-bindings)
(require 'kwb-dev)
