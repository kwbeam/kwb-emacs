;;; init --- kwbeam emacs configuration
;;; Commentary:

;; In the beginning...
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:

(setq dotfiles-dir (file-name-directory (or (buffer-file-name)
                                            load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defvar kwb-packages
  '(auto-complete
    autopair
    clojure-mode
    clojure-test-mode
    coffee-mode
    dsvn
    ein
    exec-path-from-shell
    expand-region
    feature-mode
    flycheck
    google-this
    inf-ruby
    jedi
    magit
    markdown-mode
    multiple-cursors
    nose
    paredit
    python
    rinari
    rspec-mode
    ruby-mode
    ruby-electric
    rvm
    scss-mode
    solarized-theme
    virtualenv
    yasnippet))

(mapc #'(lambda (package)
         (unless (package-installed-p package)
           (package-install package)))
      kwb-packages)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(require 'kwb-general)
(require 'kwb-personal)
(require 'kwb-bindings)
(require 'kwb-dev)

;;; init.el ends here
