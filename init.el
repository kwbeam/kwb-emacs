;;; init --- Emacs init file
;;; Commentary:
;;; Code:

;; Temporarily increase size of GC threshold to speedup initialization.
(setq gc-cons-threshold (* 128 1024 1024))

;; Bootstrap everything with package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Now use-package for all other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; time your emacs init: M-x benchmark-init/...
(use-package benchmark-init :ensure t)

(defvar modules-dir
  (expand-file-name  "modules" (file-name-directory load-file-name)))
(add-to-list 'load-path modules-dir)

(require 'defaults)
(require 'basics)
(require 'clojure)
(require 'haskell)
(require 'lisp)
(require 'javascript)
(require 'purescript)
(require 'py)
(require 'scheme)
(require 'typescript)
(require 'lsp)
(require 'dev)

(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here
