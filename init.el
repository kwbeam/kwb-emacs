;;; init --- Emacs init file
;;; Commentary:
;;; Code:

;; Temporarily increase size of GC threshold to speedup initialization.
(setq gc-cons-threshold (* 128 1024 1024))

;; Bootstrap everything with package
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Now use-package for all other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(push (expand-file-name "modules" user-emacs-directory) load-path)
(require 'basics)
(require 'packages)
(require 'javascript)
(require 'py)

(setq gc-cons-threshold (* 32 1024 1024))

;;; init.el ends here
