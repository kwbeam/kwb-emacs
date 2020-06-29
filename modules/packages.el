;;; packages --- basic set of packages
;;; Commentary:
;;; Code:

;; https://github.com/belak/base16-emacs
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

;; https://company-mode.github.io/
(use-package company
  :ensure t
  :pin melpa-stable
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (global-set-key (kbd "<C-tab>") 'company-complete))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :pin melpa
  :defer t
  :config
  (exec-path-from-shell-initialize))

;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :ensure t
  :pin melpa-stable
  :defer t)

;; https://magit.vc/
(use-package magit
  :ensure t
  :pin melpa-stable
  :defer t
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :defer t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :pin melpa
  :defer t
  :mode "\\.md\\'")

;; https://orgmode.org/
(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (js . t)
     (python . t))))

;; https://github.com/pashky/restclient.el
(use-package restclient
  :defer t
  :ensure t)

;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (require 'smartparens-config)
  (show-paren-mode t))

(provide 'packages)

;;; packages.el ends here
