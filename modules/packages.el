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
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (global-set-key (kbd "<C-tab>") 'company-complete))

(use-package crux
  :ensure t
  :bind (("M-p" . crux-smart-open-line-above)
         ("M-n" . crux-smart-open-line)))

;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :pin melpa-stable
  :init (global-flycheck-mode))

;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :ensure t
  :pin melpa-stable)

;; https://magit.vc/
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :pin melpa
  :mode "\\.md\\'")

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(provide 'packages)

;;; packages.el ends here
