;;; dev-trad --- Dev modes setup -- traditional
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------
;; JavaScript & TypeScript
;; ----------------------------------------------------------
;; https://github.com/mooz/js2-mode/
; (use-package js2-mode
;   :ensure t
;   :pin melpa-stable
;   :mode ("\\.js\\'" . js2-mode)
;   :config
;   (setq-default js-indent-level 2)
;   (setq-default js2-basic-offset 2))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :after (js-mode)
  :init
  (add-hook 'js-mode-hook #'add-node-modules-path))

;; https://github.com/NicolasPetton/indium
;; https://indium.readthedocs.io/en/latest/index.html
(use-package indium
  :ensure t
  :pin melpa-stable
  :hook (js-mode . indium-interaction-mode))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t)

;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :after (typescript-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; ----------------------------------------------------------
;; Python
;; ----------------------------------------------------------
(use-package elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

; (use-package blacken
;   :ensure t
;   :pin melpa
;   :hook (python-mode . blacken-mode))

;; (use-package company-jedi
;;   :ensure t
;;   :pin melpa-stable
;;   :hook (python-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))

;; http://millejoh.github.io/emacs-ipython-notebook/
;(use-package ein
;  :ensure t
;  :pin melpa
;  :config
;  (require 'ein-notebook)
;  (require 'ein-subpackages)
;  (setq ein:enable-keepalive t)
;  (setq ein:notebooklist-keepalive-refresh-time 0.25)
;  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;  ;;(setq ein:completion-backend 'ein:use-company-backend)
;  )

(provide 'dev-trad)

;;; dev-trad.el ends here
