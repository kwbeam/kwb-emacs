;;; init --- kwbeam emacs configuration
;;; Commentary:

;; In the beginning...
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:

;; -------------------------------------
;; Setup package loading
;; -------------------------------------
;; Bootstrap everything with package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Use use-package for all other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; time your emacs init
;;   M-x benchmark-init/show-durations-tree
;;   M-x benchmark-init/show-durations-tabulated
(use-package benchmark-init :ensure t)

;; -------------------------------------
;; Minimal Emacs Customization
;; -------------------------------------
(server-start)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(column-number-mode t)
(setq select-enable-clipboard t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq uniquify-buffer-name-style 'forward)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(set-default 'indicate-empty-lines t)
(set-default 'indent-tabs-mode nil)
(setq tab-width 2)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1)
(setq create-lockfiles nil)
(global-auto-revert-mode t)
(defalias 'auto-revert-tail-mode 'tail-mode)
(random t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq custom-file (locate-user-emacs-file ".gnu-emacs-custom"))
(load custom-file t t)
(setq browse-url-browser-function 'eww-browse-url)

;; Use ido mode
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode t)

;; keybindings
(windmove-default-keybindings)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; new line above and below
;; hat-tip: http://blog.peepcode.com/blog/2012/commanding-your-text-editor/
(defun newline-previous ()
  "Insert a blank line above the cursor and move the cursor up one line."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))
(defun newline-next ()
  "Insert an indented newline after the current line and move the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(define-key global-map (kbd "M-p") 'newline-previous)
(define-key global-map (kbd "M-n") 'newline-next)

;; -------------------------------------
;; Define packages
;; -------------------------------------
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :pin melpa-stable
  :config
  (load-theme 'sanityinc-tomorrow-blue t))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :pin melpa-stable
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :config
  (require 'smartparens-config)
  (show-paren-mode t))
  
(use-package git-timemachine
  :ensure t
  :pin melpa-stable)
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

(use-package company
  :ensure t
  :pin melpa-stable
  :config
  (global-company-mode)
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0)
  (global-set-key (kbd "<C-tab>") 'company-complete))

(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.md\\'")

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (haskell . t)
     (js . t)
     (lisp . t)
     (python . t)
     (scheme . t))))

;; TODO: Use this in all the dev mode
(defun kwb-dev-hook ()
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Haskell
;; Prerequisite Language installs:
;;   * Stack / GHC
(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.hs\\'" . haskell-mode))
(use-package intero
  :ensure t
  :pin melpa-stable
  :after (haskell-mode)
  :hook haskell-mode)

;; Lisp
;; Prerequisite Language installs:
;;   * CLisp
;;   * SBCL
(use-package slime
  :ensure t
  :pin melpa-stable
  :after lisp-mode
  :defer t
  :init
  (setq slime-lisp-implementations
      '((sbcl ("clisp"))
        (clisp ("sbcl"))))
  (setq slime-contribs '(slime-fancy)))
(use-package slime-company
  :ensure t
  :pin melpa-stable
  :defer t
  :after (company slime))


;; Scheme
;; Prerequisite Scheme installs:
;;   * MIT/GNU Scheme 9.1.1 or better
;;   * Racket 6.0 or better
(use-package geiser
  :ensure t
  :pin melpa-stable
  :defer t
  :after (scheme)
  :init
  (setq geiser-active-implementations '(mit racket)))

;; JavaScript
;; Prerequisite JS stuff:
;;   * ES5:
;;     * nvm
;;     * nvm install --lts
;;     * mkdir foo && cd foo
;;     * npm init -y
;;     * npm install --save-dev eslint tern
;;     * npx eslint --init
;;   * ESnext:
;;     * ES5, and
;;     * npm install --save-dev babel-cli babel-preset-env
;;     * echo "{'presets': ['env']}" > .babelrc
;;     * npm install --save babel-polyfill
;;   * React:
;;     * ES5 or ES6, and
;;     * npm install eslint-plugin-react --save-dev
;;     * npm install babel-preset-react --save-dev
;; Notes:
;;   When using babeljs for ES6, set a dir local variable so you
;;   can use the nodejs-repl via C-c C-z or M-x nodejs-repl.  The
;;   project's .dir-locals.el file should be at the top of the
;;   project and contain something like this:
;;     ((js2-mode (nodejs-repl-command
;;                 .
;;                 "/home/kwbeam/labs/js-lab/node_modules/.bin/babel-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * set nodejs-repl-command by finding it in the project's directory tree!
(use-package js2-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure
  :pin melpa-stable
  :after (js2-mode)
  :hook js2-mode)
                  
;; (use-package nosejs-repl
;;   :ensure t
;;   :pin melpa-stable
;;   :after (js2-mode))
;;   :hook (js2-mode)
;;   :bind (("C-x C-e" . 'nodejs-repl-send-last-sexp)
;;          ("C-cr" . 'nodejs-repl-send-region)
;;          ("C-cb" . 'nodejs-repl-send-buffer)
;;          ("C-c l" . 'nodejs-repl-load-file)
;;          ("C-c C-z" . 'nodejs-repl-switch-to-repl)))

;; (use-package tern
;;   :ensure t
;;   :pin melpa-stable
;;   :after (company js2-mode)
;;   :hook ((js2-mode . tern-mode)
;;          (nodejs-repl . tern-mode)))

;; (use-package company-tern
;;   :ensure t
;;   :pin melpa-stable
;;   :defer t
;;   :after (company)
;;   :config
;;   (add-to-list 'company-backends 'company-tern))

;; TypeScript
;; Prerequisite JS stuff:
;;   * nvm
;;   * nvm install --lts
;;   * mkdir foo && cd foo
;;   * npm init -y
;;   * npm install -D @types/node  # workaround for ts-node -- check again
;;   * npm install -D ts-node
;;   * npm install -D ts-lint typescript
;;   * npx tsc --init
;; Notes:
;;   When using a repl via ts-comint's M-x run-ts, set a dir local
;;   variable to point to the correct ts-node executable.  The project's
;;   .dir-locals.el file should be at the top of the project and
;;   contain something like this:
;;     ((typescript-mode (ts-comint-program-command
;;                        .
;;                        "/home/kwbeam/labs/ts-lab/node_modules/.bin/ts-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * Set the ts-comint-program-command by finding it in the
;;       project's directory tree!
(use-package tide
  :ensure t
  :pin melpa-stable
  :after (company flycheck)
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-executable "node_modules/.bin/tsserver"))

;; (use-package ts-comint
;;   :ensure t
;;   :pin melpa-stable
;;   :after (tide)
;;   :defer t
;;   :bind (("C-x C-e" . 'ts-send-last-sexp)
;;          ("C-c r" . 'ts-send-region-and-go)
;;          ("C-c b" . 'ts-send-buffer)
;;          ("C-M-x" . 'ts-send-last-sexp-and-go)
;;          ("C-c C-b" . 'ts-send-buffer-and-go)
;;          ("C-c l" . 'ts-load-file-and-go)))

;; TSX: Use tide & web-mode
;; (require 'web-mode)
;; (add-hook 'web-mode-hook 'run-dev-hook)
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; Web
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-enable-auto-pairing t)
;;   (setq web-mode-enable-auto-pairing nil)
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
;; (add-hook 'web-mode-hook 'run-dev-hook)

;; (defun sp-web-mode-is-code-context (id action context)
;;   (and (eq action 'insert)
;;        (not (or (get-text-property (point) 'part-side)
;;                 (get-text-property (point) 'block-side)))))
;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
;; Use eslint
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

;; Python
;; Prerequisite language installs:
;;   * pyenv
;;   * pyenv install <python version>
;;   * pyenv (global|local) <python version>
;;   * pip install pipenv
;; Project setup:
;;   $ pipenv install jedi flake8
(use-package elpy
  :ensure t
  :pin melpa-stable
  :hook (python-mode . elpy-mode))

;; (defun kwb/python-mode-hook ()
;;   "Setup all my Python stuff when we enter python mode."
;;   (pipenv-mode)
;;   (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
;;   (pyvenv-mode -1)
;;   (smartparens-mode)
;;   (add-to-list 'company-backends 'company-jedi)
;;   (setq python-indent-guess-indent-offset-verbose nil)
;;   (local-set-key "\C-ca" 'nosetests-all)
;;   (local-set-key "\C-cm" 'nosetests-module)
;;   (local-set-key "\C-c." 'nosetests-one)
;;   (local-set-key "\C-cpa" 'nosetests-pdb-all)
;;   (local-set-key "\C-cpm" 'nosetests-pdb-module)
;;   (local-set-key "\C-cp." 'nosetests-pdb-one))

;; (add-hook 'python-mode-hook 'kwb/python-mode-hook)

;; (add-hook 'python-mode-hook 'run-dev-hook)

;; ;; don't use flymake (elpy default), use flycheck
;; ;; from: https://github.com/jorgenschaefer/elpy/issues/137
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; ein - emacs ipython notebook
;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;; (add-to-list 'company-backends 'ein:company-backend)
  
;; HERE BE DRAGONS
;; (add-to-list 'safe-local-variable-values '(lexical-binding . t))
