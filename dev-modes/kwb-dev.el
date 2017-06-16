;;; init --- kwbeam emacs development configuration
;;; Commentary:

;; Setup Emacs for all the programming modes I use.

;;; Code:

;; -----------------
;; general stuff
;; -----------------
(yas-global-mode 1)

;; Use projectile for everything
(projectile-global-mode)

;; -----------------
;; dev hook
;; -----------------
(defun add-line-numbers ()
  (linum-mode 1))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun dev-before-save-hook ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)
(add-hook 'dev-hook 'dev-before-save-hook)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (interactive)
  (run-hooks 'dev-hook))

;; -----------------------
;; language-specific setup
;; -----------------------
(require 'dev-javascript)
(require 'dev-json)
(require 'dev-magit)
(require 'dev-markdown)
(require 'dev-org-babel)
(require 'dev-python)
(require 'dev-typescript)
(require 'dev-web)

(provide 'kwb-dev)

;;; kwb-dev.el ends here
