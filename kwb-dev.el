;; show me the line numbers in source
(defun add-line-numbers ()
  (linum-mode 1))

;; auto wrap, but only in comments
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (run-hooks 'dev-hook))

(provide 'kwb-dev)
