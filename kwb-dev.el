;; show me the line numbers in source
(defun add-line-numbers ()
  (linum-mode 1))

;; don't auto wrap unless we're in a comment
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

;; highlight every occurrence of the word we're at
(defun turn-on-idle-highlight ()
  (idle-highlight-mode t))

(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)
(add-hook 'dev-hook 'turn-on-idle-highlight)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (run-hooks 'dev-hook))

(provide 'kwb-dev)
