;; -----------------
;; Markdown
;; -----------------
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(cond ((eq system-type 'darwin)
       (setq markdown-open-command "/Applications/Mou.app/Contents/MacOS/Mou")))

(provide 'dev-markdown)
