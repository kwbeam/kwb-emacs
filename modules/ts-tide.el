;;; ts-tide --- basic set of typescript
;;; Commentary:
;;; Code:

;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'ts-tide)

;;; ts-tide.el ends here
