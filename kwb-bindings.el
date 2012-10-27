;; Window switching with shift-arrow
(windmove-default-keybindings)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; use interactive buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Always indent stuff
(define-key global-map (kbd "RET") 'newline-and-indent)

;; new line above and below
(define-key global-map (kbd "M-p") 'newline-previous)
(define-key global-map (kbd "M-n") 'newline-next)

(provide 'kwb-bindings)
