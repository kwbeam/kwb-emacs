;; Window switching with shift-arrow
(windmove-default-keybindings)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)

;; File finding
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Show git status
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'kwb-bindings)
