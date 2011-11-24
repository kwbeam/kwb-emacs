;; Window switching
(windmove-default-keybindings) ;; Shift+direction

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)

;; File finding
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; So good!
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'kwb-bindings)
