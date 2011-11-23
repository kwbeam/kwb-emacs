;; ***************************************************************************
;; Gandalf (Mac) Specific Stuff
;; ***************************************************************************

;; Fix up the Mac so the option key is nothing and command is meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; set geometry
(add-to-list 'default-frame-alist '(height . 53))
(add-to-list 'default-frame-alist '(width . 180))
