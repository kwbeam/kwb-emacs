;; -----------------
;; IDL
;; -----------------
(setq idlwave-shell-explicit-file-name "idl82")
(setq load-path (cons "~/tools/idlwave" load-path))
(autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
(autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
(setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))

(custom-set-variables
 '(idlwave-block-indent 3)
 '(idlwave-completion-case (quote ((routine . preserve) (keyword . downcase) (class . preserve) (method . preserve))))
 '(idlwave-completion-force-default-case t)
 '(idlwave-completion-show-classes 10)
 '(idlwave-continuation-indent 3)
 '(idlwave-do-actions t)
 '(idlwave-end-offset -3)
 '(idlwave-expand-generic-end t)
 '(idlwave-indent-to-open-paren nil)
 '(idlwave-init-rinfo-when-idle-after 2)
 '(idlwave-main-block-indent 3)
 '(idlwave-max-extra-continuation-indent 60)
 '(idlwave-pad-keyword t)
 '(idlwave-query-class (quote ((method-default) (keyword-default) ("INIT" . t) ("CLEANUP" . t) ("SETPROPERTY" . t) ("GETPROPERTY" . t))))
 '(idlwave-reserved-word-upcase nil nil nil "Want pro rather than PRO")
 '(idlwave-shell-automatic-electric-debug t)
 '(idlwave-shell-automatic-start t)
 '(idlwave-shell-debug-modifiers (quote (super)))
 '(idlwave-shell-electric-stop-color "darkviolet")
 '(idlwave-shell-reset-no-prompt t)
 '(idlwave-shell-separate-examine-output nil)
 '(idlwave-shell-show-commands (quote (run breakpoint debug misc)))
 '(idlwave-shell-use-dedicated-frame nil)
 '(idlwave-shell-use-dedicated-window nil)
 '(idlwave-surround-by-blank t))

(add-hook 'idlwave-mode-hook
	  (lambda ()

            ;; Skip over Underbars when word forward
            ;; (modify-syntax-entry ?_ "w")
            ;; 2011-09-16: <mhs>  I didn't like that. </mhs>
            ;; These insert spaces around the character.
            ;; alpha[beta] --> alpha[ beta ]
	    (idlwave-action-and-binding "["  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding "]"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "("  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding ")"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "*"  '(idlwave-surround -1 'nil 1))))

(defun my-common-idlwave-hook ()
  (local-set-key [(meta .)] 'idlwave-find-module)
  (local-set-key [(super z)] 'mhs-idlwave-shell-reset)
  (local-set-key [(meta s)] 'idlwave-complete)
  (local-set-key (kbd "C-M-?") 'idlwave-context-help))

(add-hook 'idlwave-mode-hook 'my-common-idlwave-hook 't)
(add-hook 'idlwave-shell-mode-hook 'my-common-idlwave-hook 't)

(provide 'dev-idl)
