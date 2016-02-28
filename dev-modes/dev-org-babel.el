;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (lisp . t)
   (python . t)
   (scheme . t)
   ))

(provide 'dev-org-babel)
