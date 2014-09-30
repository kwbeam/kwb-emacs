;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (js . t)
   (lisp . t)
   (python . t)
   (ruby . t)
   (scheme . t)
   ))

(provide 'dev-org-babel)
