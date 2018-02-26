;; -----------------
;; org-mode's Babel
;; -----------------
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (matlab . t)
   (octave . t)
   (python . t)
   (R . t)
   (scheme . t)
   ))

(provide 'dev-org-babel)
