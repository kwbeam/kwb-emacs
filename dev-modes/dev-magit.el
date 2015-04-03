;;; dev-magit.el ---                                 -*- lexical-binding: t; -*-

;;; Code:
;; Stolen from flamingbear: https://raw.githubusercontent.com/flamingbear/emacs-config/master/site-lisp/lisp/mhs-magit.el
;; Originally idea for Github PR Stolen from here:
;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html?source=rss


(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (let ((repo (magit-get "remote" (magit-get-current-remote) "url")))
    (if (string-match "github\\.com" repo)
        (visit-gh-pull-request repo)
      (visit-bb-pull-request repo))))

(defun visit-gh-pull-request (repo)
  (browse-url
   (format "https://github.com/%s/compare/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (magit-get-current-branch))))



;; Bitbucket pull requests are kinda funky, it seems to try to just do the
;; right thing, so there's no branches to include.
;; https://bitbucket.org/nsidc/measures-byu-vm/pull-request/new
(defun visit-bb-pull-request (repo)
  (browse-url
   (format "https://bitbucket.org/%s/pull-request/new"
           (replace-regexp-in-string
            "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
            repo))))

;; visit PR for github or bitbucket repositories with "V"
(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))


;; Yes, magit, I acknowledge. Roger, ten-four. Over-and-out.
(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'dev-magit)
;;; dev-magit.el ends here
