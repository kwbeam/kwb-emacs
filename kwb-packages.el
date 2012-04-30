(defvar kwb-packages
  '(clojure-mode color-theme color-theme-blackboard expand-region
                 feature-mode flymake-ruby inf-ruby rvm
                 markdown-mode scss-mode)
  "A list of packages to ensure are installed at launch.")

(defun kwb-packages-installed-p ()
  (loop for p in kwb-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (kwb-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p kwb-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'kwb-packages)