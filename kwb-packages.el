(defvar kwb-packages
  '(clojure-mode
    coffee-mode
    exec-path-from-shell
    expand-region
    feature-mode
    flymake-python-pyflakes
    flymake-ruby
    inf-ruby
    markdown-mode
    paredit
    python-mode
    rinari
    rvm
    scss-mode
    yasnippet)
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
