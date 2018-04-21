;;; dev-clojure --- Setup emacs for Clojure
;;; Commentary:
;; -----------------
;; Clojure
;; -----------------
;; Prerequisite Emacs packages:
;;   * cider
;;   * clojure-mode
;;   * clojure-mode-extra-font-locking
;;   * smartparens
;; Prerequisite language installs:
;;   * JDK 8
;;   * Leiningen
;;; Code:

(require 'smartparens-config)
(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
(sp-use-paredit-bindings)

(provide 'dev-clojure)
;;; dev-clojure.el ends here
