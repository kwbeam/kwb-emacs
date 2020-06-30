;;; js-indium --- add indium as the IDE
;;; Commentary:
;;; Code:

;; Connect to it in Emacs with `M-x indium-connect-to-nodejs` and specify
;; the IP address, port, and URL path on which the babel-node process is
;; listening. To open a REPL, `M-x indium-run-node`.

;; https://github.com/NicolasPetton/indium
;; https://indium.readthedocs.io/en/latest/index.html
(use-package indium
  :ensure t
  :pin melpa-stable
  :defer t
  :hook (js2-mode . indium-interaction-mode))

(provide 'js-indium)

;;; js-indium.el ends here
