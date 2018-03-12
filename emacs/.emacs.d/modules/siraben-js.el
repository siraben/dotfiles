;; siraben-js.el

;; This file handles working with JavaScript.

(use-package js2-mode
  :config 
  (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(add-hook 'js-mode-hook
          #'(lambda ()
              (smartparens-strict-mode 1)
              (js2-mode 1)
              (setq-local electric-layout-rules '((?\; . after)))
              (js2-imenu-extras-mode 1)))

(provide 'siraben-js)
