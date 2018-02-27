;; siraben-c.el

;; Configures Emacs for C-code development

(use-package irony)
(use-package company-irony)
(use-package flycheck-irony)

(add-hook 'c-mode-hook #'irony-mode)

(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

(provide 'siraben-c)
