;; siraben-c.el

;; Configures Emacs for C-code development

(use-package irony)
(use-package company-irony)
(use-package flycheck-irony)

(add-hook 'c-mode-hook #'(lambda ()
                           (irony-mode 1)
                           (flycheck-mode 1)))

(define-key c-mode-map (kbd "s-b") #'recompile)

(provide 'siraben-c)
