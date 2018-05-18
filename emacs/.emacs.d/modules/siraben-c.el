;;; siraben-c.el --- configures Emacs for C-code development

;;; Commentary:

;;; Code:
(require 'cc-mode)

;; Code auto completion packages for C code.
(use-package irony)
(use-package company-irony)
(use-package flycheck-irony)

(add-hook 'c-mode-hook #'(lambda ()
                           (irony-mode 1)
                           (flycheck-mode 1)
                           (smartparens-mode 1)))

;; I like having an easy key binding to recompile easily.
(define-key c-mode-base-map (kbd "s-b") 'recompile)

(provide 'siraben-c)

;;; siraben-c.el ends here
