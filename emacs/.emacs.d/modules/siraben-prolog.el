;; siraben-prolog.el

;; This file fixes the broken built-in Prolog editing modes that Emacs
;; provides.

(use-package ediprolog
  :config (global-set-key [f10] 'ediprolog-dwim))

(add-hook 'after-init-hook
          '(lambda ()
             (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
             (add-to-list 'auto-mode-alist '("\\.P\\'" . prolog-mode))))

(provide 'siraben-prolog)
