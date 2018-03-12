;; siraben-lisp.el

;; Lisp-specific editing packages and other miscellanea.

(defun siraben-enable-lisp-editing-modes ()
  "Enables a collection of modes (such as paredit, rainbow delimiters,
aggressive indentation etc.) that greatly help with editing lisp
code."
  (interactive)
  (progn (setq show-paren-style 'mixed)
	 (paredit-mode t)
	 (rainbow-delimiters-mode t)
	 (aggressive-indent-mode t)
	 (show-paren-mode t)
	 (global-undo-tree-mode t)
	 (company-mode t)))

(setq siraben-lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook siraben-lispy-mode-hooks)
  (add-hook hook #'siraben-enable-lisp-editing-modes))

;; Enable some Lisp modes like paredit and rainbow delimiters, but no
;; need to undo and autocomplete.
(add-hook 'inferior-scheme-mode-hook 
	  #'(lambda ()
	      (siraben-enable-lisp-editing-modes)
	      (undo-tree-mode -1)))

(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(provide 'siraben-lisp)
