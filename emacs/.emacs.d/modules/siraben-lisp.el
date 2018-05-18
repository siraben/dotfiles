;;; siraben-lisp.el --- Specific modes and packages for Lisp code.

;;; Commentary:

;;; Code:

(defun siraben-enable-lisp-editing-modes ()
  "Enables a collection of modes for editing Lisp code."
  (interactive)
  (progn (setq show-paren-style 'mixed)
         
         ;; Change to paredit because I haven't figured out how to get
         ;; smartparens to stop trying to pair single quotes.
         
         (smartparens-mode -1)
	 (paredit-mode t)
	 (rainbow-delimiters-mode t)
	 (aggressive-indent-mode t)
	 (show-paren-mode t)
	 (global-undo-tree-mode t)
	 (company-mode t)))

(defvar siraben-lispy-mode-hooks
  '(clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    scheme-mode-hook))

(dolist (hook siraben-lispy-mode-hooks)
  (add-hook hook #'(lambda ()
                     (siraben-enable-lisp-editing-modes))))

;; Enable some Lisp modes like paredit and rainbow delimiters, but no
;; need to undo and auto complete.
(add-hook 'geiser-repl-mode-hook
          #'(lambda ()
              (siraben-enable-lisp-editing-modes)
              (undo-tree-mode -1)
              (aggressive-indent-mode -1)))

(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(provide 'siraben-lisp)
;;; siraben-lisp.el ends here
