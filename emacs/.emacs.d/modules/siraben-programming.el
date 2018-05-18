;;; siraben-programming.el -- siraben's programming configuration

;;; Commentary:
;; This file makes managing the different programming modes that
;; need to be loaded easier.


;;; Code:

(global-set-key (kbd "M-C") 'comment-or-uncomment-region)

;; (require 'which-func)

(defun siraben-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (define-key c-mode-base-map (kbd "s-b") 'recompile)
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (guru-mode t)
  (which-function-mode 1)
  (smartparens-mode t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook (lambda ()
                            (siraben-prog-mode-defaults)))

(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(require 'siraben-js)
(require 'siraben-c)
(require 'siraben-lisp)
(require 'siraben-prolog)
(require 'siraben-rust)


(provide 'siraben-programming)

;;; siraben-programming.el ends here
