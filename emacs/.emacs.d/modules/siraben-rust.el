;; siraben-rust.el

;; Configures packages and various modes for Rust programming.

(use-package rust-mode)
(use-package racer)
(use-package flycheck-rust)
(use-package cargo)

(setq rust-format-on-save t)

(eval-after-load 'rust-mode
  '(progn
     (add-hook 'rust-mode-hook 'racer-mode)
     (add-hook 'racer-mode-hook 'eldoc-mode)
     (add-hook 'rust-mode-hook 'cargo-minor-mode)
     (add-hook 'rust-mode-hook 'flycheck-rust-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

     (defun siraben-rust-mode-defaults ()
       (local-set-key (kbd "C-c C-d") 'racer-describe)
       (local-set-key (kbd "s-b") 'recompile)
       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq siraben-rust-mode-hook 'siraben-rust-mode-defaults)

     (add-hook 'rust-mode-hook (lambda ()
                                 (run-hooks 'siraben-rust-mode-hook)))))

(provide 'siraben-rust)
