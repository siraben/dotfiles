;; siraben-macos.el

;; A script to be run when the host OS is macOS

;; Install Scheme
(setq scheme-program-name "/usr/local/bin/chez")
(unless (file-exists-p scheme-program-name)
  (message "Installing Chez Scheme")
  (shell-command "brew install chezscheme"))

;; Install the spell-checker
(setq ispell-program-name "/usr/local/bin/aspell")
(unless (file-exists-p ispell-program-name)
  (message "Installing aspell")
  (shell-command "brew install aspell"))

(setq system-uses-terminfo nil)

(use-package exec-path-from-shell
  :demand
  :config (progn (setq exec-path-from-shell-check-startup-files nil)
                 (exec-path-from-shell-initialize)))

(provide 'siraben-macos)
