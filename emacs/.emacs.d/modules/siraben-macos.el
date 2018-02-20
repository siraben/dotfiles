;; siraben-macos.el

;; A script to be run when the target OS is darwin.

(setq scheme-program-name "/usr/local/bin/chez")
(unless (file-exists-p scheme-program-name)
  (message "Installing Chez Scheme")
  (shell-command "brew install chezscheme"))
(setq ispell-program-name "/usr/local/bin/aspell")
(unless (file-exists-p ispell-program-name)
  (message "Installing aspell")
  (shell-command "brew install aspell"))
(defvar ben/default-font-size 12)
(setq system-uses-terminfo nil)
  
(provide 'siraben-macos)
