;; siraben-shell.el

;; Configures Emacs Shell and Terminal modes.

(if (file-exists-p "/usr/bin/zsh")
    (setq-default shell-file-name "/usr/bin/zsh"))

(provide 'siraben-shell)
