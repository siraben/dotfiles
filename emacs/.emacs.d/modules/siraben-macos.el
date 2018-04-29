;; siraben-macos.el

;; This file runs when the host OS is macOS.

(set-if-exists scheme-program-name "/usr/local/bin/guile")
(set-if-exists ispell-program-name "/usr/local/bin/aspell")
(setq system-uses-terminfo nil)

(provide 'siraben-macos)

;; End of siraben-macos.el
