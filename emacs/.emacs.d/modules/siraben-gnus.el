(require 'gnus)

;; Refer to secret.el for email address, full name and server

(add-hook 'mail-mode-hook #'siraben-enable-writing-modes)

(provide 'siraben-gnus)
