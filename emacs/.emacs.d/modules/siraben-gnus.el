;;; siraben-gnus.el --- configure Gnus.

;;; Commentary:

;; Currently quite bare because I haven't read the Gnus manual yet.

;;; Code:

(require 'gnus)

;; Refer to secret.el for email address, full name and server

(add-hook 'mail-mode-hook #'siraben-enable-writing-modes)

(provide 'siraben-gnus)
;;; siraben-gnus.el ends here
