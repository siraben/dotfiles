;;; siraben-linux.el --- This file runs when the host OS is gnu/linux.

;;; Commentary:

;;; Code:

(set-if-exists scheme-program-name "/usr/bin/guile")
(set-if-exists ispell-program-name "/usr/bin/ispell")

(provide 'siraben-linux)

;;; siraben-linux.el ends here
