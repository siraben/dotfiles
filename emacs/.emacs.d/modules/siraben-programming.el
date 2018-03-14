;; siraben-programming.el

;; This file makes managing the different programming modes that
;; need to be loaded easier.

(global-set-key (kbd "M-C") 'comment-or-uncomment-region)

(require 'siraben-js)
(require 'siraben-c)
(require 'siraben-lisp)
(require 'siraben-prolog)

(provide 'siraben-programming)
