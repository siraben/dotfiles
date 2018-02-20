;; siraben-keybindings.el

;; This file sets up my collection of keybindings

(global-set-key (kbd "M-T") #'ben/insert-time)
(global-set-key (kbd "M-R") #'replace-string)
(global-set-key (kbd "C-)") #'ben/reset-font-size)
(global-set-key (kbd "C-+") #'ben/increase-font-size)
(global-set-key (kbd "C-=") #'ben/reset-font-size)
(global-set-key (kbd "C--") #'ben/decrease-font-size)

(provide 'siraben-keybindings)
