;; siraben-keybindings.el

;; This file sets up global keybindings.

(global-set-key (kbd "M-T") #'siraben/insert-time)
(global-set-key (kbd "M-R") #'replace-string)

(global-set-key (kbd "C-)") #'siraben/reset-font-size)
(global-set-key (kbd "C-+") #'siraben/increase-font-size)
(global-set-key (kbd "C-=") #'siraben/reset-font-size)
(global-set-key (kbd "C--") #'siraben/decrease-font-size)

(provide 'siraben-keybindings)
