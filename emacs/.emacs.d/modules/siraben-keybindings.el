;; siraben-keybindings.el

;; This file sets up global keybindings. These keybindings are those
;; that weren't set up in `use-package' declarations.

;; Custom functions
(global-set-key (kbd "M-T") 'siraben-insert-time)

;; Fonts
(global-set-key (kbd "C-)") 'siraben-reset-font-size)
(global-set-key (kbd "C-+") 'siraben-increase-font-size)
(global-set-key (kbd "C-=") 'siraben-reset-font-size)
(global-set-key (kbd "C--") 'siraben-decrease-font-size)

;; UI
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Misc.

;; Fullscreen behavior like iTerm2
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(provide 'siraben-keybindings)
