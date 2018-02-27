;; siraben-ui.el

;; This file configures the visual appearance of Emacs, loads
;; my favorite theme, and adds a smart mode line.

;; Remove the annoying blinking cursor and bell ring.
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Nice scrolling.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Enable short answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Extra mode line modes.
(display-battery-mode t)
(display-time-mode t) 
(global-company-mode t)

(ido-mode t)
(desktop-save-mode 1)

(setq frame-title-format
      '("" invocation-name " Siraben - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; Enable my favorite color scheme
(use-package color-theme-sanityinc-tomorrow
  :demand
  :config (load-theme 'sanityinc-tomorrow-night t))

;; Improve the mode line
(use-package smart-mode-line
  :demand
  :config (progn (setq sml/no-confirm-load-theme t)
		 (setq sml/theme nil)))

;; Remove the auto-revert mode-line
(diminish 'auto-revert-mode)

(add-hook 'after-init-hook #'sml/setup)

(provide 'siraben-ui)
