;; siraben-ui.el
;; This file configures the visual appearance of Emacs, loads
;; my favorite theme, and adds a smart mode line.

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(display-battery-mode t)
(display-time-mode t) 
(global-company-mode t)

(ido-mode t)

(setq frame-title-format
      '("" invocation-name " Siraben - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(beacon-mode +1)

(use-package color-theme-sanityinc-tomorrow
  :demand
  :config (load-theme 'sanityinc-tomorrow-night t))

(use-package smart-mode-line
  :demand
  :config (progn (setq sml/no-confirm-load-theme t)
		 (setq sml/theme nil)))

(add-hook 'after-init-hook #'sml/setup)

(provide 'siraben-ui)
