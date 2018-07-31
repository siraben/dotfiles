;;; siraben-ui.el -- configure Emacs' visual appearance.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file configures the visual appearance of Emacs, loads
;; my favorite theme, and adds a smart mode line.

;;; Code:

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
;; (display-battery-mode t)
(use-package fancy-battery
  :config (setq fancy-battery-show-percentage t)
  :init (add-hook 'after-init-hook #'fancy-battery-mode))

(display-time-mode t)
(global-company-mode t)

;;(ido-mode t)

(setq frame-title-format
      '(""
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; Enable my favorite color scheme.
(use-package color-theme-sanityinc-tomorrow
  :demand
  :config (load-theme 'sanityinc-tomorrow-bright t))

;; Improve the mode line.
(use-package smart-mode-line
  :disabled
  :config (progn
            (add-hook 'after-init-hook #'sml/setup)
            (setq sml/no-confirm-load-theme t)
            (setq sml/theme nil)
            (add-to-list 'sml/replacer-regexp-list
                         '("^~/dotfiles/emacs/.emacs.d/" ":Emacs Config:"))))

(use-package spaceline
  :config (setq powerline-default-separator 'utf-8)
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (if (display-graphic-p)
                                     (progn
                                       (require 'spaceline-config)
                                       (spaceline-emacs-theme)
                                       (spaceline-helm-mode))))))



;; Remove the auto-revert mode-line
(require 'diminish)
(diminish 'auto-revert-mode)
(diminish 'flyspell-mode)
(diminish 'visual-line-mode "Visual Line")
(diminish 'auto-fill-function "Auto Fill")
(diminish 'eldoc-mode)
(diminish 'lisp-interaction-mode)
(diminish 'flycheck-mode)

(provide 'siraben-ui)
;;; siraben-ui.el ends here
