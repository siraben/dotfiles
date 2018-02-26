;; Welcome to siraben's Emacs Init file!

;; This file is the first file to be executed by Emacs.

;; Make package.el happy.
;; (package-initialize)

;; Always prefer the newest version of a file.
(setq load-prefer-newer t)

;; Reducing garbage collection makes startup faster.
(setq gc-cons-threshold 50000000)

;; At least remove the eyesores while we wait.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))

(defvar siraben-root-dir "~/dotfiles/emacs/.emacs.d/")

(defvar siraben-modules-dir (expand-file-name "modules" siraben-root-dir)
  "The directory that contains all the modules for my
configuration.")

(add-to-list 'load-path siraben-modules-dir)

(require 'siraben-packages)
(require 'siraben-ui)
(require 'siraben-core)
(require 'siraben-fonts)
(require 'siraben-keybindings)
(require 'siraben-editor)
(require 'siraben-org)

;; Load configuration that is OS-specific 
(when (eq system-type 'darwin)
  (require 'siraben-macos))

(when (eq system-type 'gnu/linux)
  (require 'siraben-linux))

;; Initial scratch buffer message
(setq initial-scratch-message
      (format ";; Scratch buffer created on %s\n"
	      (shell-command-to-string "date '+%A, %B %d, %Y at %R'")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell smart-mode-line color-theme-sanityinc-tomorrow emms smex helm pdf-tools auto-package-update fill-column-indicator free-keys magit neotree org-bullets markdown-mode writeroom-mode rainbow-delimiters company aggressive-indent undo-tree paredit diminish auto-compile use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
