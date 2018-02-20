;; Welcome to Ben's Emacs Init file!
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Setting `gc-cons-threshold' high makes startup faster.
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Right off the bat remove the eyesores.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))

(add-to-list 'load-path "~/.emacs.d/modules")

;; Package setup
(require 'siraben-fonts)
(require 'siraben-packages)
(require 'siraben-ui)
(require 'siraben-keybindings)
(require 'siraben-editor)
;; OS-specific settings

(when (eq system-type 'darwin)
  (require 'siraben-macos))

(when (eq system-type 'gnu/linux)
  (require 'siraben-linux))

(setq initial-scratch-message
      (format ";; Scratch buffer created on %s\n"
	      (shell-command-to-string "date '+%A, %B %d, %Y at %R'")))
