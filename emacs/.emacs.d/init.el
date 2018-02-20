;; Welcome to Ben's Emacs Init file!

;; (package-initialize)

;; Load new bytecode always
(setq load-prefer-newer t)

;; Setting `gc-cons-threshold' high makes startup faster.
(setq gc-cons-threshold 50000000)

;; Right off the bat remove the eyesores.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))

(defvar ben-root-dir (file-name-directory load-file-name)
  "The root directory of my config file.")

(defvar ben-modules-dir (expand-file-name  "modules" ben-root-dir)
  "The directory that contains all the modules for my config.")

(add-to-list 'load-path ben-modules-dir)

;; Package setup
(require 'siraben-core)
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
