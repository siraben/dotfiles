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

(setq-default cursor-type 'box)

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
(require 'siraben-c)

;; Load configuration that is OS-specific 
(when (eq system-type 'darwin)
  (require 'siraben-macos))

(when (eq system-type 'gnu/linux)
  (require 'siraben-linux))

;; Initial scratch buffer message
(setq initial-scratch-message
      (format ";; Scratch buffer created on %s\n"
	      (shell-command-to-string "date '+%A, %B %d, %Y at %R'")))
