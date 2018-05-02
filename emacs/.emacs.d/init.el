;; Welcome to siraben's Emacs init file!
;; This is the first to be executed by Emacs.

;; init.el

;; Always prefer the newest version of a file, even if the old one is
;; compiled.

(setq load-prefer-newer t)

;; Reducing garbage collection makes startup faster.
(setq gc-cons-threshold 50000000)

;; At least remove the eyesores while we wait.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

(when window-system
  (scroll-bar-mode -1))

;; And ensure the cursor is a box, and remove the fringe.
(setq-default cursor-type 'box)

(fringe-mode 0)

(defvar siraben-root-dir
  "~/dotfiles/emacs/.emacs.d/"
  "The root directory of the Emacs configuration.")

(defvar siraben-modules-dir
  (expand-file-name "modules" siraben-root-dir)
  "The directory that contains all the modules for my
configuration.")

(add-to-list 'load-path siraben-modules-dir)

;; This is the first to be executed by Emacs.

;; Make package.el happy. Drop this line in Emacs 27 as it will no
;; longer be needed.
;; (package-initialize)

(require 'siraben-core)
(require 'siraben-packages)
(require 'siraben-ui)
(require 'siraben-fonts)
(require 'siraben-keybindings)
(require 'siraben-editor)
(require 'siraben-programming)
(require 'siraben-gnus)
(require 'siraben-shell)
(require 'siraben-org)
(require 'siraben-arcadia)

;; Load configuration that is OS-specific.
(when (eq system-type 'darwin)
  (require 'siraben-macos))

(when (eq system-type 'gnu/linux)
  (require 'siraben-linux))

;; Initial scratch buffer message.
(setq initial-scratch-message
      (format ";; Scratch buffer created on %s\n"
	      (shell-command-to-string "date '+%A, %B %d, %Y at %R'")))

;; Keep some things out of version control.
(let ((secret.el (expand-file-name "secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
)

