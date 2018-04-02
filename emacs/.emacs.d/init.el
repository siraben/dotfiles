;; Welcome to siraben's Emacs init file!

;; init.el

;; This is the first to be executed by Emacs.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make package.el happy. Drop this line in Emacs 27 as it will no
;; longer be needed.
;; (package-initialize)

(require 'siraben-preconf)
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

