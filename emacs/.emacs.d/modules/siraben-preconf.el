;; siraben-preconf.el

;; Preconfigure Emacs minimally without packages.

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

(provide 'siraben-preconf)
