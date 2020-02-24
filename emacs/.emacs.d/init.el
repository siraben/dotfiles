;;; init.el --- Entry point for siraben's Emacs

;;; License:

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

;; Welcome to siraben's Emacs init file!
;; This is the first to be executed by Emacs.

;;; Code:

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
  (scroll-bar-mode -1)
  ;; And ensure the cursor is a box, and remove the fringe.
  (setq-default cursor-type 'box)
  (fringe-mode 0))


(defvar siraben-root-dir
  "~/.emacs.d/"
  "The root directory of the Emacs configuration.")

(defvar siraben-modules-dir
  (expand-file-name "modules/" siraben-root-dir)
  "The directory that contains all the modules for my configuration.")

(add-to-list 'load-path siraben-modules-dir)

;; This is the first to be executed by Emacs.

;; TODO: Drop this line in Emacs 27.
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
(require 'siraben-tramp)
(require 'siraben-arcadia)
(require 'siraben-calc)
(require 'siraben-midnight)

;; Load configuration that is OS-specific.
(require
 (cl-case system-type
   (gnu/linux  'siraben-linux)
   (darwin     'siraben-macos)))

;; Initial scratch buffer message.
(setq initial-scratch-message
      (format ";; Session started on %s\n"
	      (shell-command-to-string "date +'%A, %F at %R'")))

;; Keep some things out of version control.
(let ((secret.el "~/Nextcloud/secret.el"))
  (when (file-exists-p secret.el)
    (load secret.el)))

(let ((secret.el "~/Nextcloud/mu4e.el"))
  (when (file-exists-p secret.el)
    (load secret.el)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-bound-variable-face ((t nil)))
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#b58900"))))
 '(agda2-highlight-datatype-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-dotted-face ((t nil)))
 '(agda2-highlight-error-face ((t (:foreground "#dc322f" :underline t))))
 '(agda2-highlight-field-face ((t (:foreground "#dc322f"))))
 '(agda2-highlight-function-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-incomplete-pattern-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#859900"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#cb4b16"))))
 '(agda2-highlight-module-face ((t (:foreground "#6c71c4"))))
 '(agda2-highlight-number-face ((t (:foreground "#6c71c4"))))
 '(agda2-highlight-operator-face ((t nil)))
 '(agda2-highlight-postulate-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-record-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-string-face ((t (:foreground "#d33682"))))
 '(agda2-highlight-symbol-face ((((background "#fdf6e3")) (:foreground "#586e75"))))
 '(agda2-highlight-termination-problem-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-typechecks-face ((t (:background "#2aa198" :foreground "#002b36"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:background "#002b36" :foreground "#b58900"))))
 '(agda2-highlight-unsolved-meta-face ((t (:background "#002b36" :foreground "#b58900")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (typescript-mode yasnippet-snippets xkcd writeroom-mode writegood-mode which-key webpaste use-package undo-tree svg-clock spaceline slime rainbow-identifiers rainbow-delimiters racket-mode racer proof-general pdf-tools paredit paradox org-wc ob-sml nix-mode neotree multiple-cursors memory-usage mark-multiple magit lorem-ipsum ledger-mode js2-mode htmlize helm-ag google-c-style gnuplot geiser free-keys forth-mode flycheck-rust flycheck-irony flycheck-haskell fill-column-indicator fancy-battery exec-path-from-shell erc-view-log emojify edit-indirect ediprolog diminish demo-it csharp-mode company-irony company-emoji company-coq color-theme-sanityinc-tomorrow cider cargo auto-package-update aggressive-indent))))

;;; init.el ends here
