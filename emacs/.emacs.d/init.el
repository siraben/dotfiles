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

(setq custom-file (concat siraben-root-dir "/custom.el"))

(defvar siraben-modules-dir
  (expand-file-name "modules/" siraben-root-dir)
  "The directory that contains all the modules for my configuration.")

(add-to-list 'load-path siraben-modules-dir)

;; This is the first file to be executed by Emacs.

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

;; (let ((secret.el "~/Nextcloud/mu4e.el"))
;;   (when (file-exists-p secret.el)
;;     (load secret.el)))

;;; init.el ends here
