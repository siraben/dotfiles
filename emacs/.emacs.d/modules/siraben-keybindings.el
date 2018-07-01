;;; siraben-keybindings.el -- Customize keybindings

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

;; This file sets up global keybindings. These keybindings are those
;; that weren't set up in `use-package' declarations.

;;; Commentary:

;; Mostly convenience driven.

;;; Code:

;; Custom functions
(global-set-key (kbd "M-T") 'siraben-insert-time)

;; Fonts - I'm debating whether I should just use the default keybindings.
(global-set-key (kbd "C-)") 'siraben-reset-font-size)
(global-set-key (kbd "C-+") 'siraben-increase-font-size)
(global-set-key (kbd "C-=") 'siraben-reset-font-size)
(global-set-key (kbd "C--") 'siraben-decrease-font-size)

;; Retain old ibuffer behavior.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun unfill-paragraph (&optional region)
  "Take a REGION and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    ;; This would override `fill-column' if it was an integer.
    (emacs-lisp-docstring-fill-column t))
  (fill-paragraph nil region))


;;; Miscellaneous.

;; Fullscreen behavior like iTerm2.
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(provide 'siraben-keybindings)
;;; siraben-keybindings.el ends here
