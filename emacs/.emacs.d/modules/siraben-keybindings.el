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

;; Customization of global keybindings.

;;; Code:

;; Custom functions
(global-set-key (kbd "M-T") 'siraben-insert-time)

;; Fullscreen behavior like iTerm2.
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(global-set-key (kbd "M-B") #'list-bookmarks)

(global-set-key (kbd "M-Q") #'unfill-paragraph)

;; (require 'inline-string-rectangle)

;; (require 'mark-more-like-this)

(global-set-key (kbd "C-x r t") 'string-rectangle)
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-:") 'eval-print-last-sexp)

(global-set-key (kbd "M-W") 'whitespace-cleanup)

(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

(provide 'siraben-keybindings)
;;; siraben-keybindings.el ends here
