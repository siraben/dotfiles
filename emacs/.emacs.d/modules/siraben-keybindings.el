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
;; (global-set-key (kbd "C-)") 'siraben-reset-font-size)
;; (global-set-key (kbd "C-+") 'siraben-increase-font-size)
;; (global-set-key (kbd "C-=") 'siraben-reset-font-size)
;; (global-set-key (kbd "C--") 'siraben-decrease-font-size)

;; Retain old ibuffer behavior.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Fullscreen behavior like iTerm2.
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(global-set-key (kbd "M-B") #'list-bookmarks)

(global-set-key (kbd "M-Q") #'unfill-paragraph)

(require 'inline-string-rectangle)

;; `align-regexp' is useful. For instance:
;; - this
;;    - list of things
;;  - is not indented
;;       - to the same level!

;; Select this list of things and perform C-x \ - RET
;; - this
;; - list of things
;; - is not indented
;; - to the same level!

(require 'mark-more-like-this)
(defmacro siraben-set-key (kbd-string function)
  "Bind KBD-STRING to FUNCTION using `global-set-key'."
  `(global-set-key (kbd ,kbd-string) ,function))

(siraben-set-key "C-x r t" 'inline-string-rectangle)
(siraben-set-key "C-x \\" 'align-regexp)
(siraben-set-key "C-<" 'mark-previous-like-this)
(siraben-set-key "C->" 'mark-next-like-this)

;; Like the other two, but takes an argument (negative is previous)
(siraben-set-key "C-M-m" 'mark-more-like-this)
(siraben-set-key "s-G" 'mark-all-like-this)

(siraben-set-key "M-F" 'free-keys)
(siraben-set-key "C-:" 'eval-print-last-sexp)

(provide 'siraben-keybindings)
;;; siraben-keybindings.el ends here
