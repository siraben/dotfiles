;;; siraben-macos.el --- This files runs when the host OS is macOS.

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

;; Currently only simple customizations are being made.

;;; Code:

(require 'ispell)
(setq ispell-program-name  "aspell")

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (require 'mu4e)

(setq system-uses-terminfo nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-fontset-font t 'unicode "Apple Color Emoji" nil 'append)

;; Disable native compilation when on battery power
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "AC"
                                (battery-format "%L"
                                                (funcall battery-status-function)))))
  (setq no-native-compile t))

(global-unset-key (kbd "s-t"))

(provide 'siraben-macos)

;;; siraben-macos.el ends here
