;;; siraben-macos.el --- macOS-specific Emacs configuration -*- lexical-binding: t; -*-

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

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defun siraben-macos-emoji-frame (&optional frame)
  "Configure emoji only on graphical FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'append))))
(add-hook 'after-init-hook #'siraben-macos-emoji-frame)
(add-hook 'after-make-frame-functions #'siraben-macos-emoji-frame)

(global-unset-key (kbd "s-t"))

(provide 'siraben-macos)

;;; siraben-macos.el ends here
