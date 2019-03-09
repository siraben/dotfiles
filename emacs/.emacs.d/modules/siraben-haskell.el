;;; siraben-programming.el -- siraben's programming configuration

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

;; This file installs Haskell related packages and hooks for various modes.

;;; Code:

(use-package haskell-mode)
(use-package flycheck-haskell)

(add-hook 'haskell-mode-hook
          #'(lambda ()
              (subword-mode             t)
              (eldoc-mode               t)
              (interactive-haskell-mode t)))

(add-hook 'inferior-haskell-mode-hook
          #'(lambda ()
              (paredit-mode -1)))

(provide 'siraben-haskell)
;;; siraben-haskell.el ends here
