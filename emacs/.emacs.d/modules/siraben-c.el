;;; siraben-c.el --- configures Emacs for C-code development

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

;;; Code:
(require 'cc-mode)

;; Code auto completion packages for C code.
(use-package irony)
(use-package company-irony)
(use-package flycheck-irony)

(add-hook 'c-mode-hook #'(lambda ()
                           (irony-mode 1)
                           (flycheck-mode 1)
                           (smartparens-mode 1)))

;; I like having an easy key binding to recompile easily.
(define-key c-mode-base-map (kbd "s-b") 'recompile)

(provide 'siraben-c)

;;; siraben-c.el ends here
