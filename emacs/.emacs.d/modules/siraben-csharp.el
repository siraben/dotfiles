;;; siraben-csharp.el --- configures Emacs for C# development

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

;; Code auto completion packages for C code.
(use-package csharp-mode)

(add-hook 'csharp-mode-hook #'(lambda ()
                                (electric-pair-local-mode t)
                                (electric-indent-mode t)))

(provide 'siraben-csharp)

;;; siraben-csharp.el ends here
