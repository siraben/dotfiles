;;; siraben-ocaml.el --- Configure Emacs for OCaml development -*- lexical-binding: t; -*-

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

;; Configure Tuareg with capability-driven Eglot diagnostics.

;;; Code:

(use-package tuareg
  :hook (tuareg-mode . siraben-eglot-ensure-if-server-available))

(provide 'siraben-ocaml)
;;; siraben-ocaml.el ends here
