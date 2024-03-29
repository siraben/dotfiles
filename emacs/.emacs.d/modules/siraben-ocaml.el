;;; siraben-ocaml.el --- configures Emacs for OCaml development

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

(use-package flycheck-ocaml)
(use-package tuareg
  :hook ((tuareg-mode .
                      (lambda ()
                        (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                                      (lambda (capture-name)
                                        (not (string= capture-name "variable"))))))))


(provide 'siraben-ocaml)
;;; siraben-ocaml.el ends here
