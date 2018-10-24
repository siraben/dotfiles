;;; siraben-assembly.el --- Specific modes and packages for assembly code.

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

(add-hook 'asm-mode-hook
          #'(lambda ()
              (undo-tree-mode +1)
              (setq asm-indent-level 8)
              (orgtbl-mode    +1)))

(provide 'siraben-assembly)
;;; siraben-assembly.el ends here
