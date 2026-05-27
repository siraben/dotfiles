;;; siraben-cool.el --- Major mode skeleton for Cool source files

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

;; Define a minimal major mode for the Cool language.

;;; Code:

(defun cool-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Cool")
  (setq major-mode 'cool-mode)
  (electric-pair-mode t)
  (run-hooks 'cool-mode-hook))

(provide 'siraben-cool)
;;; siraben-cool.el ends here
