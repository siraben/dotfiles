;;; siraben-js.el --- Javascript mode customizations

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

;; This file handles working with JavaScript.

;;; Code:

(use-package js2-mode
  :config 
  (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(add-hook 'js-mode-hook
          #'(lambda ()
              (smartparens-strict-mode 1)
              (js2-mode 1)
              (setq-local electric-layout-rules '((?\; . after)))
              (js2-imenu-extras-mode 1)))

(provide 'siraben-js)
;;; siraben-js.el ends here
