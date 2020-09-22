;;; siraben-coq.el --- Coq programming customizations.

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

;; This file has Coq and Proof General specific customizations.

;;; Code:

(use-package proof-general)
(use-package company-coq)

(add-hook 'coq-mode-hook
          (lambda ()
            (siraben-prog-mode-defaults)
            (company-coq-mode t)))


(provide 'siraben-coq)
;;; siraben-coq.el ends here
