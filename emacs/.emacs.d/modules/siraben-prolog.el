;;; siraben-prolog.el --- Prolog programming customizations.

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

;; This file fixes the broken built-in Prolog editing modes that Emacs
;; provides.

;;; Code:

(use-package ediprolog
  :commands (ediprolog-dwim)
  :bind ("<f10>" . ediprolog-dwim)
  :config
  (setq ediprolog-system 'swi))

(add-hook 'after-init-hook
          (lambda ()
            (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
            (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))))

(provide 'siraben-prolog)
;;; siraben-prolog.el ends here
