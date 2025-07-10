;;; siraben-rust.el --- Rust programming customizations.

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

;; Configures packages and various modes for Rust programming.

;;; Code:

(use-package flycheck-rust)
(use-package rustic)

(use-package rust-mode
  :config
  (require 'rust-mode)
  (setq rust-format-on-save t)
  (defun siraben--rust-mode-setup ()
    "Configure Rust mode settings."
    (rustic-mode)
    (flycheck-rust-setup)
    (local-set-key (kbd "s-b") 'recompile)
    (subword-mode 1)
    (lsp))
  (add-hook 'rust-mode-hook #'siraben--rust-mode-setup))

(provide 'siraben-rust)
;;; siraben-rust.el ends here
