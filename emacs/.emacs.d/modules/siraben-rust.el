;;; siraben-rust.el --- Rust programming customizations. -*- lexical-binding: t; -*-

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

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :init
  ;; Only the capability hook owns LSP startup and Flymake diagnostics.
  (setq rustic-lsp-client 'eglot
        rustic-lsp-setup-p nil
        rustic-format-trigger nil)
  :config
  (require 'siraben-capabilities)
  ;; Only auto-format on save when rustfmt is actually installed; otherwise
  ;; every save would error out on Windows / minimal systems.
  (add-hook 'rustic-mode-hook
            (lambda ()
              (setq-local rustic-format-trigger
                          (when (siraben-have-p "rustfmt") 'on-save))))
  (defun siraben--rust-mode-setup ()
    "Configure Rust mode settings."
    (local-set-key (kbd "s-b") 'recompile)
    (subword-mode 1))
  (add-hook 'rustic-mode-hook #'siraben--rust-mode-setup))

(provide 'siraben-rust)
;;; siraben-rust.el ends here
