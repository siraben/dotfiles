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

(use-package rust-mode)
(use-package racer)
(use-package flycheck-rust)
(use-package cargo)

(setq rust-format-on-save t)

(eval-after-load 'rust-mode
  '(progn
     (add-hook 'rust-mode-hook 'racer-mode)
     (add-hook 'racer-mode-hook 'eldoc-mode)
     (add-hook 'rust-mode-hook 'cargo-minor-mode)
     (add-hook 'rust-mode-hook 'flycheck-rust-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

     (defun siraben-rust-mode-defaults ()
       (local-set-key (kbd "C-c C-d") 'racer-describe)
       (local-set-key (kbd "s-b") 'recompile)
       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq siraben-rust-mode-hook 'siraben-rust-mode-defaults)

     (add-hook 'rust-mode-hook (lambda ()
                                 (run-hooks 'siraben-rust-mode-hook)))))

(provide 'siraben-rust)
;;; siraben-rust.el ends here
