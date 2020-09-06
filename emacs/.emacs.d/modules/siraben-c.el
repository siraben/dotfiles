;;; siraben-c.el --- configures Emacs for C development

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
(require 'cc-mode)

;; Code auto completion packages for C code.
(use-package google-c-style)
(use-package ccls)
(use-package clang-format)

(add-hook 'c-mode-hook #'(lambda ()
                           (google-set-c-style)
                           (lsp)
                           (flycheck-mode t)
                           (electric-pair-local-mode t)
                           (electric-indent-mode t)
                           ))
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (progn
                (when (locate-dominating-file "." ".clang-format")
                  (clang-format-buffer))
                ;; Continue to save.
                nil))
            nil
            ;; Buffer local hook.
            t))

(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;; I like having an easy key binding to recompile easily.
(define-key c-mode-base-map (kbd "s-b") 'recompile)

(provide 'siraben-c)
;;; siraben-c.el ends here
