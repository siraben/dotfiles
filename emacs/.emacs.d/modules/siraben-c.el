;;; siraben-c.el --- Configure Emacs for C and C++ development

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

;; Configures packages and various modes for C programming.

;;; Code:

;; Code auto completion packages for C code.
(use-package clang-format+)

(require 'cc-mode)
(defvar c-prettify-symbols-alist
  '(("->"     . ?→)
    ("!"      . ?¬)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("<<"     . ?≪)
    (">>"     . ?≫)
    ("..."    . ?…)
    ("*"      . ?∗)
    ("="      . ?≔)
    ;; ("uint32_t" . (?ℕ (Br . Bl) ?₃
    ;;                   (Br . Bl) ?₂))
    ;; ("uint8_t" . (?ℕ (Br . Bl) ?₈))
    ;; ("bool" .  ?𝔹)
    ;; ("Uint32" . ,(string-to-symbols "ℕ₃₂"))
    ;; ("Uint8" . ,(string-to-symbols "ℕ₈"))
    ;; ("union" . ?∪)
    ("x_1" . (?x (Br . Bl) ?₁))
    ("x_2" . (?x (Br . Bl) ?₂))
    ("y_1" . (?y (Br . Bl) ?₁))
    ("y_2" . (?y (Br . Bl) ?₂))
    ;; ("NULL"   . ?∅)
    ))

(add-hook 'c-mode-hook (lambda ()
                         (electric-pair-local-mode t)
                         (electric-indent-mode t)
                         (clang-format+-mode t)
                         (setq-local prettify-symbols-alist c-prettify-symbols-alist)
                         (prettify-symbols-mode 1)
                         ;; (lsp)
                         ))

(add-hook 'c++-mode-hook (lambda ()
                           (clang-format+-mode t)
                           (electric-pair-local-mode t)
                           ;; (lsp)
                           ))

(define-key c-mode-base-map (kbd "s-b") 'recompile)

(provide 'siraben-c)
;;; siraben-c.el ends here
