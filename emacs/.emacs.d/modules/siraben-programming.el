;;; siraben-programming.el --- Load programming language configuration

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

;; Set shared programming-mode defaults and load language-specific modules.

;;; Code:

(global-set-key (kbd "M-C") 'comment-or-uncomment-region)

(defun siraben-prog-mode-defaults ()
  "Default programming mode hook, useful with any programming language."
  (electric-pair-mode +1)
  
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'siraben-prog-mode-defaults)

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(require 'ansi-color)

;; Process only the newly inserted compilation output.  Re-coloring the whole
;; buffer for every process chunk becomes quadratic and stalls long builds.
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-hook 'after-init-hook (lambda () (require 'siraben-c)))
(require 'siraben-coq)
(require 'siraben-haskell)
(require 'siraben-lisp)
(require 'siraben-nix)
(require 'siraben-python)
(require 'siraben-rust)
(require 'siraben-typescript)

(provide 'siraben-programming)
;;; siraben-programming.el ends here
