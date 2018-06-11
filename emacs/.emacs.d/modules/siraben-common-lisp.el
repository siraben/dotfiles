;;; siraben-common-lisp.el --- configuration for programming in Common Lisp

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

;; Common lisp stuff.

;;; Code:

(use-package slime)

(require 'slime)

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

;; select the default value from slime-lisp-implementations
(if (and (eq system-type 'darwin)
         (executable-find "ccl"))
    ;; default to Clozure CL on macOS
    (setq slime-default-lisp 'ccl)
  ;; default to SBCL on Linux and Windows
  (setq slime-default-lisp 'sbcl))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t
           slime-auto-start 'always)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

;; rainbow-delimeters messes up colors in slime-repl, and doesn't seem to work
;; anyway, so we won't use prelude-lisp-coding-defaults.

(add-hook 'comint-mode-hook
          (lambda ()
            (siraben-enable-lisp-editing-modes)
            (undo-tree-mode -1)
            (aggressive-indent-mode -1)))

(provide 'siraben-common-lisp)
;;; siraben-common-lisp.el ends here
