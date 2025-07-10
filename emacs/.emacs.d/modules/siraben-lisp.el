;;; siraben-lisp.el --- Specific modes and packages for Lisp code.

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

(require 'paren)

(defun siraben-enable-lisp-editing-modes ()
  "Enables a collection of modes for editing Lisp code."
  (interactive)
  (progn (setq show-paren-style 'mixed)
         (electric-pair-mode      -1)
	 (paredit-mode            t)
	 (rainbow-delimiters-mode t)
	 (aggressive-indent-mode  t)
	 (show-paren-mode         t)
	 (undo-tree-mode          t)
	 (company-mode            t)))

(defvar siraben-lispy-mode-hooks
  '(clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    scheme-mode-hook
    racket-mode-hook))

(dolist (hook siraben-lispy-mode-hooks)
  (add-hook hook #'siraben-enable-lisp-editing-modes))

(use-package scheme
  :config
  (setq scheme-program-name "guile")
  :hook (scheme-mode . (lambda () (flycheck-mode -1))))

(use-package geiser
  :disabled
  :defer 10
  :hook (geiser-repl-mode . siraben--setup-geiser-repl)
  :config
  (setq geiser-default-implementation 'guile)
  
  (defun siraben--setup-geiser-repl ()
    "Configure Geiser REPL mode."
    (siraben-enable-lisp-editing-modes)
    (undo-tree-mode -1)
    (paredit-mode 1)
    (aggressive-indent-mode -1)))

(add-hook 'ielm-mode-hook #'siraben--setup-ielm)

(defun siraben--setup-ielm ()
  "Configure IELM mode."
  (siraben-enable-lisp-editing-modes)
  (undo-tree-mode -1)
  (paredit-mode 1)
  (aggressive-indent-mode -1))

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode 1)))

(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  :hook (racket-mode . racket-xp-mode))

(provide 'siraben-lisp)
;;; siraben-lisp.el ends here
