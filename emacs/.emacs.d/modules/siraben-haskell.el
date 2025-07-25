;;; siraben-haskell.el -- Haskell programming customizations.

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

;; Configures packages and various modes for Haskell programming.

;;; Code:

(use-package flycheck-haskell)

(use-package haskell-mode
  :mode "\\.hs"
  :config
  (setq haskell-process-suggest-add-package nil)
  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ;; ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  (defun siraben--haskell-mode-setup ()
    (subword-mode             t)
    (interactive-haskell-mode t)
    (diminish 'interactive-haskell-mode)
    (flycheck-haskell-setup)
    (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
    (prettify-symbols-mode 1)
    (haskell-indentation-mode t))

  (add-hook 'haskell-mode-hook #'siraben--haskell-mode-setup)
  (add-hook 'inferior-haskell-mode-hook (lambda () (paredit-mode -1)))
  )

(use-package lsp-haskell)


(provide 'siraben-haskell)
;;; siraben-haskell.el ends here
