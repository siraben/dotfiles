;;; siraben-agda.el --- configures Emacs for Agda development

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
(add-to-list 'auto-mode-alist
             '(("\\.agda\\'" . agda2-mode)
               ("\\.lagda.md\\'" . agda2-mode)))

(when (locate-library "agda2-mode")
  (load-library "agda2-mode")
  (let ((base03    "#002b36") (base02    "#073642")
        (base01    "#586e75") (base00    "#657b83")
        (base0     "#839496") (base1     "#93a1a1")
        (base2     "#eee8d5") (base3     "#fdf6e3")
        (yellow    "#b58900") (orange    "#cb4b16")
        (red       "#dc322f") (magenta   "#d33682")
        (violet    "#6c71c4") (blue      "#268bd2")
        (cyan      "#2aa198") (green     "#859900"))
    (custom-set-faces
     `(agda2-highlight-keyword-face ((t (:foreground ,orange))))
     `(agda2-highlight-string-face ((t (:foreground ,magenta))))
     `(agda2-highlight-number-face ((t (:foreground ,violet))))
     `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
     `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
     `(agda2-highlight-bound-variable-face ((t nil)))
     `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
     `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
     `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
     `(agda2-highlight-field-face ((t (:foreground ,red))))
     `(agda2-highlight-function-face ((t (:foreground ,blue))))
     `(agda2-highlight-module-face ((t (:foreground ,violet))))
     `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
     `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
     `(agda2-highlight-record-face ((t (:foreground ,blue))))
     `(agda2-highlight-dotted-face ((t nil)))
     `(agda2-highlight-operator-face ((t nil)))
     `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
     `(agda2-highlight-unsolved-meta-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-unsolved-constraint-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03)))))))

;;; siraben-agda.el ends here
