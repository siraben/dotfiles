;;; siraben-org.el --- Org Mode customizations

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

;; This file configure Org mode, especially its agenda and notes
;; features.

;;; Code:

(use-package org
  :bind
  (("C-c c" . org-capture) ("C-c l" . org-store-link) ("C-c a" . org-agenda))
  :config
  (setq org-hide-emphasis-markers t
        org-fontify-emphasized-text t
        ;; org-image-actual-width nil
        org-src-fontify-natively t
        org-startup-with-inline-images t
        org-startup-truncated t
        org-adapt-indentation nil
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil
        org-agenda-diary-file nil
        org-habit-graph-column 50
        org-habit-preceding-days 20
        org-habit-following-days 4
        org-habit-show-habits-only-for-today t
        org-log-done 'time)

  ;; Org mode code block languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (gnuplot . t)
     (,(if (version< emacs-version "26") 'sh 'shell) . t)
     (calc . t)
     (python . t)
     (scheme . t)
     (dot . t)
     (octave . t)))
  ;; I want to have source code syntax highlighting for LaTeX export as well.
  (setq org-latex-listings 'minted)
  (setq org-latex-packages-alist '(("" "minted")))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; These are code blocks that are "safe" to evaluate.
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "dot")
             (string= lang "gnuplot")
             (string= lang "octave"))))

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-modules '(org-mu4e
                      org-habit
                      org-crypt
                      org-bbdb
                      org-bibtex
                      org-docview
                      org-gnus
                      org-info
                      org-irc
                      org-mhe
                      org-rmail))

  (setq org-export-backends '(ascii beamer html icalendar latex odt))
  (setq org-list-allow-alphabetical t)
  (defvar org-electric-pairs '((?$ . ?$)))
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (add-hook 'org-mode-hook 'org-add-electric-pairs)

  (require 'ox-latex)
  (add-to-list 'org-latex-classes '("journal" "\\documentclass[11pt]{journal}"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(use-package gnuplot)
(use-package htmlize)
(use-package edit-indirect)
(use-package org-wc)

(use-package laas
  :hook ((LaTeX-mode org-mode) . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
    :cond #'laas-org-mathp
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    )
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "lr" (lambda () (interactive)
           (yas-expand-snippet "\\left($0\\right)"))
    )
  (apply #'aas-set-snippets 'laas-mode laas-basic-snippets)
  )

(provide 'siraben-org)
;;; siraben-org.el ends here
