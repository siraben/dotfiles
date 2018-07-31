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
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

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

(setq org-habit-show-habits-only-for-today t)

(setq org-agenda-diary-file nil)
(setq org-habit-graph-column 50)
(setq org-habit-preceding-days 20)
(setq org-habit-following-days 4)

(setq org-log-done t)

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

;; These are code blocks that are "safe" to evaluate.
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "dot")
           (string= lang "gnuplot")
           (string= lang "octave"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; From `https://emacs.stackexchange.com/a/13828'.
;; The fifth entry specifies how many newlines are allowed inside a
;; marked up expression. By default, org-mode allows a single
;; newline. So if you want to be able to add markup to text that spans
;; more than two consecutive lines, you'll need to modify this entry.

(setcar (nthcdr 4 org-emphasis-regexp-components) 2)

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; ... where N is the number of newlines you want to allow.

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package gnuplot)
(use-package htmlize)
(use-package edit-indirect)
(use-package org-wc)

;; I want to have source code syntax highlighting for LaTeX export as well.
(setq org-latex-listings 'minted)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))


(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(provide 'siraben-org)
;;; siraben-org.el ends here
