;; siraben-org.el

;; This file configure Org mode, especially its agenda and notes
;; features.

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-log-done t)

;; Org mode code block languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (sh . t)
   (calc . t)
   (python . t)
   (scheme . t)))

;; From `https://emacs.stackexchange.com/a/13828'
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

(provide 'siraben-org)
