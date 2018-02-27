;; siraben-org.el

;; This file configure Org mode, especially its agenda and notes
;; features.

(setq org-directory "~/Nextcloud")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files "~/Nextcloud/homework.org")

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

(provide 'siraben-org)
