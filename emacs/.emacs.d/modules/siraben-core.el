;; siraben-core.el

;; This file contains the core functions I wrote.

(defun siraben-insert-time ()
  "Inserts the date and time into the current buffer."
  (interactive)
  (shell-command "date '+%A, %B %d, %Y at %R'" 1))

(defun siraben-new-diary-entry ()
  "Creates a new buffer with a new diary entry with org mode
activated and a time stamp added."
  (interactive)
  (pop-to-buffer (generate-new-buffer-name "diary-"))
  (org-mode)
  (insert "* ")
  (ben/insert-time)
  (end-of-buffer))

(defun siraben-update ()
  "Update siraben's Emacs config to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update siraben's Emacs config?")
    (message "Updating installed packages...")
    (auto-package-update-now)
    (message "Updating siraben's Emacs config...")
    (cd siraben-root-dir)
    (shell-command "git pull") 
    (message "Update finished. Restart Emacs to complete the process.")))

(defun siraben-reset-packages ()
  "Deletes all packages from the directory `siraben-root-dir'"
  (interactive)
  (when (y-or-n-p "Really reset packages?")
    (message "Removing installed package directory...")
    (delete-directory (concat siraben-root-dir "elpa/") t t) 
    (when (y-or-n-p "Packages deleted. Quit Emacs?")
      (save-buffers-kill-emacs))))

(provide 'siraben-core)
