(defun siraben-fullscreen ()
  "Make Emacs window fullscreen.
This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun ben/insert-time ()
  "Inserts the date and time into the current buffer."
  (interactive)
  (shell-command "date '+%A, %B %d, %Y at %R'" 1))

(defun ben/new-diary-entry ()
  "Creates a new buffer with a new diary entry with org mode
activated and a time stamp added."
  (interactive)
  (pop-to-buffer (generate-new-buffer-name "diary-"))
  (org-mode)
  (insert "* ")
  (ben/insert-time)
  (end-of-buffer))

(defun siraben-update ()
  "Update Prelude to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update siraben's Emacs config?")
    (message "Updating installed packages...")
    (auto-package-update-now)
    (message "Updating siraben's Emacs config...")
    (cd (file-name-directory load-file-name))
    (shell-command "git pull") 
    (message "Update finished. Restart Emacs to complete the process.")))
