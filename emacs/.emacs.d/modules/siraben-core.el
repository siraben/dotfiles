;;; siraben-core.el --- This file contains the core functions I wrote.

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

(defun siraben-insert-time ()
  "Insert the date and time into the current buffer."
  (interactive)
  (shell-command "date '+%A, %B %d, %Y at %R'" 1))

(defun siraben-new-diary-entry ()
  "Create a new buffer with a new diary entry with Org mode."
  (interactive)
  (pop-to-buffer (generate-new-buffer-name "diary-"))
  (org-mode)
  (insert "* ")
  (siraben-insert-time)
  (goto-char (point-max)))

(defun siraben-reset-packages ()
  "Deletes all packages from the directory `siraben-root-dir'."
  (interactive)
  (when (y-or-n-p "Really reset packages?")
    (message "Removing installed package directory...")
    (delete-directory (concat siraben-root-dir "elpa/") t t)
    (when (y-or-n-p "Packages deleted. Quit Emacs?")
      (save-buffers-kill-emacs))))

(defmacro set-if-exists (sym str)
  "Set SYM TO STR if STR exists as a file."
  `(if (file-exists-p ,str)
       (setq ,sym ,str)))

(defun enable-all-commands ()
  "Enable all commands, reporting on which were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that were disabled*"
    (mapatoms
     (function
      (lambda (symbol)
        (when (get symbol 'disabled)
          (put symbol 'disabled nil)
          (prin1 symbol)
          (princ "\n")))))))

(defun enable-me (&rest args)
  "Called when a disabled command is executed.
    Enable it and reexecute it."
  (put this-command 'disabled nil)
  (message "You typed %s.  %s was disabled.  It ain't no more."
           (key-description (this-command-keys)) this-command)
  (sit-for 0)
  (call-interactively this-command))

(setq disabled-command-hook 'enable-me)

(provide 'siraben-core)
;;; siraben-core.el ends here
