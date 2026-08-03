;;; siraben-core.el --- Core helper functions and command defaults

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

;; Shared interactive helpers used by the rest of the Emacs configuration.

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
