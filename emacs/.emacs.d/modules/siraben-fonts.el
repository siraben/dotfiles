;;; siraben-fonts.el --- Setup fonts

;;; Commentary:

;; This file sets up the use of the Hack font and various functions
;; that allow fonts to be resized.  The font settings were inspired by
;; hrs's dotfiles repository at
;; `https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org'

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

;;; Code:

(defvar siraben-default-font "Hack" "The default font type.")
(defvar siraben-default-font-size
  (cond
   ((eq system-type 'darwin) 13)
   ((eq system-type 'gnu/linux) 10))
  "The default font size.")

(defvar siraben-font-change-increment 1.1
  "The multipler to the font size when `siraben-increase-font-size' is invoked.")

(defun siraben-font-code ()
  "Return a string representing the current font (like \"Hack-13\")."
  (concat siraben-default-font "-"
          (number-to-string siraben-current-font-size)))

(defun siraben-set-font-size ()
  "Set the font to `siraben-default-font' at `siraben-current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (siraben-font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun siraben-reset-font-size ()
  "Change font size back to `siraben-default-font-size'."
  (interactive)
  (setq siraben-current-font-size siraben-default-font-size)
  (siraben-set-font-size))

(defun siraben-increase-font-size ()
  "Increase current font size by a factor of `siraben-font-change-increment'."
  (interactive)
  (setq siraben-current-font-size
        (ceiling (* siraben-current-font-size
                    siraben-font-change-increment)))

  (siraben-set-font-size))

(defun siraben-decrease-font-size ()
  "Decrease current font size by a factor of `siraben-font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq siraben-current-font-size
        (max 1
             (floor (/ siraben-current-font-size
                       siraben-font-change-increment))))
  (siraben-set-font-size))

(add-hook 'after-init-hook 'siraben-reset-font-size)

(provide 'siraben-fonts)
;;; siraben-fonts.el ends here
