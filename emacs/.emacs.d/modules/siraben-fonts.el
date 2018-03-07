;; siraben-fonts.el

;; This file sets up the use of the Hack font and various functions
;; that allow fonts to be resized. The font settings were inspired by
;; hrs's dotfiles repository at
;; `https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org'

(defvar siraben-default-font "Hack" "The default font to use in Emacs.")
(defvar siraben-default-font-size
  (cond
   ((eq system-type 'darwin) 13)
   ((eq system-type 'gnu/linux) 10.7))
  "The default font size in Emacs.")

(defvar siraben-font-change-increment 1.1
  "The multipler to the font size when
`siraben-increase-font-size' is invoked.")

(defun siraben-font-code ()
  "Return a string representing the current font (like
  \"Hack-13\")."
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
  "Increase current font size by a factor of
`siraben-font-change-increment'."
  (interactive)
  (setq siraben-current-font-size
        (ceiling (* siraben-current-font-size
                    siraben-font-change-increment)))

  (siraben-set-font-size))

(defun siraben-decrease-font-size ()
  "Decrease current font size by a factor of
`siraben-font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq siraben-current-font-size
        (max 1
             (floor (- siraben-current-font-size
                       siraben-font-change-increment))))
  (siraben-set-font-size))


(siraben-reset-font-size)


(provide 'siraben-fonts)
