;; Font settings, inspired by
;; `https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org'
(setq ben/default-font "Hack")
(setq ben/default-font-size 12)
(setq ben/font-change-increment 1.1)

(defun ben/font-code ()
  "Return a string representing the current font (like
  \"Hack-13\")."
  (concat ben/default-font "-" (number-to-string ben/current-font-size)))

(defun ben/set-font-size ()
  "Set the font to `ben/default-font' at `ben/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (ben/font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun ben/reset-font-size ()
  "Change font size back to `ben/default-font-size'."
  (interactive)
  (setq ben/current-font-size ben/default-font-size)
  (ben/set-font-size))

(defun ben/increase-font-size ()
  "Increase current font size by a factor of
`ben/font-change-increment'."
  (interactive)
  (setq ben/current-font-size
        (ceiling (* ben/current-font-size ben/font-change-increment)))

  (ben/set-font-size))

(defun ben/decrease-font-size ()
  "Decrease current font size by a factor of
`ben/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq ben/current-font-size
        (max 1
             (floor (/ ben/current-font-size ben/font-change-increment))))
  (ben/set-font-size))


(ben/reset-font-size)


(provide 'siraben-fonts)
