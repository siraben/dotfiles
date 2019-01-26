;;; siraben-editor.el --- make Emacs a great editor.

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

(global-auto-revert-mode t)
;; Dynamic binding sucks.
(setq lexical-binding t)

;; Don't use tabs to indent but maintain correct appearance.
(setq-default indent-tabs-mode nil
              tab-width        8)
(setq tab-always-indent 'complete)

;; Require newline at end of file.
(setq require-final-newline t)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'undo-tree)
;; Auto-save the undo-tree history.
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; Blinking parens are ugly.
(setq blink-matching-paren nil)

(require 'windmove)
(windmove-default-keybindings)

(defun siraben-enable-writing-modes ()
  "Enables writing modes for writing prose.
Enables auto-fill mode, spell checking and disables company mode."
  (interactive)
  (progn (auto-fill-mode 1)
	 (undo-tree-mode 1)
	 (flyspell-mode 1)
         (visual-line-mode 1)    ; Org mode headings don't wrap.
         (company-mode -1)))

(defvar-local siraben-timed-writing-timer
  nil
  "The current writing timer object. Stop this timer with
`cancel-timer'.")

(defun siraben-timed-writing-mode (&optional length)
  "Begin a timed writing session for LENGTH minutes.
If LENGTH is not given, it defaults to 5.

After LENGTH minutes has passed, the user is prompted to make the
buffer from which the function was invoked read-only."
  (interactive "p")
  (let ((working-buffer (current-buffer))
        (default-length 5))
    (setq-local siraben-timed-writing-timer
                (run-at-time (concat (int-to-string (or current-prefix-arg
                                                        length
                                                        default-length))
                                     "min")
                             nil
                             `(lambda ()
                                (switch-to-buffer ,working-buffer)
                                (read-only-mode t))))))

(defalias 'timed-writing-mode 'siraben-timed-writing-mode)

(add-hook 'markdown-mode-hook #'siraben-enable-writing-modes)
(add-hook 'org-mode-hook #'siraben-enable-writing-modes)

(require 'siraben-mdm)

(add-hook 'sgml-mode-hook
          #'(lambda ()
              (require 'rename-sgml-tag)
              (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

(use-package emojify)
(use-package company-emoji)

;; De-duplicate kill ring entries.
(setq kill-do-not-save-duplicates t)

;; Save the system clipboard when we put something in the kill ring in
;; Emacs.
(setq save-interprogram-paste-before-kill t)

;;; Dired stuff
;; In case we want to restore the file we deleted..
(setq delete-by-moving-to-trash t)
(dired-async-mode 1)

(defmacro my-defun (name params doc &rest rest)
  "Define NAME as a function with PARAMS with siraben/ prepended to it.
Expands to (defun siraben/NAME DOC REST), and creates an
alias (see `defalias') to NAME, so you can use the function as if
it was defined with `defun' but when you look up the function's
documentation you'll know who to blame if you break something."
  (let ((internal-name
         (intern (format "siraben/%s" (symbol-name name)))))
    `(progn (defun ,internal-name ,params ,doc ,@rest)
            (defalias ',name ',internal-name))))

(defun siraben-days-until (&optional date just-number)
  "Display the number of days until DATE, in the minibuffer.
For \"days\" it counts the number of nights that between today
and then, so tomorrow is 1 day from now, and so on.

The date must be in (month day year) format."
  (interactive)
  (let ((target (or date (calendar-read-date))))
    (if (not (and (listp target)
                  (= 3 (list-length target))
                  (every #'numberp target)))
        ;; If the date was invalid.
        (error "Date passed was not a valid date")
      (let ((days (- (calendar-absolute-from-gregorian target)
                     (calendar-absolute-from-gregorian
                      (calendar-current-date)))))
        (if just-number
            days
          (message (format "%s days until %s %s, %s"
                           days
                           (calendar-month-name (car target))
                           (cadr target)
                           (caddr target))))))))


(defun siraben-unfill-paragraph ()
  "Unfill the current region into a single line of text.
This is very useful when pasting from Emacs into, say, a web form
that doesn't want text with hard newlines"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

;; Inspired by `https://waitbutwhy.com/2016/10/100-blocks-day.html'

(cl-defun siraben-get-time (&optional time-int (prompt "Time (HHMM): "))
  "Get the current time.  TIME-INT is in 24 hour format, 4 digit number."
  (interactive)
  (let ((time-int (or time-int
                      (string-to-number (read-string prompt)))))
    (if (< time-int 0)
        (user-error "Invalid time: %s" time-int)
      (let ((minutes (mod time-int 100)) (hours (/ time-int 100)))
        (if (not (and (<= hours 23) (<= 59)))
            (user-error "Invalid time: %s %s" hours minutes)
          (cons hours minutes))))))


(defun siraben-convert-time-to-minutes (time-cons)
  "Convert time in TIME-CONS (HH . MM) given into the number of minutes since 00:00."
  (let ((hours (car time-cons))
        (minutes (cdr time-cons)))
    (+ 0.0 (+ (* 60 hours) minutes))))


;; TODO: REFACTOR: This section has very bad (i.e. repetitive) Elisp code!
(defun siraben-minutes-since-wake (&optional wake-time)
  "Compute the difference between the present and WAKE-TIME."
  (let* ((wake-time (convert-time-to-minutes (get-time wake-time "Wake time (HHMM): ")))
         (current-time (decode-time (current-time)))
         (present (convert-time-to-minutes (cons (caddr current-time) (cadr current-time)))))
    (- present wake-time)))


(defun siraben-day-percent (&optional wake-time sleep-time)
  "Compute how far you are through the day from WAKE-TIME to SLEEP-TIME."
  (let* ((wake-time (convert-time-to-minutes (get-time wake-time "Wake time (HHMM): ")))
         (sleep-time (convert-time-to-minutes (get-time sleep-time "Sleep time (HHMM): ")))
         (current-time (decode-time (current-time)))
         (present (convert-time-to-minutes (cons (caddr current-time) (cadr current-time))))
         (day-length (- sleep-time wake-time)))
    (/ (- present wake-time) day-length)))


(defun siraben-day-block-length (&optional wake-time sleep-time)
  "Compute the length of block in minutes with 100 blocks per day = (- SLEEP-TIME WAKE-TIME)."
  (let* ((wake-time (convert-time-to-minutes (get-time wake-time "Wake time (HHMM): ")))
         (sleep-time (convert-time-to-minutes (get-time sleep-time "Sleep time (HHMM): ")))
         (current-time (decode-time (current-time)))
         (day-length (- sleep-time wake-time)))
    (/ day-length 100.0)))


(defvar siraben-default-wake-time
  0700
  "Your default waking time.  Must be a number in HHMM form.")

(defvar siraben-default-sleep-time
  2230
  "Your default sleeping time.  Must be a number in HHMM form.")

(cl-defun siraben-current-block (&optional (wake-time 800)
                                           (sleep-time 2230))
  "Compute the number of the block that you are currently in, between WAKE-TIME and SLEEP-TIME."
  (let ((block-length (day-block-length wake-time sleep-time)))
    (message (format "Current block: %s. Each block is %s minutes long."
                     (round-off (/ (minutes-since-wake wake-time) block-length) 2)
                     block-length))))

(defun siraben-round-off (z n)
  "Round Z to N decimal places."
  (let ((power (+ 0.0 (expt 10 n)))) ; coerce power to be a float.
    (/ (round (* power z)) power)))

;; If they ever cause problems, inspecting them should show that they're aliased.
(defalias 'days-until              'siraben-days-until)
(defalias 'unfill-paragraph        'siraben-unfill-paragraph)
(defalias 'get-time                'siraben-get-time)
(defalias 'convert-time-to-minutes 'siraben-convert-time-to-minutes)
(defalias 'minutes-since-wake      'siraben-minutes-since-wake)
(defalias 'day-block-length        'siraben-day-block-length)
(defalias 'current-block           'siraben-current-block)
(defalias 'round-off               'siraben-round-off)

(provide 'siraben-editor)
;;; siraben-editor.el ends here
