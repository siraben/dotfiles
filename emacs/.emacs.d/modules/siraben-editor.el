;; siraben-editor.el

;; This file configures Emacs to be more usable as a text editor.

;; Don't use tabs to indent but maintain correct appearance.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Newline at end of file
(setq require-final-newline t)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Autosave the undo-tree history.
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; Revert buffers automatically when underlying files are changed
;; externally.
(global-auto-revert-mode t)
(setq tab-always-indent 'complete)

;; Don't blink!
(setq blink-matching-paren nil)

(defun siraben-enable-lisp-editing-modes ()
  "Enables a collection of modes (such as paredit, rainbow delimiters,
aggressive indentation etc.) that greatly help with editing lisp
code."
  (interactive)
  (progn (setq show-paren-style 'mixed)
	 (paredit-mode t)
	 (rainbow-delimiters-mode t)
	 (aggressive-indent-mode t)
	 (show-paren-mode t)
	 (global-undo-tree-mode t)
	 (company-mode t)))

(setq siraben-lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook siraben-lispy-mode-hooks)
  (add-hook hook #'siraben-enable-lisp-editing-modes))

;; Enable some Lisp modes like paredit and rainbow delimiters, but no
;; need to undo and autocomplete.
(add-hook 'inferior-scheme-mode-hook 
	  #'(lambda ()
	      (siraben-enable-lisp-editing-modes)
	      (undo-tree-mode -1)))

(defun siraben-enable-writing-modes ()
  "Enables auto-fill mode, spell checking and disables company
mode. Although it looks like hard wrapping will warp the text on
org-export, it actually doesn't!"
  (interactive)
  (progn (auto-fill-mode 1)
	 (undo-tree-mode 1)
	 (flyspell-mode 1)
	 ;; This is for situations like vertically split windows when
	 ;; Org mode headings don't wrap.
	 (visual-line-mode 1)
	 (company-mode -1)))

(defvar-local siraben-timed-writing-timer
  nil
  "The current writing timer object. Stop this timer with
  `cancel-timer'.")

(defun siraben-timed-writing-mode (&optional length)
  "Begin a timed writing session for X minutes, where X is the
numerical prefix passed to the function, or the numerical
argument from an Elisp function call, or the default value of 5.

After X minutes, the user is prompted to make the buffer from
which the function was invoked read only."
  (interactive "p")
  (let ((working-buffer (current-buffer))
        (default-length 5))
    (setq-local siraben-timed-writing-timer
                (run-at-time (concat (int-to-string (or current-prefix-arg
                                                        length
                                                        default-length)) "min")
                             nil
                             `(lambda ()
                                (switch-to-buffer ,working-buffer)
                                (read-only-mode))))))

(add-hook 'markdown-mode-hook #'siraben-enable-writing-modes)
(add-hook 'org-mode-hook #'siraben-enable-writing-modes)
(setq auto-save-interval 100)

;; Implement the most dangerous mode

(defvar grace 5)
(defvar restore-mode-line nil)
(defvar most-dangerous-timer nil)

(defun most-dangerous-timer-reset ()
  (setq grace 5))

(defvar end-time 0)

(defun most-dangerous-mode (&optional duration) 
  (interactive "p")
  (most-dangerous-timer-reset)
  (setq end-time (+ (or current-prefix-arg duration 300) (cadr (current-time)))
        most-dangerous-timer
        (run-with-timer 1 1 #'(lambda ()
                                (unless (> grace 0)
                                  (backward-kill-word 1))
                                (setq mode-line-format (format "Grace: %d Time left: %d"
                                                               (setq grace (- grace 1))
                                                               (- end-time (cadr (current-time)))))
                                (force-mode-line-update)))
        restore-mode-line mode-line-format)
  (add-hook 'post-self-insert-hook #'most-dangerous-timer-reset)
  (run-at-time (- end-time (cadr (current-time)))
               nil
               #'(lambda ()
                   (cancel-timer most-dangerous-timer)
                   (remove-hook 'post-self-insert-hook #'most-dangerous-timer-reset)
                   (setq mode-line-format restore-mode-line))))

(provide 'siraben-editor)
