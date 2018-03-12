;; siraben-mdm.el

;; This mode was written from scratch by me after being inspired by
;; The Most Dangerous Writing App which can be found at
;; `https://www.themostdangerouswritingapp.com/'

;; There's no guarantee that this code is up to style. It's very hacky
;; as it is, but it works good enough for my purposes.

(defvar siraben-mdt-grace 5)
(defvar siraben-mdt-restore nil)
(defvar siraben-mdm-timer nil)

(defun siraben-mdm-timer-reset ()
  (setq siraben-mdt-grace 5))

(defvar siraben-mdm-end-time 0)

(defun most-dangerous-mode (&optional duration) 
  (interactive "p")
  (siraben-mdm-timer-reset)
  (setq siraben-mdm-end-time (+ (or current-prefix-arg duration 300) (cadr (current-time)))
        siraben-mdm-timer
        (run-with-timer 1 1 #'(lambda ()
                                (unless (> siraben-mdm-grace 0)
                                  (backward-kill-word 1))
                                (setq mode-line-format
                                      (format "Siraben-Mdm-Grace: %d Time left: %d"
                                              (setq siraben-mdm-grace
                                                    (- siraben-mdm-grace 1))
                                              (- siraben-mdm-end-time
                                                 (cadr (current-time)))))
                                (force-mode-line-update)))
        siraben-mdm-restore mode-line-format)
  (add-hook 'post-self-insert-hook #'siraben-mdm-timer-reset)
  (run-at-time (- siraben-mdm-end-time (cadr (current-time)))
               nil
               #'(lambda ()
                   (cancel-timer siraben-mdm-timer)
                   (remove-hook 'post-self-insert-hook #'siraben-mdm-timer-reset)
                   (setq mode-line-format siraben-mdm-restore))))

(provide 'siraben-mdm)
