(defun formula-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name  		"Formula")
  (setq major-mode 		'formula-mode)
  ;; Turn on font-lock mode
  ;; (and formula-font-lock-mode (font-lock-mode))
  ;; (font-lock-mode)

  ;; Finally, run the hooks and be done.
  (run-hooks 'formula-mode-hook)
  )
