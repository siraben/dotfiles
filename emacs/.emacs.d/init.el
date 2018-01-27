(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(cond ((eq system-type 'darwin)
       (progn (setq scheme-program-name "/usr/local/bin/chez")
	      (setq ispell-program-name "/usr/local/bin/aspell")
	      (setq ben/default-font-size 14)
	      (setq system-uses-terminfo nil)
	      (setq cask-path "/usr/local/share/emacs/site-lisp/cask/cask.el")))
      
      ((eq system-type 'gnu/linux)
       (progn (setq scheme-program-name "/usr/bin/mit-scheme")
              (setq ispell-program-name "/usr/bin/aspell")
              (setq ben/default-font-size 11)
	      (setq cask-path "~/.cask/cask.el"))))

(unless (file-exists-p cask-path)
  (user-error "Cask is not installed! Get it at https://github.com/cask/cask"))

(require 'cask cask-path)

(cask-initialize)

(unless (package-installed-p 'use-package)
  (progn (package-refresh-contents)
	 (package-install 'use-package)))

(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq load-prefer-newer t)

(use-package pallet
  :config (pallet-mode t))

(setq initial-scratch-message (format ";; Scratch buffer created on %s\n"
				      (shell-command-to-string "date '+%A, %B %d %Y at %R'")))

(use-package auto-compile  
  :config (auto-compile-on-load-mode))

(defun ben/apply-solarized-theme ()
  (interactive)
  (setq solarized-use-variable-pitch nil)
  ;; (setq solarized-height-plus-1 1.0)
  ;; (setq solarized-height-plus-2 1.0)
  ;; (setq solarized-height-plus-3 1.0)
  ;; (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-dark t))


(use-package solarized-theme 
  :config (ben/apply-solarized-theme))

(use-package exec-path-from-shell
  :defer t
  :config (exec-path-from-shell-initialize))


(setq ben/default-font "Hack")

(setq ben/current-font-size ben/default-font-size)

(setq ben/font-change-increment 1.1)

(defun ben/font-code ()
  "Return a string representing the current font (like \"Hack-13\")."
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
  "Increase current font size by a factor of `ben/font-change-increment'."
  (interactive)
  (setq ben/current-font-size
        (ceiling (* ben/current-font-size ben/font-change-increment)))
  (ben/set-font-size))

(defun ben/decrease-font-size ()
  "Decrease current font size by a factor of `ben/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq ben/current-font-size
        (max 1
             (floor (/ ben/current-font-size ben/font-change-increment))))
  (ben/set-font-size))

(define-key global-map (kbd "C-)") 'ben/reset-font-size)
(define-key global-map (kbd "C-+") 'ben/increase-font-size)
(define-key global-map (kbd "C-=") 'ben/reset-font-size)
(define-key global-map (kbd "C--") 'ben/decrease-font-size)

(ben/reset-font-size)

(when window-system
  (scroll-bar-mode -1))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (calc . t)
   (python . t)
   (scheme . t)))



(use-package paredit)

(use-package rainbow-delimiters)

(use-package undo-tree)

(use-package aggressive-indent)


(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (progn (setq show-paren-style 'mixed)
			  (paredit-mode)
			  (rainbow-delimiters-mode)
			  (aggressive-indent-mode)
			  (show-paren-mode)
			  (company-mode)
			  (undo-tree-mode)
			  (linum-mode)))))

(add-hook 'inferior-scheme-mode-hook
	  (lambda ()
	    (progn (paredit-mode)
		   (aggressive-indent-mode)
		   (rainbow-delimiters-mode)
		   (show-paren-mode))))

(setq show-paren-style 'expression)

(use-package pdf-tools
  :defer t)

(use-package helm
  :defer t
  :bind (("M-x" . #'helm-M-x)
         ("C-x r b" . #'helm-filtered-bookmarks)
         ("C-x b" . #'helm-buffers-list)
	 ("C-x C-f" . #'helm-find-files)
	 ("C-h a" . #'helm-apropos))
  :config (helm-mode 1))


(use-package company
  :defer t 
  :config (global-company-mode))

(use-package writeroom-mode)

(use-package markdown-mode
  :defer t)

(use-package org-bullets)


(use-package smooth-scrolling
  :config (smooth-scrolling-mode))

(use-package magit
  :defer t)

(defun ben/insert-time ()
  (interactive)
  "Inserts the date and time into the current buffer."
  (shell-command "date '+%A, %B %d %Y at %R'" 1))

(defun ben/new-diary-entry ()
  "Creates a new buffer with a new diary entry with org mode activated
and a time stamp added."
  (interactive)
  (pop-to-buffer (generate-new-buffer-name "diary-"))
  (org-mode)
  (insert "* ")
  (ben/insert-time)
  (end-of-buffer))

(define-key global-map (kbd "M-T") 'ben/insert-time)


(defun ben/essential-settings ()
  "Modifies a bunch of settings to make Emacs nicer."
  (progn (setq-default cursor-type 'box)
	 (display-battery-mode t)
	 (display-time-mode t)
	 (setq inhibit-startup-screen t)
	 (setq line-number-mode t)
	 (setq auto-save-interval 100)
	 (setq gc-cons-threshold 20000000)
	 (menu-bar-mode -1)
	 (tool-bar-mode -1)))

(add-hook 'after-init-hook
	  #'ben/essential-settings)

(use-package emms
  :defer t
  :config (progn (require 'emms-setup)
		 (require 'emms-player-mplayer)
		 (emms-standard)
		 (emms-default-players)
		 (define-emms-simple-player mplayer '(file url)
		   (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
				 ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
				 ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
		   "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

		 ;;** EMMS
		 ;; Autoload the id3-browser and bind it to F7.
		 ;; You can change this to your favorite EMMS interface.
		 (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
		 (global-set-key [(f7)] 'emms-smart-browse)

		 (with-eval-after-load 'emms
		   (emms-standard) ;; or (emms-devel) if you want all features
		   (setq emms-source-file-default-directory "~/Music"
			 emms-info-asynchronously t
			 emms-show-format "♪ %s")

		   ;; Might want to check `emms-info-functions',
		   ;; `emms-info-libtag-program-name',
		   ;; `emms-source-file-directory-tree-function'
		   ;; as well.

		   ;; Determine which player to use.
		   ;; If you don't have strong preferences or don't have
		   ;; exotic files from the past (wma) `emms-default-players`
		   ;; is probably all you need.
		   (if (executable-find "mplayer")
		       (setq emms-player-list '(emms-player-mplayer))
		     (emms-default-players)))))

