;; Welcome to Ben's Init file!

;; Setting `gc-cons-threshold' high makes startup faster.
(setq gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; OS-specific settings
(cond ((eq system-type 'darwin)
       (progn (setq scheme-program-name "/usr/local/bin/chez")
	      (setq ispell-program-name "/usr/local/bin/aspell")
	      (setq ben/default-font-size 13)
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

;; Ensure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (progn (package-refresh-contents)
	 (package-install 'use-package)))

(require 'use-package)

;; Ensure that all packages are downloaded to their latest version,
;; but also defer them to speed up init.
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-verbose t)
(setq load-prefer-newer t)

(use-package pallet
  :config (pallet-mode t))

(use-package auto-compile  
  :config (auto-compile-on-load-mode))

(defun ben/apply-solarized-theme ()
  (interactive)
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t))

;; But at least load the solarized theme now!
(use-package solarized-theme
  :demand
  :config (ben/apply-solarized-theme))

(use-package exec-path-from-shell)


;; Font settings, inspired by
;; `https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org'
(setq ben/default-font "Hack")
(setq ben/current-font-size ben/default-font-size)
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

(when window-system
  (scroll-bar-mode -1))

;; I sometimes use code blocks in Org mode, so enable those.

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

(use-package pdf-tools
  ;; The :magic tag automatically turns on pdf-view-mode when PDF
  ;; files are opened.
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install))

(use-package helm
  ;; Override default key bindings with those from Helm
  :bind (("M-x" . #'helm-M-x)
         ("C-x r b" . #'helm-filtered-bookmarks)
         ("C-x b" . #'helm-buffers-list)
	 ("C-x C-f" . #'helm-find-files)
	 ("C-h a" . #'helm-apropos))
  :config (helm-mode 1))

(use-package company)

;; Unused, possibly remove?
(use-package writeroom-mode)

(use-package markdown-mode)

(use-package org-bullets)

(use-package smooth-scrolling 
  :init (smooth-scrolling-mode))

(use-package magit)

(use-package free-keys)

(defun ben/insert-time ()
  "Inserts the date and time into the current buffer."
  (interactive)
  (shell-command "date '+%A, %B %d %Y at %R'" 1))

(setq initial-scratch-message
      (format ";; Scratch buffer created on %s\n"
	      (shell-command-to-string "date '+%A, %B %d %Y at %R'")))

(defun ben/new-diary-entry ()
  "Creates a new buffer with a new diary entry with org mode
activated and a time stamp added."
  (interactive)
  (pop-to-buffer (generate-new-buffer-name "diary-"))
  (org-mode)
  (insert "* ")
  (ben/insert-time)
  (end-of-buffer))

;; My custom keybindings
(global-set-key (kbd "M-T") #'ben/insert-time)
(global-set-key (kbd "M-R") #'replace-string)
(global-set-key (kbd "C-)") #'ben/reset-font-size)
(global-set-key (kbd "C-+") #'ben/increase-font-size)
(global-set-key (kbd "C-=") #'ben/reset-font-size)
(global-set-key (kbd "C--") #'ben/decrease-font-size)

(setq ben/lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(defun ben/enable-lisp-editing-modes ()
  "Enables a collection of modes (such as paredit, rainbow delimiters,
aggressive indentation etc.) that greatly help with editing lisp
code."
  (progn (setq show-paren-style 'mixed)
	 (paredit-mode 1)
	 (rainbow-delimiters-mode 1)
	 (aggressive-indent-mode 1)
	 (show-paren-mode 1)
	 (undo-tree-mode 1)
	 (company-mode 1)))

(dolist (hook ben/lispy-mode-hooks)
  (add-hook hook #'ben/enable-lisp-editing-modes))

;; Enable some Lisp modes like paredit and rainbow delimiters, but no
;; need to undo and autocomplete.
(add-hook 'inferior-scheme-mode-hook 
	  (lambda ()
	    (progn (ben/enable-lisp-editing-modes)
		   (undo-tree-mode -1)
		   (company-mode -1))))

(defun ben/enable-writing-modes ()
  "Enables auto-fill mode, spell checking and disables company
mode. Although it looks like hard wrapping will warp the text on
org-export, it actually doesn't!"
  (progn (auto-fill-mode 1)
	 (undo-tree-mode 1)
	 (flyspell-mode 1)
	 (company-mode -1)))

(add-hook 'markdown-mode-hook #'ben/enable-writing-modes)
(add-hook 'org-mode-hook #'ben/enable-writing-modes)

(use-package emms
  :bind ("<f7>" . emms-smart-browse)
  :config (progn (require 'emms-setup)
		 (require 'emms-player-mplayer)
		 (emms-standard)
		 (emms-default-players)
		 (define-emms-simple-player mplayer '(file url)
		   (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
				 ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
				 ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
		   "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
		 
		 ;; You can change this to your favorite EMMS interface.
		 (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
		 
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

;; Finally, we have the essential-settings function that makes Emacs
;; usable for my personal setup.
(defun ben/essential-settings ()
  "Modifies a bunch of settings and enables a bunch of modes to
make Emacs nicer."
  (progn (display-time-mode 1)
	 (display-battery-mode 1)
	 (global-company-mode 1)
	 (setq-default cursor-type 'box)
	 (setq auto-save-interval 100)
	 (setq gc-cons-threshold 800000)
	 (setq inhibit-startup-screen t) 
	 (setq show-paren-style 'expression)
	 (setq backup-directory-alist
	       `(("." . ,(concat user-emacs-directory "backups"))))	 
	 (menu-bar-mode -1)
	 (tool-bar-mode -1)
	 (ben/reset-font-size) 
	 (exec-path-from-shell-initialize)))


(add-hook 'after-init-hook #'ben/essential-settings)
