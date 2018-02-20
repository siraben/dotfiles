;;; siraben-packages.el
;; This file sets up `use-package' and installs other packages.

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

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

(use-package auto-compile
  :demand
  :config (auto-compile-on-load-mode))

(use-package diminish)
(use-package paredit
  :diminish paredit-mode)
(use-package rainbow-delimiters)
(use-package undo-tree
  :diminish undo-tree-mode)
(use-package aggressive-indent
  :diminish aggressive-indent-mode)
(use-package company
  :diminish company-mode)
(use-package writeroom-mode)
(use-package markdown-mode)
(use-package org-bullets)
(use-package neotree)
(use-package magit)
(use-package free-keys)
(use-package fill-column-indicator)

(use-package auto-package-update 
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 4)
  (auto-package-update-maybe))

;; I sometimes use code blocks in Org mode, so enable those.

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (calc . t)
   (python . t)
   (scheme . t)))

(use-package pdf-tools
  ;; The :magic tag automatically turns on pdf-view-mode when PDF
  ;; files are opened.
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install))

(setq helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(use-package helm 
  ;; Override default key bindings with those from Helm
  :bind (("C-h a" . #'helm-apropos)
	 ("M-y" . #'helm-show-kill-ring)))


(use-package smex
  :bind (("M-x" . #'smex)))

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

(provide 'siraben-packages)
