;;; siraben-packages.el --- Set up MELPA packages

;;; Commentary:
;; This file sets up `use-package', which handles the installation of
;; most packages.

;;; Code:

;; Initialize package.el
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (progn (package-refresh-contents)
	 (package-install 'use-package)))

(require 'use-package)

(setq use-package-compute-statistics t)

;; Ensure that all packages are downloaded to their latest version,
;; but also defer them to speed up init.
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-verbose t)

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  :demand)
(use-package auto-compile
  :demand
  :config
  (auto-compile-on-load-mode))

;; These packages add things to the mode line when they're activated,
;; I don't want them to clutter my screen.
(use-package diminish)

(use-package paredit 
  :diminish paredit-mode)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

(use-package company
  :diminish company-mode)

(use-package which-key
  :diminish)

(add-hook 'after-init-hook #'(lambda () (which-key-mode 1)))

(use-package exec-path-from-shell
  :disabled
  :demand
  :config (progn (setq exec-path-from-shell-check-startup-files nil)
                 (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters)
(use-package writeroom-mode)
(use-package markdown-mode)
(use-package neotree)
(use-package magit)
(use-package free-keys)
(use-package fill-column-indicator)
(use-package memory-usage)

(use-package auto-package-update 
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 3))

(use-package pdf-tools
  ;; The :magic tag automatically turns on pdf-view-mode when PDF
  ;; files are opened.
  :magic ("%PDF" . pdf-view-mode))

(use-package helm 
  ;; Override default key bindings with those from Helm 
  :bind (("C-h a"   . 'helm-apropos)
         ("C-h f"   . 'helm-apropos)
         ("C-h r"   . 'helm-info-emacs)
         ("C-x C-f" . 'helm-find-files)
         ("M-x"     . 'helm-M-x)
         ("C-x b"   . 'helm-mini)))

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      helm-autoresize-max-height            0
      helm-autoresize-min-height            20)

(add-hook 'after-init-hook '(lambda ()
                              (helm-mode 1)
                              (helm-autoresize-mode 1)))

;; This one has a pretty long configuration, it's a music player in Emacs.
(use-package emms
  :bind ("<f7>" . emms-smart-browse)
  :config
  (require 'emms-setup)
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
      (emms-default-players))))

(use-package clojure-mode)
(use-package cider)
(use-package paradox
  :config
  (setq paradox-github-token nil))
(use-package erc-view-log)
(use-package geiser)

(use-package guru-mode)

(provide 'siraben-packages)

;;; siraben-packages.el ends here
