;;; siraben-packages.el --- Set up MELPA packages

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

;; These packages don't require any configuration, so let's just clump
;; them together.
(use-package diminish)
(use-package svg-clock)
(use-package writegood-mode)
(use-package mark-multiple)
(use-package multiple-cursors)
(use-package svg-clock)
(use-package erc-view-log)
(use-package clojure-mode)
(use-package cider)
(use-package xkcd)
(use-package rainbow-delimiters)
(use-package writeroom-mode)
(use-package markdown-mode)
(use-package neotree)
(use-package magit)
(use-package free-keys)
(use-package fill-column-indicator)
(use-package memory-usage)
(use-package yasnippet-snippets)
(use-package demo-it)
(use-package ledger-mode)

(use-package paredit
  :diminish paredit-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (lambda ()
            (global-undo-tree-mode +1)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

(use-package company
  :diminish company-mode)

(use-package which-key
  :diminish)

(add-hook 'after-init-hook #'(lambda () (which-key-mode t)))

(use-package exec-path-from-shell
  :demand
  :config (progn (setq exec-path-from-shell-check-startup-files nil)
                 (exec-path-from-shell-initialize)))


(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 3))

(use-package pdf-tools
  ;; The :magic tag automatically turns on pdf-view-mode when PDF
  ;; files are opened.
  :magic ("%PDF" . pdf-view-mode))

(use-package helm
  :demand
  ;; Override default key bindings with those from Helm
  :bind (("C-h a"   . 'helm-apropos)
         ("C-h f"   . 'helm-apropos)
         ("C-h r"   . 'helm-info-emacs)
         ("C-x C-f" . 'helm-find-files)
         ("M-x"     . 'helm-M-x)
         ("C-x b"   . 'helm-mini)))

(use-package helm-ag)

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

(use-package paradox
  :config
  (setq paradox-github-token nil))

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile))

(use-package exwm
  :disabled
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default))

(use-package define-word
  :bind (("C-M-=" . define-word-at-point)))

;; This is useful for testing out various commands.
(use-package lorem-ipsum)

(use-package forth-mode)

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'libnotify))

;; (use-package quelpa-use-package
;;   :demand)

;; (use-package matrix-client
;;   :defer t
;;   :ensure nil
;;   :quelpa (matrix-client-ng
;;            :fetcher github
;;            :repo "jgkamat/matrix-client-el"
;;            :branch "client-ng")

;;   :commands (matrix-client)
;;   :config
;;   (setq matrix-client-render-membership nil
;;         matrix-client-render-presence nil
;;         matrix-client-use-tracking t
;;         matrix-client-save-token t))

(provide 'siraben-packages)
;;; siraben-packages.el ends here
