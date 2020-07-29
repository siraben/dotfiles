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
(use-package writegood-mode)
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-m" . mc/mark-more-like-this)
         ("s-G" . mc/mark-all-like-this)))

(use-package erc-view-log)
(use-package clojure-mode)
(use-package cider)
(use-package xkcd)
(use-package rainbow-delimiters)
(use-package writeroom-mode)
(use-package markdown-mode)
(use-package neotree)
(use-package magit)

(use-package free-keys
  :commands free-keys)

(use-package fill-column-indicator)
(use-package memory-usage
  :commands memory-usage)

(use-package yasnippet-snippets)
(use-package demo-it)
(use-package ledger-mode
  :config
  (setq ledger-reconcile-default-commodity "THB"))

(use-package paredit
  :diminish paredit-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (lambda ()
            (global-undo-tree-mode +1)
            (defadvice undo-tree-make-history-save-file-name
                (after undo-tree activate)
              (setq ad-return-value (concat ad-return-value ".gz")))))

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
      helm-autoresize-min-height            40
      recentf-max-saved-items               200)

(add-hook 'after-init-hook '(lambda ()
                              (helm-mode 1)
                              (helm-autoresize-mode 1)))

(use-package paradox
  :config
  (setq paradox-github-token nil))

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile))

;; This is useful for testing out various commands.
(use-package lorem-ipsum)

(use-package nix-mode)
(use-package webpaste
  :config
  (setq webpaste-provider-priority '("dpaste.de" "ix.io"))
  (setq webpaste-paste-raw-text t))

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq TeX-PDF-mode t))

(use-package lsp-mode)
(use-package lsp-ui)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (company-auctex-init)
              (setq TeX-command-extra-options "-shell-escape")
              (flyspell-mode t)))

(use-package company-auctex)

(use-package vyper-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(when (locate-library "agda2-mode")
  (load-library "agda2-mode")
  (let ((base03    "#002b36") (base02    "#073642")
        (base01    "#586e75") (base00    "#657b83")
        (base0     "#839496") (base1     "#93a1a1")
        (base2     "#eee8d5") (base3     "#fdf6e3")
        (yellow    "#b58900") (orange    "#cb4b16")
        (red       "#dc322f") (magenta   "#d33682")
        (violet    "#6c71c4") (blue      "#268bd2")
        (cyan      "#2aa198") (green     "#859900"))
    (custom-set-faces
     `(agda2-highlight-keyword-face ((t (:foreground ,orange))))
     `(agda2-highlight-string-face ((t (:foreground ,magenta))))
     `(agda2-highlight-number-face ((t (:foreground ,violet))))
     `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
     `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
     `(agda2-highlight-bound-variable-face ((t nil)))
     `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
     `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
     `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
     `(agda2-highlight-field-face ((t (:foreground ,red))))
     `(agda2-highlight-function-face ((t (:foreground ,blue))))
     `(agda2-highlight-module-face ((t (:foreground ,violet))))
     `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
     `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
     `(agda2-highlight-record-face ((t (:foreground ,blue))))
     `(agda2-highlight-dotted-face ((t nil)))
     `(agda2-highlight-operator-face ((t nil)))
     `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
     `(agda2-highlight-unsolved-meta-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-unsolved-constraint-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03)))))))

(provide 'siraben-packages)
;;; siraben-packages.el ends here
