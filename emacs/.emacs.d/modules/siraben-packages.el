
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
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure `use-package' is installed.
(straight-use-package 'use-package)

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1)

(require 'use-package)

(setq use-package-compute-statistics t)

;; Ensure that all packages are downloaded to their latest version,
;; but also defer them to speed up init.
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

(use-package rainbow-delimiters)
(use-package writeroom-mode)
(use-package markdown-mode
  :hook
  (markdown-mode . (lambda ()
                     (auto-fill-mode -1)
                     (markdown-toggle-wiki-links t)))
  :config
  (setq markdown-link-space-sub-char " "
        markdown-wiki-link-alias-first nil
        markdown-enable-math t))

(use-package magit
  :bind (("C-x g"   . 'magit-status)
         ("C-x M-g" . 'magit-dispatch))
  :config
  (when (eq system-type 'darwin)
    (setq magit-git-executable (executable-find "git"))))


(use-package diff-hl
  :after magit
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode))

(use-package forge
  :after magit)

(use-package free-keys
  :commands free-keys)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs `(,(concat siraben-root-dir "snippets"))))

(use-package yasnippet-snippets)

(use-package ledger-mode
  :config
  (setq ledger-reconcile-default-commodity "USD"))

(use-package paredit
  :diminish paredit-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (defadvice undo-tree-make-history-save-file-name
                (after undo-tree activate)
              (setq ad-return-value (concat ad-return-value ".gz")))
            (global-undo-tree-mode)
            ))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

(use-package company
  :diminish company-mode
  :config (global-company-mode t))

(use-package which-key
  :diminish
  :init (which-key-mode t))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :demand
    :hook
    (after-init . (lambda ()
                    (setenv "SHELL" "/bin/zsh")
                    (exec-path-from-shell-initialize)
                    (exec-path-from-shell-copy-envs
                     '("PATH" "NIX_PATH" "NIX_SSL_CERT_FILE" "COQPATH"))))))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 3))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode))

(use-package helm
  :diminish helm-mode
  :config
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t
        helm-autoresize-max-height            0
        helm-autoresize-min-height            40
        helm-autoresize-mode                  t
        recentf-max-saved-items               1000
        recentf-keep                          nil)
  :bind (("C-h a"   . 'helm-apropos)
         ("C-h f"   . 'helm-apropos)
         ("C-h r"   . 'helm-info-emacs)
         ("C-x C-f" . 'helm-find-files)
         ("M-x"     . 'helm-M-x)
         ("C-x b"   . 'helm-mini))
  :hook
  (after-init . helm-mode))

(use-package helm-rg
  :bind (("M-G" . 'helm-rg)))

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile))

(use-package webpaste
  :config
  (setq webpaste-provider-priority '("dpaste.de" "ix.io"))
  (setq webpaste-paste-raw-text t))

(use-package auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-PDF-mode t)
  :hook
  (LaTeX-mode . (lambda ()
                  (company-auctex-init)
                  (setq TeX-command-extra-options "-shell-escape")
                  (auto-fill-mode 1)
                  (flyspell-mode t))))

(use-package lsp-mode
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-log=error")))

(use-package lsp-ui)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package company-auctex)

(use-package vyper-mode)

(use-package flycheck
  :diminish)

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :bind (("C-c e" . envrc-command-map)))

(use-package esup
  :config (setq esup-user-init-file (file-truename "~/.emacs.d/init.el"))
  :commands (esup))

(use-package yaml-mode)
(use-package build-farm)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package dart-mode)
(use-package lsp-dart
  :hook (dart-mode . lsp)
  :config
  (setq lsp-dart-sdk-dir "/usr/local/Caskroom/flutter/2.2.3/flutter/bin/cache/dart-sdk"
        lsp-dart-flutter-widget-guides nil
        lsp-dart-line-length 120))

(use-package flutter)

(use-package tree-sitter-langs
  :demand)

(use-package tree-sitter
  :demand
  :diminish "ts"
  :after tree-sitter-langs
  :config
  (push (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (setq tree-sitter-major-mode-language-alist
        `((nix-mode . nix)
          (markdown-mode . markdown)
          (latex-mode . latex)
          (yaml-mode . yaml)
          (toml-mode . toml)
          (typescript-mode . tsx)
          (haskell-mode . haskell)
          ,@tree-sitter-major-mode-language-alist))
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'siraben-packages)
;;; siraben-packages.el ends here
