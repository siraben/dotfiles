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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

(use-package diminish
  :hook (after-init . siraben--setup-diminish)
  :config
  (defun siraben--setup-diminish ()
    "Configure mode line diminishing."
    (diminish 'auto-revert-mode)
    (diminish 'visual-line-mode "vl")
    (diminish 'auto-fill-function "af")
    (diminish 'eldoc-mode)
    (diminish 'abbrev-mode)))
(use-package multiple-cursors
  :defer 3
  :commands (mc/mark-previous-like-this mc/mark-next-like mc/edit-lines mc/mark-more-like-this mc/mark-all-like-this)
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-m" . mc/mark-more-like-this)
         ("s-G" . mc/mark-all-like-this)))

(use-package rainbow-delimiters
  :hook
  ((racket-mode . rainbow-delimiters-mode)))

(use-package markdown-mode
  :hook
  (markdown-mode . (lambda ()
                     (markdown-toggle-wiki-links t)))
  :config
  (setq markdown-link-space-sub-char " "
        markdown-wiki-link-alias-first nil
        markdown-enable-math t
        markdown-fontify-code-blocks-natively t))

(use-package magit
  :commands (magit-dispatch magit-blame)
  :bind
  (("C-x g" . magit-dispatch)
   ("M-L" . magit-blame))
  :config
  ;; Don't show graph by default in log buffers
  (transient-remove-suffix 'magit-log "-g")
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  )

(use-package free-keys
  :commands free-keys)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs `(,(concat siraben-root-dir "snippets"))))

(use-package yasnippet-snippets)

(use-package paredit
  :diminish paredit-mode
  :hook ((racket-repl-mode . paredit-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :hook ((after-init . global-undo-tree-mode))
  :config
  ;; Compress undo-tree history files
  (advice-add 'undo-tree-make-history-save-file-name :filter-return
              (lambda (filename) (concat filename ".gz")))
  
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

(use-package company
  :diminish company-mode
  :config (global-company-mode t))

(use-package which-key
  :diminish
  :commands (which-key-mode)
  :hook (after-init . which-key-mode))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :hook (after-init . siraben--setup-exec-path)
    :config
    (defun siraben--setup-exec-path ()
      "Configure exec-path-from-shell for macOS."
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-envs
       '("PATH" "NIX_PATH" "NIX_SSL_CERT_FILE" "COQPATH")))))

(use-package helm
  :diminish helm-mode
  :bind (("C-h a"   . helm-apropos)
         ("C-h f"   . helm-apropos)
         ("C-h r"   . helm-info-emacs)
         ("C-x C-f" . helm-find-files)
         ("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-resume))
  :hook (after-init . helm-mode)
  :config
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t
        helm-autoresize-max-height            0
        helm-autoresize-min-height            40)
  (helm-autoresize-mode 1))

(use-package helm-rg
  :commands (helm-rg)
  :bind (("M-G" . helm-rg)))

;; Enable my favorite color scheme.
(use-package color-theme-sanityinc-tomorrow
  :demand
  :config (load-theme 'sanityinc-tomorrow-night t))

(use-package spaceline
  :hook (after-init . siraben--setup-spaceline)
  :config
  (defun siraben--setup-spaceline ()
    "Configure spaceline modeline."
    (setq powerline-default-separator 'arrow)
    (spaceline-emacs-theme)
    (spaceline-helm-mode)))

(use-package webpaste
  :config
  (setq webpaste-provider-priority '("dpaste.de" "ix.io"))
  (setq webpaste-paste-raw-text t))

(use-package auctex
  :hook (LaTeX-mode . siraben--setup-latex)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-PDF-mode t
        ;; Modern AUCTeX completion settings
        TeX-complete-expert-commands t
        TeX-insert-braces nil
        LaTeX-electric-left-right-brace t)
  
  ;; Enable preview-latex if available
  (when (locate-library "preview")
    (setq preview-scale-function 1.2))
  
  (defun siraben--setup-latex ()
    "Configure LaTeX mode settings."
    (siraben-enable-writing-modes)
    (setq TeX-command-extra-options "-shell-escape")
    (auto-fill-mode 1)
    (flyspell-mode 1)
    ;; Enable modern completion
    (setq-local completion-at-point-functions
                (append '(LaTeX-completion-at-point)
                        completion-at-point-functions))))

(use-package lsp-mode
  :disabled
  :config
  (require 'lsp-mode)
  ;; (advice-add 'lsp :before #'direnv-update-environment)
  (setq lsp-clients-clangd-args '("-j=4" "-log=error"))
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'interactive)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-file-watch-threshold 500)
  (setq lsp-enable-dap-auto-configure nil)
  ;; append "^~" to lsp-file-watch-ignored to ignore files in home directory
  (setq lsp-file-watch-ignored
        (append lsp-file-watch-ignored '("^~")))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "ruff")
  ;;                   :major-modes '(python-mode)
  ;;                   :server-id 'ruff))
  )

;; (use-package lsp-ui)

;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package flycheck
  :diminish)

(use-package direnv
  :disabled
  :config (direnv-mode))

(use-package envrc
  ;; add hook after init
  :hook (after-init . envrc-global-mode)
  )

(use-package esup
  :commands (esup)
  :config
  (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package yaml-mode)
(use-package build-farm)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package graphviz-dot-mode)

(use-package tree-sitter-langs
  :defer 3
  :demand
  :config
  (setq tree-sitter-load-path `(,(expand-file-name "~/.tree-sitter/bin"))))

(use-package solidity-mode)

(use-package tree-sitter
  :demand
  :diminish "ts"
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (setq tree-sitter-load-path `(,(expand-file-name "~/.tree-sitter/bin")))
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (setq tree-sitter-major-mode-language-alist
        `((nix-mode . nix)
          (markdown-mode . markdown)
          (latex-mode . latex)
          (yaml-mode . yaml)
          (typescript-mode . tsx)
          (conf-toml-mode . toml)
          (graphviz-dot-mode . dot)
          (makefile-bsdmake-mode . make)
          (sml-mode . sml)
          (solidity-mode . solidity)
          (kotlin-mode . kotlin)
          ,@tree-sitter-major-mode-language-alist))
  ;; remove haskell-mode
  (setq tree-sitter-major-mode-language-alist
        (assq-delete-all 'haskell-mode tree-sitter-major-mode-language-alist))
  )


(use-package vterm
  :if (require 'vterm-module nil t)
  :config
  (setq vterm-timer-delay nil))

(use-package multi-vterm
  :commands (multi-vterm)
  :bind ("<f8>" . multi-vterm))

(use-package projectile
  :commands (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package kotlin-mode
  :hook (kotlin-mode . lsp-deferred))

(use-package writeroom-mode)

(use-package copilot
  :disabled
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  :config
  (setq copilot-indent-offset-warning-disable t)
  )

(use-package boogie-friends)

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :bind
  ("M-." . lsp-bridge-find-def)
  ("M-," . lsp-bridge-find-def-return)
  )

(provide 'siraben-packages)
;;; siraben-packages.el ends here
