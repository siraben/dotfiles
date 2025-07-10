;;; siraben-editor.el --- make Emacs a great editor.

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

;;; Code:

(add-hook 'after-init-hook #'siraben--setup-global-modes)

(defun siraben--setup-global-modes ()
  "Setup global modes for better editing experience."
  (global-auto-revert-mode 1)
  (global-so-long-mode 1))

;; Don't use tabs to indent but maintain correct appearance.
(setq-default indent-tabs-mode nil
              tab-width        4)
(setq tab-always-indent 'complete)

;; Require newline at end of file.
(setq require-final-newline t)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Blinking parens are ugly.
(setq blink-matching-paren nil)

(require 'windmove)
(windmove-default-keybindings)

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil))

(defun siraben-enable-writing-modes ()
  "Enable writing modes for writing prose.
Enables auto-fill mode, spell checking and disables company mode."
  (interactive)
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (electric-pair-mode 1)
  (setq electric-pair-inhibit-predicate
        `(lambda (c)
           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
  (visual-line-mode 1)
  (company-mode -1))

;; This will be handled by the diminish package configuration

(add-hook 'text-mode-hook #'siraben-enable-writing-modes)
(add-hook 'markdown-mode-hook #'siraben-enable-writing-modes)
(add-hook 'org-mode-hook #'siraben-enable-writing-modes)

(require 'siraben-mdm)

;; De-duplicate kill ring entries.
(setq kill-do-not-save-duplicates t)

;; Save the system clipboard when we put something in the kill ring in
;; Emacs.
(setq save-interprogram-paste-before-kill t)

;;; Dired stuff
;; In case we want to restore the file we deleted..
(setq delete-by-moving-to-trash t)
(use-package async
  :hook (after-init . dired-async-mode))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "M-G") nil))

(setq ffap-machine-p-known 'reject)

(use-package dashboard
  :hook (after-init . siraben--setup-dashboard)
  :config
  (defun siraben--setup-dashboard ()
    "Configure dashboard startup screen."
    (dashboard-mode)
    (dashboard-refresh-buffer))
  
  (setq dashboard-set-footer nil
        dashboard-startup-banner 'logo
        dashboard-items '((recents . 10))
        dashboard-center-content t)
  (dashboard-setup-startup-hook))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; By MysteriousSilver on #emacs, Friday, June 18, 2021 at 07:34 UTC

(defun anon-new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (text-mode)
    (setq buffer-offer-save t)
    $buf))

(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-N") #'anon-new-empty-buffer)

(setq auto-save-interval 100
      kept-new-versions 10
      kept-old-versions 0
      backup-inhibited t
      delete-old-versions t)

(setq recentf-max-saved-items 10000
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:"))

(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'paren)
(set-face-background 'show-paren-match nil)
(set-face-background 'show-paren-mismatch nil)
(set-face-foreground 'show-paren-match "#ff0")
(set-face-foreground 'show-paren-mismatch "#f00")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(use-package helm-swoop
  :commands (helm-swoop-without-pre-input helm-swoop)
  :bind
  (("C-S-s" . helm-swoop)))

(provide 'siraben-editor)
;;; siraben-editor.el ends here
