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

(add-hook 'after-init-hook '(lambda () (global-auto-revert-mode t) (global-so-long-mode t)))
;; Dynamic binding sucks.
(setq lexical-binding t)

;; Don't use tabs to indent but maintain correct appearance.
(setq-default indent-tabs-mode nil
              tab-width        8)
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

(require 'undo-tree)
;; Auto-save the undo-tree history.
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; Blinking parens are ugly.
(setq blink-matching-paren nil)

(require 'windmove)
(windmove-default-keybindings)

(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(defun siraben-enable-writing-modes ()
  "Enables writing modes for writing prose.
Enables auto-fill mode, spell checking and disables company mode."
  (interactive)
  (progn (auto-fill-mode 1)
	 (undo-tree-mode 1)
	 (flyspell-mode 1)
         (electric-pair-mode 1)
         (visual-line-mode 1)    ;; Org mode headings don't wrap.
         (company-mode -1)))


(add-hook 'markdown-mode-hook #'siraben-enable-writing-modes)
(add-hook 'org-mode-hook #'siraben-enable-writing-modes)

(require 'siraben-mdm)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; De-duplicate kill ring entries.
(setq kill-do-not-save-duplicates t)

;; Save the system clipboard when we put something in the kill ring in
;; Emacs.
(setq save-interprogram-paste-before-kill t)

;;; Dired stuff
;; In case we want to restore the file we deleted..
(setq delete-by-moving-to-trash t)
(add-hook 'after-init-hook 'dired-async-mode)


(setq ffap-machine-p-known 'reject)

(use-package dashboard
  :demand
  :config
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents . 10)))
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

(global-set-key (kbd "M-n") #'anon-new-empty-buffer)

(provide 'siraben-editor)
;;; siraben-editor.el ends here
