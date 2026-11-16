;;; siraben-typescript.el --- TypeScript editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer Emacs treesit with installed grammars; preserve the legacy fallback.
;; Grammar installation is explicit, never a side effect of visiting a file.

;;; Code:
(use-package typescript-mode)

(defun siraben-typescript-mode ()
  "Select native TypeScript/TSX when its grammar is ready, else legacy mode."
  (interactive)
  (let* ((tsx (and buffer-file-name (string-match-p "\\.tsx\\'" buffer-file-name)))
         (language (if tsx 'tsx 'typescript)))
    (if (treesit-ready-p language t)
        (if tsx (tsx-ts-mode) (typescript-ts-mode))
      (typescript-mode))))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . siraben-typescript-mode))
(setq typescript-indent-level 2
      typescript-ts-mode-indent-offset 2
      js-indent-level 2)

(provide 'siraben-typescript)
;;; siraben-typescript.el ends here
