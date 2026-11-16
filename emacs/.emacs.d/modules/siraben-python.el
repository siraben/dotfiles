;;; siraben-python.el --- Python programming customizations. -*- lexical-binding: t; -*-

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

;; Configures packages and various modes for Python programming.

;;; Code:

(require 'use-package)

(use-package pyvenv)

;; Analyze open files rather than eagerly checking the whole workspace.
(setq-default eglot-workspace-configuration
              '(:basedpyright.analysis (:diagnosticMode "openFilesOnly")))

(defun siraben-python--limit-loose-file-analysis ()
  "Avoid enumerating broad fallback directories for standalone files."
  (unless (project-current nil)
    (setq-local eglot-workspace-configuration
                '(:basedpyright.analysis
                  (:diagnosticMode "openFilesOnly" :exclude ["**"])))))

(add-hook 'python-mode-hook #'siraben-python--limit-loose-file-analysis)
(add-hook 'python-ts-mode-hook #'siraben-python--limit-loose-file-analysis)

(provide 'siraben-python)
;;; siraben-python.el ends here
