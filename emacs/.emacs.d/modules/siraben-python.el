;;; siraben-python.el --- Python programming customizations.

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

;; Keep loose files and large workspaces responsive.  Project-local config
;; files can still opt into whole-workspace diagnostics when appropriate.
(setq-default eglot-workspace-configuration
              '(:basedpyright.analysis
                (:diagnosticMode "openFilesOnly")))

(defun siraben-python--limit-loose-file-analysis ()
  "Keep basedpyright from treating a loose file's directory as a workspace.
Eglot represents files outside a recognized project as transient projects.
For those buffers, mirror the lightweight single-file behavior available in
modern editors: analyze open and imported files, but do not enumerate the
entire fallback directory."
  (unless (project-current nil)
    (setq-local eglot-workspace-configuration
                '(:basedpyright.analysis
                  (:diagnosticMode "openFilesOnly"
                   :exclude ["**"])))))

(add-hook 'python-mode-hook #'siraben-python--limit-loose-file-analysis)
(add-hook 'python-ts-mode-hook #'siraben-python--limit-loose-file-analysis)

;; Capability-driven Python LSP setup.
;;
;; Preferred stack: basedpyright (type checking) + ruff (lint / format)
;; multiplexed by `rass'.  If `rass' is missing we fall back to plain
;; basedpyright; if that is also missing we leave the defaults alone and
;; let `siraben-eglot-ensure-if-server-available' decide whether eglot
;; should fire at all.
(with-eval-after-load 'eglot
  (require 'siraben-capabilities)
  (cond
   ((and (siraben-have-p "rass")
         (siraben-have-p "basedpyright-langserver"))
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) .
                   ("rass" "--" "basedpyright-langserver" "--stdio" "--" "ruff" "server"))))
   ((siraben-have-p "basedpyright-langserver")
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) .
                   ("basedpyright-langserver" "--stdio"))))))

(provide 'siraben-python)
;;; siraben-python.el ends here
