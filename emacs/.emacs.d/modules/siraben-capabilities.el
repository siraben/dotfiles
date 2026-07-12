;;; siraben-capabilities.el --- Capability detection for external tools

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

;; This module centralises detection of external programs (compilers,
;; language servers, formatters, etc.) so that the rest of the config
;; can be capability-driven: features auto-enable when their backing
;; binary is installed and silently skip otherwise.
;;
;; Use:
;;   (siraben-have-p "rust-analyzer")            ; cached boolean
;;   (siraben-when-program "clang-format" ...)   ; eval body conditionally
;;   (siraben-eglot-ensure-if-server-available)  ; safe LSP hook
;;
;; The cache is invalidated by `siraben-capabilities-refresh'.

;;; Code:

(require 'cl-lib)

(defvar siraben-capabilities--cache (make-hash-table :test 'equal)
  "Memoization table for `siraben-have-p'.")

(defun siraben-capabilities-refresh ()
  "Forget every cached executable lookup."
  (interactive)
  (clrhash siraben-capabilities--cache))

(defun siraben-have-p (program)
  "Return non-nil when PROGRAM is on `exec-path' (cached).
PROGRAM may be a string or a list of alternatives; for a list the
first installed one is returned."
  (cond
   ((null program) nil)
   ((listp program)
    (cl-some #'siraben-have-p program))
   (t
    (let ((cached (gethash program siraben-capabilities--cache 'unset)))
      (if (eq cached 'unset)
          (let ((found (executable-find program)))
            (puthash program found siraben-capabilities--cache)
            found)
        cached)))))

(defmacro siraben-when-program (program &rest body)
  "Evaluate BODY only if PROGRAM is installed.
PROGRAM is a string (or list of alternatives) accepted by
`siraben-have-p'.  Returns the body's value or nil."
  (declare (indent 1))
  `(when (siraben-have-p ,program)
     ,@body))

;; ---------------------------------------------------------------------------
;; eglot helpers

(defvar siraben-lsp-servers
  '((python-mode      . ("basedpyright-langserver" "pyright-langserver"
                         "pylsp" "jedi-language-server"))
    (python-ts-mode   . ("basedpyright-langserver" "pyright-langserver"
                         "pylsp" "jedi-language-server"))
    (rustic-mode      . ("rust-analyzer"))
    (rust-mode        . ("rust-analyzer"))
    (rust-ts-mode     . ("rust-analyzer"))
    (typescript-mode  . ("typescript-language-server" "tsserver"))
    (typescript-ts-mode . ("typescript-language-server" "tsserver"))
    (tsx-ts-mode        . ("typescript-language-server" "tsserver"))
    (kotlin-mode      . ("kotlin-language-server"))
    (c-mode           . ("clangd" "ccls"))
    (c++-mode         . ("clangd" "ccls"))
    (c-ts-mode        . ("clangd" "ccls"))
    (c++-ts-mode      . ("clangd" "ccls"))
    (nix-mode         . ("nil" "nixd" "rnix-lsp"))
    (haskell-mode     . ("haskell-language-server-wrapper"
                         "haskell-language-server")))
  "Alist of (MAJOR-MODE . LIST-OF-LSP-BINARIES) used by
`siraben-eglot-ensure-if-server-available'.  The first installed
binary in the list wins.")

(defun siraben-eglot-ensure-if-server-available ()
  "Like `eglot-ensure', but no-op if no known LSP server is installed.
Consult `siraben-lsp-servers' for the binaries to look for.  This is
meant to be used in `MODE-hook' clauses where blindly calling
`eglot-ensure' on a system without the server only pollutes the echo
area with \"Searching for program: No such file or directory\" messages."
  (let* ((servers (cdr (assq major-mode siraben-lsp-servers)))
         (chosen  (siraben-have-p servers)))
    (when chosen
      (require 'eglot)
      (eglot-ensure))))

;; ---------------------------------------------------------------------------
;; Reporting

(defun siraben-capabilities-report ()
  "Display a buffer listing which optional tools are detected.
Useful to see at a glance what is enabled on the current host."
  (interactive)
  (let ((buf (get-buffer-create "*siraben-capabilities*"))
        (tools '(;; LSP servers
                 "basedpyright-langserver" "pyright-langserver" "pylsp"
                 "rust-analyzer"
                 "typescript-language-server" "tsserver"
                 "clangd" "ccls"
                 "nil" "nixd"
                 "haskell-language-server-wrapper"
                 "kotlin-language-server"
                 ;; Formatters / linters
                 "clang-format" "rustfmt" "black" "ruff"
                 "prettier" "nixpkgs-fmt" "ormolu" "stylish-haskell"
                 ;; Multiplexers
                 "rass"
                 ;; Build / package tools
                 "cargo" "rustc" "cabal" "stack" "ghc" "nix" "make"
                 ;; Shells
                 "zsh" "bash" "fish" "pwsh"
                 ;; Lisps
                 "sbcl" "ccl" "clisp"
                 ;; Misc
                 "git" "rg" "direnv")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; Capabilities on %s (%s)\n\n"
                        system-type
                        (or (and (boundp 'system-configuration)
                                 system-configuration)
                            "?")))
        (dolist (tool tools)
          (insert (format "  %-40s %s\n"
                          tool
                          (or (siraben-have-p tool) "—"))))
        (special-mode)))
    (pop-to-buffer buf)))

(provide 'siraben-capabilities)
;;; siraben-capabilities.el ends here
