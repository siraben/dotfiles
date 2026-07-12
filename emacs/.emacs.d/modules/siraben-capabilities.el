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

(defcustom siraben-eglot-idle-delay 0.5
  "Idle time before starting an automatic Eglot connection.
This keeps language-server loading and process startup out of the
latency-sensitive file-opening command."
  :type 'number
  :group 'tools)

(defvar-local siraben-eglot--ensure-timer nil
  "Pending idle timer for automatic Eglot startup in this buffer.")

(defcustom siraben-eglot-loose-file-project-directory
  (expand-file-name "var/loose-file-project/" user-emacs-directory)
  "Small synthetic workspace used for loose files handled by Eglot.
Without this, project.el promotes a file's containing directory to a
transient project.  Opening /tmp/foo.py can therefore make a language server
index all of /tmp."
  :type 'directory
  :group 'tools)

(defun siraben-project-find-loose-file (_directory)
  "Return a bounded transient project for a loose LSP-capable file.
This finder is appended after normal project finders, so Git and other real
projects always win."
  (when (and buffer-file-name (assq major-mode siraben-lsp-servers))
    (make-directory siraben-eglot-loose-file-project-directory t)
    (cons 'transient
          (file-name-as-directory siraben-eglot-loose-file-project-directory))))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'siraben-project-find-loose-file t))

(defun siraben-eglot--ensure-buffer (buffer)
  "Start Eglot in BUFFER if it is still live and eligible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq siraben-eglot--ensure-timer nil)
      (when (and buffer-file-name
                 (siraben-have-p (cdr (assq major-mode siraben-lsp-servers))))
        (require 'eglot)
        (eglot-ensure)))))

(defun siraben-eglot-ensure-if-server-available ()
  "Schedule `eglot-ensure' when a known LSP server is installed.
Consult `siraben-lsp-servers' for the binaries to look for.  This is
meant to be used in `MODE-hook' clauses where blindly calling
`eglot-ensure' on a system without the server only pollutes the echo
area with \"Searching for program: No such file or directory\" messages.
The idle timer is important: mode hooks run synchronously inside
`find-file', so loading Eglot there makes Emacs ignore input."
  (let* ((servers (cdr (assq major-mode siraben-lsp-servers)))
         (chosen  (siraben-have-p servers)))
    (when chosen
      (when (timerp siraben-eglot--ensure-timer)
        (cancel-timer siraben-eglot--ensure-timer))
      (setq siraben-eglot--ensure-timer
            (run-with-idle-timer siraben-eglot-idle-delay nil
                                 #'siraben-eglot--ensure-buffer
                                 (current-buffer))))))

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
