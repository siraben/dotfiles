;;; siraben-bootstrap.el --- Source-verified initial straight bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Authenticate the initial straight checkout instead of evaluating a floating
;; installer.  Once installed, use straight's ordinary pull/freeze/thaw commands;
;; startup does not enforce old checkout revisions or reset package state.

;;; Code:
(require 'subr-x)

(defconst siraben-straight-commit "64e19d65d35d47b7ac9099e42757d07008d07f19")
(defconst siraben-straight-hashes
  '(("bootstrap.el" . "f40d94e176373f9a1bc2d0061acdad11d3b1de66bb19994d846c0c2afcda8418")
    ("straight.el" . "b8ac7d1e972cc1644aa0d9e56b0fa2dd038db1777b2257645f61e257a03176e3")))

(defun siraben-bootstrap-git (directory &rest args)
  "Run Git ARGS in DIRECTORY, returning output or signaling failure."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "Bootstrap Git failed (%S): %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun siraben-bootstrap ()
  "Authenticate a fresh straight bootstrap, then use normal package management."
  (when (version< emacs-version "30.1")
    (error "This configuration requires Emacs 30.1 or newer"))
  (setq straight-repository-branch "develop")
  (let* ((repos (expand-file-name "straight/repos/" user-emacs-directory))
         (repo (expand-file-name "straight.el/" repos)))
    (unless (file-directory-p repo)
      (unless (executable-find "git") (error "Bootstrap requires Git"))
      (make-directory repos t)
      (let ((staging (make-temp-file (expand-file-name ".straight-" repos) t)))
        (unwind-protect
            (progn
              (siraben-bootstrap-git staging "init" "--quiet")
              (siraben-bootstrap-git staging "remote" "add" "origin"
                                     "https://github.com/radian-software/straight.el.git")
              (siraben-bootstrap-git staging "fetch" "--depth" "1" "origin"
                                     siraben-straight-commit)
              (siraben-bootstrap-git staging "checkout" "--detach" "FETCH_HEAD")
              (unless (equal (siraben-bootstrap-git staging "rev-parse" "HEAD")
                             siraben-straight-commit)
                (error "straight bootstrap checkout does not match its pin"))
              (dolist (entry siraben-straight-hashes)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally (expand-file-name (car entry) staging))
                  (unless (equal (secure-hash 'sha256 (current-buffer)) (cdr entry))
                    (error "straight bootstrap integrity failure: %s" (car entry)))))
              (siraben-bootstrap-git staging "update-ref" "refs/remotes/origin/develop"
                                     siraben-straight-commit)
              (rename-file staging (directory-file-name repo)))
          (when (file-exists-p staging) (delete-directory staging t)))))
    (load (expand-file-name "bootstrap.el" repo) nil 'nomessage)))

(provide 'siraben-bootstrap)
;;; siraben-bootstrap.el ends here
