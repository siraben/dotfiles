;;; siraben-arcadia.el --- provides integration with Arcadia.

;;; Commentary:
;; Emacs interoperability with the Clojure REPL in Arcadia.  See
;; `https://github.com/arcadia-unity/Arcadia' for more.

;;; Code:

(defcustom unity-repl-command "ruby repl-client.rb"
  "Command to use for arcadia-repl.")

(defcustom unity-repl-command-path "Assets/Arcadia/Editor"
  "Launch the REPL command in this relative path.")

(defun unity-root-p (dir)
  "Is this DIR the root of a Unity project?"
  (-any? (lambda (f)
           ;; TODO: Maybe this could be better?
           (string-equal f "ProjectSettings"))
         (directory-files dir)))

(defun unity-find-root (start levels)
  "Search from the START directory to find the Unity root
directory and return its full path.  Search for the number of
LEVELS specified."
  (cond ((= levels 0) nil)
        ((unity-root-p start) start)
        (t (unity-find-root
            (expand-file-name ".." start) (- levels 1)))))

(defun unity-jack-in ()
  "Start the Arcadia REPL."
  (interactive)
  (let ((default-directory
          (concat (unity-find-root default-directory 10) "/"
                  unity-repl-command-path "/")))
    (run-lisp unity-repl-command)))

(provide 'siraben-arcadia)
;;; siraben-arcadia.el ends here

