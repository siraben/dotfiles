;;; -*- lexical-binding: t -*-
;;; sutysisku.el --- Sutysisku for Helm
;; Copyright (C) 2018 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (request "0") (helm "0") (a "0") (ivy "0"))
;; Keywords: hydra
;; URL: http://github.com/dustinlacewell/hera

;;; Commentary:

;; This package offers Lojban dictionary search with Helm

;;; Code:
(require 'cl)
(require 'a)
(require 'helm)
(require 'ivy)
(require 'request)

(setq sutysisku/data-url
      "https://rawgit.com/La-Lojban/sutysisku/master/data/parsed-en.js")

(setq sutysisku--data nil)

(setq sutysisku--matches nil)

(defun sutysisku--propertize-regex (regexp string &rest props)
  (let* ((matches (s-matched-positions-all regexp string)))
    (cl-loop for match in matches
             for start = (car match)
             for end = (cdr match)
             do (add-text-properties start end props string))
    string))

(defun sutysisku--word-match-p (record)
  (let ((word (a-get record :word)))
    (if (or (string-equal "" helm-pattern)
            (string-equal "" word))
        nil
      (string-match-p helm-pattern word))))

(defun sutysisku--gloss-match-p (record)
  (let ((word (a-get record :word))
        (gloss (a-get record :gloss)))
    (if (or (string-equal "" helm-pattern)
            (string-equal "" gloss))
        nil
      (string-match-p gloss helm-pattern))))

(defun sutysisku--word-or-gloss-match-p (record)
  (or (sutysisku--word-match-p record)
      (sutysisku--gloss-match-p record)))

(defun sutysisku--definition-match-p (record)
  (string-match-p helm-pattern (a-get record :definition)))

(defun sutysisku--format-display (record)
  (format
   "%s %s\n%s"
   (propertize (a-get record :word)
               'face '(:height 2.0 :weight bold))
   (propertize (a-get record :gloss)
               'face '(:foreground "light grey" :slant italic))
   (sutysisku--propertize-regex
    "\\[[^]]*\\]"
    (sutysisku--propertize-regex
     "X[[:digit:]]+"
     (a-get record :definition)
     'face '(:foreground "deep sky blue"))
    'face '(:foreground "dim grey"))))

(defun sutysisku--clean-definition (definition)
  (replace-regexp-in-string
   "\\$[[:alpha:]]+_\\([[:digit:]]\\)=[[:alpha:]]+_[[:digit:]]+\\$" "X\\1"
   (replace-regexp-in-string
    "\\$[[:alpha:]]_\\([[:digit:]]\\)\\$" "X\\1"
    (replace-regexp-in-string
     "\\$[[:alpha:]]_\{\\([[:digit:]]+\\)\}\\$" "X\\1"
     (decode-coding-string definition 'utf-8)))))

(defun sutysisku--clean-data (candidates)
  (let ((candidates (cl-loop for c in candidates
                             for word = (format "%s" (car c))
                             for record = (cdr c)
                             for definition = (sutysisku--clean-definition (a-get record 'd))
                             for type = (a-get record 't)
                             for gloss = (decode-coding-string (a-get record 'g) 'utf-8)
                             for record = (a-list :word word
                                                  :gloss gloss
                                                  :type type
                                                  :definition definition)
                             for display = (sutysisku--format-display record)
                             for full-record = (a-assoc record :display display )
                             collect (cons display full-record))))
    (message "Finished cleaning.")
    candidates))

(defun sutysisku--filtered-transform (pred candidates)
  (let ((results (cl-loop for c in candidates
                          for record = (cdr c)
                          if (apply pred (list record))
                          collect c)))
    results))

(setq sutysisku--word-match-source
      (helm-build-sync-source "Word Match"
        :multiline t
        :candidates 'sutysisku--data
        :filtered-candidate-transformer
        (lambda (c s)
          (sutysisku--filtered-transform
           'sutysisku--word-match-p c))))

(setq sutysisku--gloss-match-source
      (helm-build-sync-source "Gloss Match"
        :multiline t
        :candidates 'sutysisku--data
        :filtered-candidate-transformer
        (lambda (c s)
          (sutysisku--filtered-transform
           'sutysisku--gloss-match-p c))))

(setq sutysisku--word-or-gloss-match-source
      (helm-build-sync-source "Word or Gloss Match"
        :multiline t
        :candidates 'sutysisku--data
        :filtered-candidate-transformer
        (lambda (c s)
          (sutysisku--filtered-transform
           'sutysisku--word-or-gloss-match-p c))))

(setq sutysisku--definition-match-source
      (helm-build-sync-source "Definition Match"
        :multiline t
        :candidates 'sutysisku--data
        :filtered-candidate-transformer
        (lambda (c s)
          (sutysisku--filtered-transform
           'sutysisku--definition-match-p c))))

(defun sutysisku-fetch (&optional then)
  (interactive)
  (message "Downloading wordlist...")
  (request
   sutysisku/data-url
   :sync nil
   :parser (lambda () (search-forward "= ") (json-read))
   :error (lambda (&key error-thrown &accept-other-keys &rest _)
            (message (format "Error: %s" error-thrown)))
   :success (lambda (&key data &accept-other-keys &rest _)
              (message (format "%s words downloaded. Cleaning..." (length data)))
              (setq sutysisku--data (sutysisku--clean-data data))
              (message "Done!")
              (when (functionp then) (funcall then)))))

(defun sutysisku-search ()
  (interactive)
  (if (> (length sutysisku--data) 0)
      (helm
       :init (lambda (setq sutysisku--matches nil))
       :candidate-number-limit nil
       :sources '(sutysisku--word-match-source
                  sutysisku--gloss-match-source
                  sutysisku--definition-match-source))

    (sutysisku-fetch 'sutysisku-search)))

(defun sutysisku-ivy-candidates (str)
  (when (and (not (equal str nil)) (not (equal str "")))
    (let ((exact)
          (gloss-exact)
          (word-prefix)
          (word-substring)
          (gloss-prefix)
          (gloss-substring)
          (definition-substring))
      (cl-loop for item in sutysisku--data
               for display = (car item)
               for record = (cdr item)
               for word = (a-get record :word)
               for gloss = (a-get record :gloss)
               for definition = (a-get record :definition)
               do (add-text-properties 0 1 `(record ,record) display)
               do (cond
                   ((s-equals? str word)
                    (setf exact (append (list display) exact)))

                   ((s-equals? str gloss)
                    (setf gloss-exact (append (list display) exact)))

                   ((s-prefix? str word)
                    (setf word-prefix (append (list display) word-prefix)))

                   ((s-contains? str word)
                    (setf word-substring (append (list display) word-substring)))

                   ((s-prefix? str gloss)
                    (setf gloss-prefix (append (list display) gloss-prefix)))

                   ((s-contains? str gloss)
                    (setf gloss-substring (append (list display) gloss-substring)))

                   ((s-contains? str definition)
                    (setf definition-substring (append (list display) definition-substring)))))
      (append
       exact gloss-exact
       word-prefix word-substring
       gloss-prefix gloss-substring
       definition-substring))))

(defun sutysisku--search-ivy-kill-word-action (entry)
  (let* ((record (get-text-property 0 'record entry))
         (word (a-get record :word)))
    (kill-new word)))

(defun sutysisku--search-ivy-kill-definition-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (a-get record :definition))))

(defun sutysisku--search-ivy-kill-gloss-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (a-get record :gloss))))

(defun sutysisku--search-ivy-kill-all-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (format "%s (%s): %s"
                      (a-get record :word)
                      (a-get record :gloss)
                      (a-get record :definition)))))

(defun sutysisku-search-ivy ()
  (interactive)
  (if (> (length sutysisku--data) 0)
      (ivy-read
       "vlasisku: " 'sutysisku-ivy-candidates
       :dynamic-collection t
       :action 'sutysisku--search-ivy-kill-word-action)
    (sutysisku-fetch 'sutysisku-search-ivy)))

(ivy-set-actions
 'sutysisku-search-ivy
 '(("w" sutysisku--search-ivy-kill-word-action "Word")
   ("g" sutysisku--search-ivy-kill-gloss-action "Gloss")
   ("d" sutysisku--search-ivy-kill-definition-action "Definition")
   ("a" sutysisku--search-ivy-kill-all-action "All")))

(provide 'sutysisku)
;;; sutysisku.el ends here
