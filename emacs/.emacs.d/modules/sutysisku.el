;;; -*- lexical-binding: t -*-
;;; sutysisku.el --- Sutysisku for Helm
;; Copyright (C) 2018 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (request "0") (helm "0") (a "0")
;; Keywords: hydra
;; URL: http://github.com/dustinlacewell/hera

;;; Commentary:

;; This package offers Lojban dictionary search with Helm.

;;; Code:

(use-package a)
(use-package helm)
(use-package request)

(require 'cl)
(require 'a)
(require 'helm)
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

(provide 'sutysisku)
;;; sutysisku.el ends here
