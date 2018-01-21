(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; activate all the packages (in particular autoloads)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(display-battery-mode t)
 '(display-time-mode t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(htmlize gnuplot gnuplot-mode emms guide-key free-keys smooth-scrolling exec-path-from-shell neotree memory-usage pdf-tools magit perspective undo-tree flycheck writeroom-mode auto-complete rainbow-delimiters paredit markdown-mode csv-mode aggressive-indent 2048-game))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; List desired packages
(setq package-list package-selected-packages)

(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Spell checking langauge
(setq ispell-dictionary "en_US")


;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (calc . t)
   (python . t)
   (scheme . t)))
;; Add additional languages with '((language . t)))

;; OS Specific Settings
(cond ((eq system-type 'darwin)
       (progn (setq scheme-program-name "/usr/local/bin/chez")
	      (setq ispell-program-name "/usr/local/bin/aspell")
	      (setq ben/default-font-size 13)
	      (setq system-uses-terminfo nil)
	      (setq gnuplot-program "/usr/local/bin/gnuplot")
	      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2017/bin/x86_64-darwin"))
	      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
	      (add-to-list 'exec-path "/usr/local/bin")))
      
      ((eq system-type 'gnu/linux)
       (progn (setq scheme-program-name "/usr/bin/mit-scheme")
              (setq ispell-program-name "/usr/bin/aspell")
              (setq ben/default-font-size 11))))

;; In case the editor doesn't have a box style cursor.
(setq-default cursor-type 'box)

(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-standard)
(emms-default-players)
(define-emms-simple-player mplayer '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
		".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
		".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
  "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

;;** EMMS
;; Autoload the id3-browser and bind it to F7.
;; You can change this to your favorite EMMS interface.
(autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
(global-set-key [(f7)] 'emms-smart-browse)

(with-eval-after-load 'emms
  (emms-standard) ;; or (emms-devel) if you want all features
  (setq emms-source-file-default-directory "~/Music"
        emms-info-asynchronously t
        emms-show-format "♪ %s")

  ;; Might want to check `emms-info-functions',
  ;; `emms-info-libtag-program-name',
  ;; `emms-source-file-directory-tree-function'
  ;; as well.

  ;; Determine which player to use.
  ;; If you don't have strong preferences or don't have
  ;; exotic files from the past (wma) `emms-default-players`
  ;; is probably all you need.
  (if (executable-find "mplayer")
      (setq emms-player-list '(emms-player-mplayer))
    (emms-default-players))

  ;; For libre.fm see `emms-librefm-scrobbler-username' and
  ;; `emms-librefm-scrobbler-password'.
  ;; Future versions will use .authoinfo.gpg.
  )



(defun ben/apply-solarized-theme ()
  "Apply the dark solarized theme and don't allow the use of non-fix width fonts."
  (interactive)
  (setq solarized-use-variable-pitch nil)
  ;; (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(ben/apply-solarized-theme)


(setq ben/default-font "Hack")

(setq ben/current-font-size ben/default-font-size)

(setq ben/font-change-increment 1.1)

(defun ben/font-code ()
  "Return a string representing the current font (like \"Hack-13\")."
  (concat ben/default-font "-" (number-to-string ben/current-font-size)))

(defun ben/set-font-size ()
  "Set the font to `ben/default-font' at `ben/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (ben/font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun ben/reset-font-size ()
  "Change font size back to `ben/default-font-size'."
  (interactive)
  (setq ben/current-font-size ben/default-font-size)
  (ben/set-font-size))

(defun ben/increase-font-size ()
  "Increase current font size by a factor of `ben/font-change-increment'."
  (interactive)
  (setq ben/current-font-size
        (ceiling (* ben/current-font-size ben/font-change-increment)))
  (ben/set-font-size))

(defun ben/decrease-font-size ()
  "Decrease current font size by a factor of `ben/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq ben/current-font-size
        (max 1
             (floor (/ ben/current-font-size ben/font-change-increment))))
  (ben/set-font-size))

(define-key global-map (kbd "C-)") 'ben/reset-font-size)
(define-key global-map (kbd "C-+") 'ben/increase-font-size)
(define-key global-map (kbd "C-=") 'ben/reset-font-size)
(define-key global-map (kbd "C--") 'ben/decrease-font-size)

(ben/reset-font-size)

(defun ben/my-lisp-mode ()
  "Activates modes that greatly help with editing Lisp code."
  (interactive)
  (paredit-mode 1)
  (aggressive-indent-mode 1)
  (rainbow-delimiters-mode 1)
  (undo-tree-mode 1)
  (auto-complete-mode 1)
  (show-paren-mode 1))

(add-hook 'scheme-mode-hook #'(lambda () (ben/my-lisp-mode)))

(add-hook 'markdown-mode-hook #'(lambda ()
				  (progn (visual-line-mode 1)
					 (undo-tree-mode 1)
					 (writeroom-mode 1))))

(add-hook 'org-mode-hook #'(lambda ()
			     (progn (visual-line-mode 1)
				    (undo-tree-mode 1)
				    (flyspell-mode 1))))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (ben/my-lisp-mode)))

(add-hook 'after-init (smooth-scrolling-mode 1))


