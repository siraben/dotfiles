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
 '(package-selected-packages
   (quote
    (smooth-scrolling exec-path-from-shell neotree memory-usage pdf-tools magit perspective undo-tree flycheck writeroom-mode auto-complete rainbow-delimiters solarized-theme paredit markdown-mode csv-mode aggressive-indent 2048-game)))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; list the packages you want
(setq package-list package-selected-packages)

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Spell Checking
(setq ispell-dictionary "en_US")

;; OS Specific Settings
(cond ((eq system-type 'darwin)
       (progn (setq scheme-program-name "/usr/local/bin/chez")
	      (setq ispell-program-name "/usr/local/bin/aspell")
	      (setq ben/default-font-size 13)))
      
      ((eq system-type 'gnu/linux)
       (progn (setq scheme-program-name "/usr/bin/mit-scheme")
	      (setq ispell-program-name "/usr/bin/aspell")
	      (setq ben/default-font-size 11))))


(defun ben/apply-solarized-theme ()
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

(defun ben/my-lisp-hook ()
  (paredit-mode 1)
  (aggressive-indent-mode 1)
  (rainbow-delimiters-mode 1)
  (undo-tree-mode 1)
  (auto-complete-mode 1)
  (show-paren-mode 1))

(add-hook 'scheme-mode-hook #'(lambda () (ben/my-lisp-hook)))

(add-hook 'markdown-mode-hook #'(lambda ()
				  (progn (visual-line-mode 1)
					 (undo-tree-mode 1)
					 (writeroom-mode 1))))

(add-hook 'org-mode-hook #'(lambda ()
			     (progn (visual-line-mode 1)
				    (undo-tree-mode 1))))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (ben/my-lisp-hook)))

(add-hook 'after-init (smooth-scrolling-mode 1))
