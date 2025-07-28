;;; init.el --- Entry point for siraben's Emacs

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

;; Welcome to siraben's Emacs init file!
;; This is the first to be executed by Emacs.

;;; Code:

;; Store original values for restoration
(defvar siraben--file-name-handler-alist file-name-handler-alist)
(defvar siraben--gc-cons-threshold (* 500 1024 1024))  ; 500MB instead of default
(defvar siraben--gc-cons-percentage 0.1)

;; Maximize garbage collection threshold and disable file handlers during startup
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

;; Restore settings after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist siraben--file-name-handler-alist
                  gc-cons-threshold siraben--gc-cons-threshold
                  gc-cons-percentage siraben--gc-cons-percentage)
            (garbage-collect)) t)

;; Suppress warnings during startup
(setq byte-compile-warnings nil
      native-comp-async-report-warnings-errors nil
      warning-suppress-log-types '((comp) (bytecomp)))

;; Improve subprocess performance
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Remove UI elements early to prevent flashing
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Configure frame settings
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (scroll-bar-mode -1)
              (setq-default cursor-type 'box))))

;; Define configuration directories
(defvar siraben-root-dir user-emacs-directory
  "The root directory of the Emacs configuration.")

(defvar siraben-modules-dir
  (expand-file-name "modules/" siraben-root-dir)
  "The directory that contains all the modules for my configuration.")

;; Add modules to load path
(add-to-list 'load-path siraben-modules-dir)

;; Set custom file location (but don't load it automatically)
(setq custom-file (expand-file-name "custom.el" siraben-root-dir))

;; Load configuration modules
(require 'siraben-core)
(require 'siraben-packages)
(require 'siraben-ui)
(require 'siraben-fonts)
(require 'siraben-keybindings)
(require 'siraben-editor)
(require 'siraben-programming)
(require 'siraben-shell)
(require 'siraben-org)

;; Load OS-specific configuration
(pcase system-type
  ('gnu/linux (require 'siraben-linux))
  ('darwin (require 'siraben-macos)))

;; Set higher GC threshold for runtime performance
(setq gc-cons-threshold (* 500 1024 1024)  ; 500MB
      gc-cons-percentage 0.1)

;; Configure startup behavior
(setq inhibit-startup-message t
      initial-scratch-message nil
      default-directory "~/")

;;; init.el ends here
