;;; siraben-shell.el --- Configures Emacs Shell and Terminal modes.

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

;; Make some shell customizations.

;;; Code:

(if (file-exists-p "/usr/bin/zsh")
    (setq-default shell-file-name "/usr/bin/zsh"))

(add-hook 'shell-mode-hook
          #'(lambda ()
              (paredit-mode -1)))

(provide 'siraben-shell)
;;; siraben-shell.el ends here
