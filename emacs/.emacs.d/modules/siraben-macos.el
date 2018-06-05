;;; siraben-macos.el --- This files runs when the host OS is macOS.

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

;; Currently only simple customizations are being made.

;;; Code:

(set-if-exists scheme-program-name "/usr/local/bin/guile")
(set-if-exists ispell-program-name "/usr/local/bin/aspell")
(setq system-uses-terminfo nil)

(provide 'siraben-macos)

;;; siraben-macos.el ends here
