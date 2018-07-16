;;; siraben-mu4e.el --- configures mu4e in Emacs

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

;;; Code:

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; default
(setq mu4e-maildir "~/Maildir")

;; (setq mu4e-drafts-folder "/Personal/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/Personal/[Gmail].Sent Mail")
;; (setq mu4e-trash-folder  "/Personal/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.


;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about me.
(setq
 user-mail-address "bensiraphob@@gmail.com"
 user-full-name  "Siraphob (Ben) Phipathananunth"
 mu4e-compose-signature
 (concat
  "Siraphob (Ben) Phipathananunth\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "bensiraphob@@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      
      ;; Don't keep message buffers around.
      message-kill-buffer-on-exit t
      ;; Save attachments into the Downloads directory.
      mu4e-attachment-dir "~/Downloads"
      ;; Show images whenever possible.
      mu4e-view-show-images t
      ;; Show full addresses of contacts.
      mu4e-view-show-addresses t)

(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (flyspell-mode t)))
(setq mu4e-maildir-shortcuts 
      '(("/School/INBOX"               . ?i)
        ("/School/[Gmail].Sent Mail"   . ?s)
        ("/School/[Gmail].Trash"       . ?t)
        ("/School/[Gmail].All Mail"    . ?a)))
(setq mu4e-contexts
      `( ,(make-mu4e-context
	   :name "School"
	   :enter-func (lambda () (mu4e-message "Entering School context")
                         (setq mu4e-maildir-shortcuts 
                               '(("/School/INBOX"               . ?i)
                                 ("/School/[Gmail].Sent Mail"   . ?s)
                                 ("/School/[Gmail].Trash"       . ?t)
                                 ("/School/[Gmail].All Mail"    . ?a))))
           :leave-func (lambda () (mu4e-message "Leaving School context"))

	   ;; :match-func (lambda (msg)
           ;;               (when msg
           ;;                 (string-match-p "^/School" (mu4e-message-field msg :maildir))))
	   :vars '((user-mail-address	   . "17282@students.isb.ac.th")
		   (user-full-name	   . "Siraphob (Ben) Phipathananunth")
		   (mu4e-compose-signature . "Siraphob (Ben) Phipathananunth\n")
                   (mu4e-drafts-folder     . "/School/[Gmail].Drafts")
                   (mu4e-sent-folder       . "/School/[Gmail].Sent Mail")
                   (mu4e-trash-folder      . "/School/[Gmail].Trash")))
         ,(make-mu4e-context
	   :name "Personal"
	   :enter-func (lambda () (mu4e-message "Switch to the Personal context")
                         (setq mu4e-maildir-shortcuts 
                               '(("/Personal/INBOX"               . ?i)
                                 ("/Personal/[Gmail].Sent Mail"   . ?s)
                                 ("/Personal/[Gmail].Trash"       . ?t)
                                 ("/Personal/[Gmail].All Mail"    . ?a))))
           :leave-func (lambda () (mu4e-message "Leaving Personal context"))
           ;; :match-func (lambda (msg)
           ;;               (when msg
           ;;                 (string-match-p "^/Personal" (mu4e-message-field msg :maildir))))
	   :vars '((user-mail-address	   . "bensiraphob@gmail.com")
		   (user-full-name	   . "Siraphob (Ben) Phipathananunth")
		   (mu4e-compose-signature . "Siraphob (Ben) Phipathananunth\n")
                   (mu4e-drafts-folder     . "/Personal/[Gmail].Drafts")
                   (mu4e-sent-folder       . "/Personal/[Gmail].Sent Mail")
                   (mu4e-trash-folder      . "/Personal/[Gmail].Trash")))))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context; 
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask 
;; (setq mu4e-compose-context-policy nil)


(provide 'siraben-mu4e)

;;; siraben-mu4e.el ends here
