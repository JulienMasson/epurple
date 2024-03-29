;;; epurple.el --- Instant messaging environment for Emacs

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson

;;; License

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

;;; Code:

;;; Struct

(cl-defstruct epurple-account
  name
  face
  username
  alias
  protocol-id
  active-p
  auto-reconnect
  prpl-buffers
  buddies
  chats)

(cl-defstruct epurple-buddy
  name
  display-name
  url
  icon
  status
  signed-on
  typing-p)

(cl-defstruct epurple-chat
  name
  url)

;; Libraries

(require 'epurple-buffer)
(require 'epurple-commands)
(require 'epurple-server)
(require 'epurple-utils)

;;; Groups

(defgroup epurple nil
  "epurple group"
  :group 'applications)

(defgroup epurple-faces nil
  "Faces used by epurple"
  :group 'epurple
  :group 'faces)

;;; Faces

(defface epurple-facebook-face
  '((((class color) (background light)) :foreground "DodgerBlue4" :weight bold)
    (((class color) (background  dark)) :foreground "DodgerBlue1" :weight bold))
  "Face for epurple facebook"
  :group 'epurple-faces)

(defface epurple-slack-face
  '((((class color) (background light)) :foreground "purple4" :weight bold)
    (((class color) (background  dark)) :foreground "purple2" :weight bold))
  "Face for epurple slack"
  :group 'epurple-faces)

(defface epurple-irc-face
  '((((class color) (background light)) :foreground "DarkSlateGrey" :weight bold)
    (((class color) (background  dark)) :foreground "DarkCyan" :weight bold))
  "Face for epurple irc"
  :group 'epurple-faces)

(defface epurple-whatsapp-face
  '((((class color) (background light)) :foreground "SeaGreen4" :weight bold)
    (((class color) (background  dark)) :foreground "SeaGreen3" :weight bold))
  "Face for epurple whatsapp"
  :group 'epurple-faces)

(defface epurple-nick-face
  '((((class color) (background light)) :foreground "SpringGreen4" :weight bold)
    (((class color) (background  dark)) :foreground "SpringGreen3" :weight bold))
  "Face for epurple nick msg"
  :group 'epurple-faces)

;;; Customization

(defcustom epurple-name-alias nil
  "An alist mapping a name to a purple username"
  :type 'alist
  :group 'epurple)

(defcustom epurple-nick-name nil
  "Nick name used to identified the user in epurple conversation"
  :type 'string
  :group 'epurple)

(defcustom epurple-auto-reconnect nil
  "Auto reconnect an account when this one has been disconnected"
  :type 'string
  :group 'epurple)

;;; External Variables

(defvar epurple-log-levels '("SILENT" "ERROR" "WARN" "INFO" "DEBUG"))

(defvar epurple-accounts nil)

;;; Internal Functions

(defun epurple--find-account (name)
  (cl-find-if (lambda (account)
		(string= (epurple-account-name account) name))
	      epurple-accounts))

(defun epurple--find-account-by-username (username)
  (cl-find-if (lambda (account)
		(string= (epurple-account-username account) username))
	      epurple-accounts))

(defun epurple--find-account-by-prpl-buffer (prpl-buffer)
  (cl-find-if (lambda (account)
		(member prpl-buffer (epurple-account-prpl-buffers account)))
	      epurple-accounts))

(defun epurple--find-prpl-buffer (buffer)
  (catch 'found
    (dolist (account epurple-accounts)
      (dolist (prpl-buffer (epurple-account-prpl-buffers account))
	(when (eq (epurple-buffer-buffer prpl-buffer) buffer)
	  (throw 'found prpl-buffer))))))

(defun epurple--find-buddy (account name)
  (cl-find-if (lambda (buddy)
		(string= (epurple-buddy-name buddy) name))
	      (epurple-account-buddies account)))

(defun epurple--find-chat (account name)
  (cl-find-if (lambda (chat)
		(string= (epurple-chat-name chat) name))
	      (epurple-account-chats account)))

(defun epurple--prompt (prompt accounts &optional all)
  (let ((collection (mapcar (lambda (account)
			      (with-struct-slots (name face)
				epurple-account account
				(propertize name 'face face)))
			    accounts)))
    (when (and all (> (length collection) 1))
      (push "ALL" collection))
    (completing-read prompt collection)))

(defun epurple--inactive ()
  (cl-remove-if (lambda (a) (epurple-account-active-p a)) epurple-accounts))

(defun epurple--prompt-inactive (prompt &optional all)
  (epurple--prompt prompt (epurple--inactive) all))

(defun epurple--active ()
  (cl-remove-if-not (lambda (a) (epurple-account-active-p a)) epurple-accounts))

(defun epurple--prompt-active (prompt &optional all)
  (epurple--prompt prompt (epurple--active) all))

(defun epurple--prompt-buffers (prompt &optional only-unreads)
  (let (collection)
    (dolist (account epurple-accounts)
      (with-struct-slots (face prpl-buffers) epurple-account account
	(dolist (prpl-buffer prpl-buffers)
	  (with-struct-slots (display-name buffer unread-p unread-count)
	    epurple-buffer prpl-buffer
	    (unless (and only-unreads (not unread-p))
	      (let ((str (if (zerop unread-count) display-name
			   (format "%s (%s)" display-name unread-count))))
		(add-to-list 'collection (cons (propertize str 'face face)
					       prpl-buffer))))))))
    (let* ((sorted-collection (cl-sort collection (lambda (mute-a mute-b)
						    (or (not mute-a) mute-b))
				       :key (lambda (e) (epurple-buffer-mute-p (cdr e)))))
	   (name (completing-read prompt (mapcar #'car sorted-collection))))
      (epurple-buffer-buffer (cdr (assq name sorted-collection))))))

(defun epurple--prompt-buddies (account prompt)
  (let (onlines offlines)
    (dolist (buddy (epurple-account-buddies account))
      (with-struct-slots (name display-name signed-on) epurple-buddy buddy
	(if signed-on
	    (push (cons (propertize display-name 'face 'success) name) onlines)
	  (push (cons (propertize display-name 'face 'error) name) offlines))))
    (let* ((collection (append onlines offlines))
	   (target (completing-read prompt (mapcar #'car collection))))
      (assoc target collection))))

(defun epurple--chats-info (account chats)
  (dolist (chat chats)
    (let ((chat-s (alist-to-struct chat 'epurple-chat)))
      (with-struct-slots (name) epurple-chat chat-s
	(setq name (decode-coding-string name 'utf-8)))
      (push chat-s (epurple-account-chats account)))))

(defun epurple--buddies-info (account buddies)
  (dolist (buddy buddies)
    (let ((buddy-s (alist-to-struct buddy 'epurple-buddy)))
      (with-struct-slots (name) epurple-buddy buddy-s
	(setq name (decode-coding-string name 'utf-8)))
      (push buddy-s (epurple-account-buddies account))))
  (epurple-chats-get-all account #'epurple--chats-info))

(defun epurple--accounts-info (accounts)
  (dolist (account accounts)
    (let ((account (alist-to-struct account 'epurple-account)))
      (with-struct-slots (name face username protocol-id auto-reconnect) epurple-account account
	(let ((alias (assoc-default username epurple-name-alias))
	      (a-face (cond ((string= protocol-id "prpl-facebook") 'epurple-facebook-face)
			    ((string= protocol-id "prpl-slack") 'epurple-slack-face)
			    ((string= protocol-id "prpl-irc") 'epurple-irc-face)
			    ((string= protocol-id "prpl-hehoe-gowhatsapp") 'epurple-whatsapp-face)
			    (t 'default))))
	  (setq name (if alias alias username))
	  (setq face a-face)
	  (setq auto-reconnect epurple-auto-reconnect)
	  (push account epurple-accounts))))))

(defun epurple--mark-buffer-as-read (buffer)
  (when-let ((prpl-buffer (epurple--find-prpl-buffer buffer)))
    (with-struct-slots (mention-p unread-p unread-count) epurple-buffer prpl-buffer
      (setq mention-p nil)
      (setq unread-p nil)
      (setq unread-count 0))))

(defun epurple--select-window (old-fn &rest args)
  (let ((prev-buffer (current-buffer))
	next-buffer)
    (apply old-fn args)
    (setq next-buffer (current-buffer))
    ;; mark as read
    (with-current-buffer next-buffer
      (when (derived-mode-p 'lui-mode)
	(epurple--mark-buffer-as-read next-buffer)))
    ;; remove unread separator
    (with-current-buffer prev-buffer
      (when (and (derived-mode-p 'lui-mode)
		 (not (eq prev-buffer next-buffer)))
	(epurple-buffer-remove-unread-separator prev-buffer)))))

;;; External Functions

(defun epurple-chat (name)
  (interactive (list (epurple--prompt-active "Chat: ")))
  (let* ((account (epurple--find-account name))
	 (prompt (with-struct-slots (name face) epurple-account account
		   (format "Chat (%s): " (propertize name 'face face))))
	 (chat (completing-read prompt (s-mapcar (epurple-account-chats account) 'name))))
    (epurple-find-conv account 2 chat chat #'epurple-buffer-conv)))

(defun epurple-im (name)
  (interactive (list (epurple--prompt-active "IM: ")))
  (let* ((account (epurple--find-account name))
	 (prompt (with-struct-slots (name face) epurple-account account
		   (format "IM (%s): " (propertize name 'face face)))))
    (pcase-let ((`(,display-name . ,name) (epurple--prompt-buddies account prompt)))
      (if-let ((prpl-buffer (epurple-buffer--find account 1 display-name)))
	  (pop-to-buffer (epurple-buffer-buffer prpl-buffer))
	(epurple-find-conv account 1 name display-name #'epurple-buffer-conv)))))

(defun epurple-mute-toggle (buffer)
  (interactive (list (epurple--prompt-buffers "Toggle Mute: ")))
  (when-let ((prpl-buffer (epurple--find-prpl-buffer buffer)))
    (with-struct-slots (mute-p) epurple-buffer prpl-buffer
      (setq mute-p (not mute-p)))))

(defun epurple-jump (buffer)
  (interactive (list (epurple--prompt-buffers "Jump: ")))
  (epurple-buffer-display buffer))

(defun epurple-unread (buffer)
  (interactive (list (epurple--prompt-buffers "Unread: " t)))
  (epurple-buffer-display buffer))

(defun epurple-connect (name)
  (interactive (list (epurple--prompt-inactive "Connect: " t)))
  (let ((inactives (s-mapcar (epurple--inactive) 'name)))
    (dolist (name (if (string= name "ALL") inactives (list name)))
      (let ((account (epurple--find-account name)))
	(epurple-account-connect account)))))

(defun epurple-disconnect (name)
  (interactive (list (epurple--prompt-active "Disconnect: " t)))
  (let ((actives (s-mapcar (epurple--active) 'name)))
    (dolist (name (if (string= name "ALL") actives (list name)))
      (let ((account (epurple--find-account name)))
	(epurple-account-disconnect account)))))

(defun epurple-update-restart ()
  (interactive)
  (epurple-exit t)
  (epurple-server-update-init))

(defun epurple-restart ()
  (interactive)
  (epurple-exit t)
  (epurple-init))

(defun epurple-exit (confirm)
  (interactive (list (yes-or-no-p "Do you really want to exit ?")))
  (advice-remove 'select-window #'epurple--select-window)
  (when confirm
    (epurple-server-exit t)
    (dolist (account epurple-accounts)
      (dolist (prpl-buffer (epurple-account-prpl-buffers account))
	(kill-buffer (epurple-buffer-buffer prpl-buffer))))
    (setq epurple-accounts nil)))

(defun epurple-init ()
  (interactive)
  (advice-add 'select-window :around #'epurple--select-window)
  (epurple-server-init))

(defun epurple-set-log-level (log-level)
  (interactive (list (completing-read "Set log level: " epurple-log-levels)))
  (epurple-update-log-level (cl-position log-level epurple-log-levels)))

(provide 'epurple)
