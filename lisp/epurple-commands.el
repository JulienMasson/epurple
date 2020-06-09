;;; epurple-commands.el --- Commands handler for epurple

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

(require 'bindat)
(require 'epurple-buffer)

;;; Internal Variables

(defvar epurple--header-spec '((command strz 80)
			       (id      u32r)))
(defvar epurple--header-length (bindat-length epurple--header-spec
					      '((command "") (id 0))))

(defvar epurple--queue nil)
(defvar epurple--id 1)

(defvar epurple--handlers
  '(("account_connected"    . epurple-account-connected)
    ("account_disconnected" . epurple-account-disconnected)
    ("new_msg"              . epurple-new-msg)))

;;; Internal Functions

(defun epurple--send (command payload payload-spec &optional cb)
  (cl-incf epurple--id)
  (let* ((buf-spec (if payload
		       `((header struct epurple--header-spec)
			 (data   struct ,payload-spec))
		     '((header struct epurple--header-spec))))
	 (header `(header . ((command . ,command)
			     (id      . ,epurple--id))))
	 (data `(data . ,payload))
	 (struct (if payload (list header data) (list header)))
	 (buf (bindat-pack buf-spec struct)))
    (when cb
      (add-to-list 'epurple--queue (cons epurple--id cb) t))
    (epurple-server-send buf)))

;;; External Functions

;; purple
(defun epurple-purple-init (cb)
  (epurple--send "purple_init" nil nil `(lambda (p) (funcall #',cb))))

;; accounts
(defvar epurple--account-spec '((username    strz 80)
				(alias       strz 80)
				(protocol-id strz 80)))

(defun epurple-accounts-get-all-cb (cb payload)
  (let* ((account-length (bindat-length epurple--account-spec '((username "")
								(alias "")
								(protocol-id ""))))
	 (length (/ (length payload) account-length))
	 (spec `((accounts repeat ,length (struct epurple--account-spec))))
	 (decoded (bindat-unpack spec payload)))
    (funcall cb (assoc-default 'accounts decoded))))

(defun epurple-accounts-get-all (cb)
  (epurple--send "accounts_get_all" nil nil
		 (apply-partially #'epurple-accounts-get-all-cb cb)))

(defun epurple-account-connect (account)
  (with-struct-slots (username alias protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol-id . ,protocol-id))))
      (epurple--send "account_connect" payload 'epurple--account-spec))))

(defun epurple-account-connected (payload)
  (let* ((spec '((username strz 80)))
	 (decoded (bindat-unpack spec payload))
	 (username (assoc-default 'username decoded)))
    (when-let ((account (epurple--find-account-by-username username)))
      (setf (epurple-account-active-p account) t)
      (epurple-buddies-get-all account (apply-partially #'epurple--buddies-info
							account)))))

(defun epurple-account-disconnect (account)
  (with-struct-slots (username alias protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol-id . ,protocol-id))))
      (epurple--send "account_disconnect" payload 'epurple--account-spec))))

(defun epurple-account-disconnected (payload)
  (let* ((spec '((username strz 80)))
	 (decoded (bindat-unpack spec payload))
	 (username (assoc-default 'username decoded)))
    (when-let ((account (epurple--find-account-by-username username)))
      (with-struct-slots (active-p buddies) epurple-account account
	(setq active-p nil)
	(setq buddies nil)))))

;; buddies
(defvar epurple--buddy-spec '((name         strz 80)
			      (alias        strz 80)
			      (server-alias strz 80)
			      (icon         strz 80)))

(defun epurple-buddies-get-all-cb (cb payload)
  (let* ((buddy-length (bindat-length epurple--buddy-spec '((name "")
							    (alias "")
							    (server-alias "")
						    	    (icon ""))))
	 (length (/ (length payload) buddy-length))
	 (spec `((buddys repeat ,length (struct epurple--buddy-spec))))
	 (decoded (bindat-unpack spec payload)))
    (funcall cb (assoc-default 'buddys decoded))))

(defun epurple-buddies-get-all (account cb)
  (with-struct-slots (username alias protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol-id . ,protocol-id))))
      (epurple--send "buddies_get_all" payload 'epurple--account-spec
		     (apply-partially #'epurple-buddies-get-all-cb cb)))))

;; chats
(defvar epurple--chat-spec '((name strz 80)))

(defun epurple-chats-get-all-cb (cb payload)
  (let* ((chat-length (bindat-length epurple--chat-spec '((name ""))))
	 (length (/ (length payload) chat-length))
	 (spec `((chats repeat ,length (struct epurple--chat-spec))))
	 (decoded (bindat-unpack spec payload)))
    (funcall cb (assoc-default 'chats decoded))))

(defun epurple-chats-get-all (account cb)
  (with-struct-slots (username alias protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol-id . ,protocol-id))))
      (epurple--send "chats_get_all" payload 'epurple--account-spec
		     (apply-partially #'epurple-chats-get-all-cb cb)))))

;; conv
(defvar epurple--conv-spec '((username    strz 80)
			     (protocol-id strz 80)
			     (conv-type   u32r)
			     (conv-name   strz 80)))

(defun epurple-create-conv (account conv-type conv-name cb)
  (with-struct-slots (username protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (protocol-id . ,protocol-id)
		     (conv-type   . ,conv-type)
		     (conv-name   . ,conv-name))))
      (epurple--send "create_conv" payload 'epurple--conv-spec
		      `(lambda (p) (funcall #',cb ,account ,conv-type ,conv-name))))))

;; msg
(defvar epurple--send-msg-spec '((username    strz 80)
				 (protocol-id strz 80)
				 (conv-type   u32r)
				 (conv-name   strz 80)
				 (msg         strz 512)))

(defun epurple-send-msg (account prpl-buffer msg)
  (with-struct-slots (username protocol-id) epurple-account account
    (with-struct-slots (conv-type conv-name) epurple-buffer prpl-buffer
      (let ((payload `((username    . ,username)
		       (protocol-id . ,protocol-id)
		       (conv-type   . ,conv-type)
		       (conv-name   . ,conv-name)
		       (msg         . ,msg))))
	(epurple--send "send_msg" payload 'epurple--send-msg-spec)))))

(defun epurple-new-msg (payload)
  (let* ((spec '((username   strz 80)
		 (conv-type  u32r)
		 (conv-name  strz 80)
		 (sender     strz 80)
		 (msg        strz 512)
		 (flags      u32r)
		 (time       u32r)))
	 (decoded (bindat-unpack spec payload)))
    (epurple-buffer-new-msg decoded)))

;; handler
(defun epurple-commands-handler (str)
  (let* ((header (substring str 0 epurple--header-length))
	 (payload (substring str epurple--header-length (length str)))
	 (decoded (bindat-unpack epurple--header-spec header)))
    (let-alist decoded
      (if-let ((resp (assoc-default .id epurple--queue)))
	  (progn
	    (funcall resp payload)
	    (setq epurple--queue (assq-delete-all .id epurple--queue)))
	(if-let ((handler (assoc-default .command epurple--handlers)))
	    (funcall handler payload)
	  (message "Unknown command: %s" .command))))))

(provide 'epurple-commands)
