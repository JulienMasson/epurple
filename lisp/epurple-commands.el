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

;;; Internal Variables

(defvar epurple--header-spec '((command      strz 80)
			       (id           u32r)
			       (payload-size u32r)))

(defvar epurple--header-length (bindat-length epurple--header-spec
					      '((command "")
						(id 0)
						(payload-size 0))))

(defvar epurple--payload-spec nil)
(defvar epurple--queue nil)
(defvar epurple--id 1)

(defvar epurple--handlers
  '(("account_connected"    . epurple-account-connected)
    ("account_disconnected" . epurple-account-disconnected)
    ("buddy_typing_update"  . epurple-buddy-typing-update)
    ("buddy_update"         . epurple-buddy-update)
    ("new_msg"              . epurple-new-msg)
    ("purple_init_done"     . epurple-purple-init-done)))

;;; Internal Functions

(defun epurple--send (command payload payload-spec &optional cb)
  (cl-incf epurple--id)
  (setq epurple--payload-spec payload-spec)
  (let* ((buf-spec (if payload
		       `((header struct epurple--header-spec)
			 (data   struct epurple--payload-spec))
		     '((header struct epurple--header-spec))))
	 (payload-size (if payload (bindat-length payload-spec payload) 0))
	 (header `(header . ((command      . ,command)
			     (id           . ,epurple--id)
			     (payload-size . ,payload-size))))
	 (data `(data . ,payload))
	 (struct (if payload (list header data) (list header)))
	 (buf (bindat-pack buf-spec struct)))
    (when cb
      (add-to-list 'epurple--queue (cons epurple--id cb) t))
    (epurple-server-send buf)))

;;; External Functions

;; purple
(defun epurple-purple-init ()
  (epurple--send "purple_init" nil nil))

(defun epurple-purple-init-done (payload)
  (epurple-accounts-get-all #'epurple--accounts-info))

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
      (epurple--send "account_connect" payload epurple--account-spec))))

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
      (epurple--send "account_disconnect" payload epurple--account-spec))))

(defun epurple-account-disconnected (payload)
  (let* ((spec '((username strz 80)))
	 (decoded (bindat-unpack spec payload))
	 (username (assoc-default 'username decoded)))
    (when-let ((account (epurple--find-account-by-username username)))
      (with-struct-slots (active-p auto-reconnect buddies chats) epurple-account account
	(setq active-p nil)
	(setq buddies nil)
	(setq chats nil)
	(when auto-reconnect
	  (epurple-account-connect account))))))

;; buddies
(defvar epurple--buddy-spec '((name         strz 80)
			      (alias        strz 80)
			      (server-alias strz 80)))

(defun epurple-buddies-get-all-cb (cb payload)
  (let* ((buddy-length (bindat-length epurple--buddy-spec '((name "")
							    (alias "")
							    (server-alias ""))))
	 (length (/ (length payload) buddy-length))
	 (spec `((buddys repeat ,length (struct epurple--buddy-spec))))
	 (decoded (bindat-unpack spec payload)))
    (funcall cb (assoc-default 'buddys decoded))))

(defun epurple-buddies-get-all (account cb)
  (with-struct-slots (username alias protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol-id . ,protocol-id))))
      (epurple--send "buddies_get_all" payload epurple--account-spec
		     (apply-partially #'epurple-buddies-get-all-cb cb)))))

(defun epurple-buddy-typing-update (payload)
  (let* ((spec '((account-username strz 80)
		 (buddy-name       strz 80)
		 (conv-name        strz 80)
		 (typing           u32r)))
	 (decoded (bindat-unpack spec payload)))
    (let-alist decoded
      (when-let* ((account (epurple--find-account-by-username .account-username))
		  (buddy-name (decode-coding-string .buddy-name 'utf-8))
		  (buddy (epurple--find-buddy account buddy-name)))
	(setf (epurple-buddy-typing-p buddy) (not (zerop .typing)))
	(epurple-buffer-update account .conv-name)))))

(defun epurple-buddy-update (payload)
  (let* ((spec '((account-username strz 80)
		 (buddy-name       strz 80)
		 (icon             strz 80)
		 (online           u32r)))
	 (decoded (bindat-unpack spec payload)))
    (let-alist decoded
      (when-let* ((account (epurple--find-account-by-username .account-username))
		  (buddy-name (decode-coding-string .buddy-name 'utf-8))
		  (buddy (epurple--find-buddy account buddy-name)))
	(with-struct-slots (icon signed-on) epurple-buddy buddy
	  (setq icon .icon)
	  (setq signed-on (not (zerop .online)))
	  (epurple-buffer-update account .buddy-name))))))

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
      (epurple--send "chats_get_all" payload epurple--account-spec
		     (apply-partially #'epurple-chats-get-all-cb cb)))))

;; conv
(defvar epurple--conv-spec '((username    strz 80)
			     (protocol-id strz 80)
			     (conv-type   u32r)
			     (conv-name   strz 80)))

(defun epurple-find-conv-cb (cb account conv-type conv-name display-name payload)
  (let* ((spec '((conv-url strz 80)))
	 (decoded (bindat-unpack spec payload)))
    (let-alist decoded
      (funcall cb account conv-type conv-name display-name .conv-url))))

(defun epurple-find-conv (account conv-type conv-name display-name cb)
  (with-struct-slots (username protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (protocol-id . ,protocol-id)
		     (conv-type   . ,conv-type)
		     (conv-name   . ,conv-name))))
      (epurple--send "find_conv" payload epurple--conv-spec
		     (apply-partially #'epurple-find-conv-cb
				      cb account conv-type conv-name display-name)))))

(defun epurple-update-conv (account conv-type conv-name)
  (with-struct-slots (username protocol-id) epurple-account account
    (let ((payload `((username    . ,username)
		     (protocol-id . ,protocol-id)
		     (conv-type   . ,conv-type)
		     (conv-name   . ,conv-name))))
      (epurple--send "update_conv" payload epurple--conv-spec))))

;; msg
(defvar epurple--send-msg-spec nil)

(defun epurple-send-msg (account prpl-buffer msg)
  (with-struct-slots (username protocol-id) epurple-account account
    (with-struct-slots (conv-type conv-name) epurple-buffer prpl-buffer
      (let* ((name (encode-coding-string conv-name 'utf-8))
	     (msg (encode-coding-string msg 'utf-8))
	     (payload `((username    . ,username)
			(protocol-id . ,protocol-id)
			(conv-type   . ,conv-type)
			(conv-name   . ,name)
			(msg         . ,msg))))
	(setq epurple--send-msg-spec `((username    strz 80)
				       (protocol-id strz 80)
				       (conv-type   u32r)
				       (conv-name   strz 80)
				       (msg         strz ,(length msg))))
	(epurple--send "send_msg" payload epurple--send-msg-spec)))))

(defun epurple-new-msg (payload)
  (let* ((spec '((username   strz 80)
		 (conv-type  u32r)
		 (conv-name  strz 80)
		 (conv-url   strz 80)
		 (sender     strz 80)
		 (flags      u32r)
		 (time       u32r)
		 (msg-size   u32r)))
	 (msg-data-length (bindat-length spec '((username "")
						(conv-type 0)
						(conv-name "")
						(conv-url "")
						(sender "")
						(flags 0)
						(time 0)
						(msg-size 0))))
	 (payload-msg-data (substring payload 0 msg-data-length))
	 (decoded (bindat-unpack spec payload-msg-data)))
    (let-alist decoded
      (epurple-buffer-new-msg decoded(substring payload msg-data-length
						(+ msg-data-length .msg-size))))))

;; handler
(defun epurple-commands-handler (data)
  (catch 'remaining-data
    (while data
      (let* ((header (substring data 0 epurple--header-length))
	     (decoded (bindat-unpack epurple--header-spec header)))
	(let-alist decoded
	  ;; data size should be greater than header + payload size
	  ;; otherwise that means we have not received yet all the data
	  (if (< (length data) (+ epurple--header-length .payload-size))
	      (throw 'remaining-data data)
	    (let ((payload (substring data epurple--header-length
				      (+ epurple--header-length .payload-size)))
		  (data-size (+ epurple--header-length .payload-size)))
	      ;; check in callbacks queue
	      (if-let ((resp (assoc-default .id epurple--queue)))
		  (progn
		    (funcall resp payload)
		    (setq epurple--queue (assq-delete-all .id epurple--queue)))
		;; check in handlers
		(if-let ((handler (assoc-default .command epurple--handlers)))
		    (funcall handler payload)
		  ;; data can not be handled
		  (setq data nil)
		  (message "epurple: data not handled")
		  (throw 'remaining-data data)))
	      ;; remove data handled
	      (if (= data-size (length data))
		  (setq data nil)
		(setq data (substring data data-size (length data))))))))))
  data)

(provide 'epurple-commands)
