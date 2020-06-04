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

(defvar epurple--header-spec '((command strz 80)
			       (id      u32r)))

(defvar epurple--header-length (bindat-length epurple--header-spec
					      '((command "") (id 0))))

(defvar epurple--queue nil)

(defvar epurple--id 0)

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
				(protocol_id strz 80)))

(defun epurple-accounts-get-all-cb (cb payload)
  (let* ((account-length (bindat-length epurple--account-spec '((username "")
								(alias "")
								(protocol_id ""))))
	 (length (/ (length payload) account-length))
	 (spec `((accounts repeat ,length (struct epurple--account-spec))))
	 (decoded (bindat-unpack spec payload)))
    (funcall cb (assoc-default 'accounts decoded))))

(defun epurple-accounts-get-all (cb)
  (epurple--send "accounts_get_all" nil nil
		 (apply-partially #'epurple-accounts-get-all-cb cb)))

(defun epurple-account-connect (account)
  (with-struct-slots (username alias protocol_id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol_id . ,protocol_id))))
    (epurple--send "account_connect" payload 'epurple--account-spec))))

(defun epurple-account-disconnect (account)
  (with-struct-slots (username alias protocol_id) epurple-account account
    (let ((payload `((username    . ,username)
		     (alias       . ,alias)
		     (protocol_id . ,protocol_id))))
    (epurple--send "account_disconnect" payload 'epurple--account-spec))))

;; handler
(defun epurple-commands-handler (str)
  (let* ((header (substring str 0 epurple--header-length))
	 (data (substring str epurple--header-length (length str)))
	 (decoded (bindat-unpack epurple--header-spec header)))
    (let-alist decoded
      (if-let ((resp (assoc-default .id epurple--queue)))
	  (progn
	    (funcall resp data)
	    (setq epurple--queue (assq-delete-all .id epurple--queue)))
	(if-let ((handler (assoc-default .command epurple-handlers)))
	    (funcall handler data)
	  (message "Unknown command: %s" .command))))))

(provide 'epurple-commands)
