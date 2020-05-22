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

(defvar epurple--header-length (bindat-length header-spec '((command "") (id 0))))

(defvar epurple--handlers '(("ping" . epurple--ping-handler)))

(defvar epurple--queue nil)

(defvar epurple--id 0)

;;; Internal Functions

(defun epurple--ping-handler (id)
  (message "Ping -> %s" id))

(defun epurple--send (command payload payload-spec &optional cb)
  (cl-incf epurple--id)
  (let* ((buf-spec `((header struct epurple--header-spec)
		     (data   struct ,payload-spec)))
	 (header `(header . ((command . ,command)
			     (id      . ,epurple--id))))
	 (data `(data . ,payload))
	 (struct (list header data))
	 (buf (bindat-pack buf-spec struct)))
    (when cb
      (add-to-list 'epurple--queue (cons epurple--id cb) t))
    (epurple-server-send buf)))

;;; External Functions

(defvar epurple--ping-spec '((value     u32r)
			     (name      strz 256)
			     (new_value u32r)))

(defun epurple-ping-resp (payload)
  (let ((decoded (bindat-unpack epurple--ping-spec payload)))
    (let-alist decoded
      (message "%s -> %d:%d" .name .value .new_value))))

(defun epurple-ping ()
  (let ((payload '((value . 6)
		   (name . "yop")
		   (new_value . 22))))
    (epurple--send "ping" payload 'epurple--ping-spec
		   #'epurple-ping-resp)))

(defun epurple-commands-handler (str)
  (let* ((header (substring str 0 epurple--header-length))
	 (data (substring str epurple--header-length (length str)))
	 (decoded (bindat-unpack epurple--header-spec header)))
    (let-alist decoded
      (if-let ((resp (assoc-default .id epurple--queue)))
	  (funcall resp data)
	(if-let ((handler (assoc-default .command epurple--handlers)))
	    (funcall handler data)
	  (message "Unknown command: %s" .command))))))

(provide 'epurple-commands)
