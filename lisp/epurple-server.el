;;; epurple-server.el --- Instant messaging environment for Emacs

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

(require 'epurple-commands)

;;; Struct

(cl-defstruct epurple-server
  (process nil)
  (socket  nil))

;;; Internal Variables

(defconst epurple-server--src-dir (expand-file-name
				   (concat (file-name-directory load-file-name)
					   "../server/")))

(defconst epurple-server--program (concat epurple-server--src-dir "build/epurple"))

(defvar epurple-server (make-epurple-server))

;;; Internal Functions

(defun epurple-server--sentinel (proc event)
  (message "%s exited with status: %s" proc (process-exit-status proc))
  (epurple-server-exit))

(defun epurple-server--filter (proc str)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert str)))

;; compilation

(defun epurple-server--sentinel-compilation (process event)
  (if (zerop (process-exit-status process))
      (progn
	(kill-buffer (process-buffer process))
	(epurple-server--start))
    (message "epurple: compilation %s" (propertize "failed" 'face 'error))
    (switch-to-buffer-other-window (process-buffer process))))

(defun epurple-server--sentinel-configuration (process event)
  (if (eq (process-exit-status process) 0)
      (with-current-buffer (process-buffer process)
	(let ((process (start-process "epurple-server-compilation"
				      (current-buffer) "ninja" "-C" "build")))
	  (set-process-filter process 'epurple-server--filter)
	  (set-process-sentinel process 'epurple-server--sentinel-compilation)))
    (message "epurple: configuration %s" (propertize "failed" 'face 'error))
    (switch-to-buffer-other-window (process-buffer process))))

(defun epurple-server--configure-compile ()
  (let* ((default-directory epurple-server--src-dir)
	 (buffer (get-buffer-create "*epurple-server-compilation*"))
	 (process (start-process "epurple-server-configuration"
				 buffer "meson" "build")))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'epurple-server--filter)
    (set-process-sentinel process 'epurple-server--sentinel-configuration)))

;; socket

(defun epurple-server--filter-socket (proc str)
  (epurple-commands-handler str))

(defun epurple-server--socket-start ()
  (let* ((default-directory epurple-server--src-dir)
	 (server-file (concat default-directory ".epurple.sock"))
	 (process (make-network-process
		   :name     "epurple-socket"
		   :family   'local
		   :service  server-file
		   :noquery  t
		   :filter   #'epurple-server--filter-socket
		   :sentinel #'epurple-server--sentinel)))
    (set-process-coding-system process 'binary 'binary)
    (setf (epurple-server-socket epurple-server) process)))

;; server

(defun epurple-server--filter-process (proc str)
  (when (and (not (epurple-server-socket epurple-server))
	     (string= "Waiting Emacs\n" str))
    (epurple-server--socket-start))
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert str)))

(defun epurple-server--start ()
  (let* ((default-directory epurple-server--src-dir)
	 (buffer (get-buffer-create "*epurple-server*"))
	 (process (start-process "epurple-server" buffer
				 epurple-server--program)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'epurple-server--filter-process)
    (set-process-sentinel process 'epurple-server--sentinel)
    (setf (epurple-server-process epurple-server) process)))

;;; External Functions

(defun epurple-server-recompile ()
  (interactive)
  (epurple-server-exit)
  (let ((default-directory epurple-server--src-dir))
    (when (file-exists-p "build")
      (delete-directory "build" t))
    (epurple-server--configure-compile)))

(defun epurple-server-running-p ()
  (with-struct-slots (process socket) epurple-server epurple-server
    (and process socket)))

(defun epurple-server-send (buf)
  (when (epurple-server-running-p)
    (process-send-string (epurple-server-socket epurple-server) buf)))

(defun epurple-server-exit ()
  (with-struct-slots (process socket) epurple-server epurple-server
    (when process (delete-process process))
    (when socket (delete-process socket)))
  (setf (epurple-server-process epurple-server) nil)
  (setf (epurple-server-socket epurple-server) nil))

(defun epurple-server-init ()
  (if (file-exists-p epurple-server--program)
      (epurple-server--start)
    (message "epurple: compiling server ...")
    (epurple-server--configure-compile)))

(provide 'epurple-server)
