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

(require 'epurple-server)
(require 'epurple-utils)

;;; Struct

(cl-defstruct epurple-account username alias protocol_id)

;;; Internal Variables

(defvar epurple--accounts nil)

;;; Internal Functions

(defun epurple--accounts-info (accounts)
  (dolist (account accounts)
    (push (alist-to-struct account 'epurple-account) epurple--accounts)))

;;; External Functions

(defun epurple-init-done ()
  (epurple-accounts-get-all #'epurple--accounts-info))

(defun epurple-exit ()
  (interactive)
  (setq epurple--accounts nil)
  (epurple-server-exit))

(defun epurple-init ()
  (interactive)
  (epurple-exit)
  (epurple-server-init))

(provide 'epurple)
