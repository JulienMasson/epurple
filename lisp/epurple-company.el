;;; epurple-company.el --- Instant messaging environment for Emacs

;; Copyright (C) 2021 Julien Masson.

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

(require 'company)

;;; Internal Functions

(defun epurple-company--type (arg)
  (save-excursion
    (unless (string= arg "")
      (re-search-backward arg nil t))
    (eq ?@ (char-before))))

(defun epurple-company--candidates (arg)
  (when-let* ((account (epurple--find-account-by-prpl-buffer epurple--buffer))
	      (buddies (s-mapcar (epurple-account-buddies account) 'name))
	      (candidates (if (epurple-company--type arg)
			      buddies
			    (epurple-account-chats account))))
    (cl-remove-if-not (lambda (candidate) (string-prefix-p arg candidate))
		      candidates)))

;;; External Functions

(defun epurple-company (command &optional arg &rest _ignore)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'epurple-company))
    (prefix (and (derived-mode-p 'lui-mode)
		 (or (company-grab-symbol-cons "[@#]") 'stop)))
    (candidates (epurple-company--candidates arg))
    (no-cache t)))

(defun epurple-company-setup ()
  (company-mode)
  (setq-local company-backends '(epurple-company)))

(provide 'epurple-company)
