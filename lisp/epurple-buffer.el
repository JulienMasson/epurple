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

(require 'lui)

;;; Struct

(cl-defstruct epurple-buffer
  conv-type
  conv-name
  buffer
  mute-p
  unread-p
  (unread-count 0))

;;; Epurple mode

(define-derived-mode epurple-mode lui-mode "Epurple"
  "Epurple major mode."
  (lui-set-prompt lui-prompt-string)
  (add-hook 'kill-buffer-hook #'epurple-buffer--killed nil t))

;;; Internal Variables

(defvar-local epurple--buffer nil)

;;; Internal Functions

(defun epurple-buffer--insert-header (sender time &optional icon)
  (let* ((ts (format-time-string lui-time-stamp-format time
                                 lui-time-stamp-zone))
	 (ts-str (propertize ts 'face 'lui-time-stamp-face))
	 (icon-str (when icon (propertize "image" 'display icon)))
	 (icon-length (if icon 6 0))
	 (spaces (- lui-fill-column (length ts) icon-length))
	 (fmt (format "%%-%ds%%s" spaces))
	 (inhibit-read-only t)
	 (beg lui-output-marker))
    (save-excursion
      (goto-char lui-output-marker)
      (when icon-str (insert icon-str " "))
      (insert (format fmt sender ts-str))
      (insert "\n")
      (set-marker lui-output-marker (point)))))

(defun epurple-buffer--propertize-sender (account sender)
  (with-struct-slots (alias face) epurple-account account
    (if (or (string= "(null)" sender) (string= alias sender))
	(propertize epurple-nick-name 'face 'epurple-nick-face)
      (propertize sender 'face face))))

(defun epurple-buffer--insert-msg (account buffer sender msg time)
  (let ((sender (epurple-buffer--propertize-sender account sender)))
    (with-current-buffer buffer
      (epurple-buffer--insert-header sender time)
      (lui-insert (concat msg "\n")))))

(defun epurple-buffer--send (msg)
  (when-let ((account (epurple--find-account-by-prpl-buffer epurple--buffer)))
    (epurple-send-msg account epurple--buffer msg)))

(defun epurple-buffer--setup (account prpl-buffer)
  (with-struct-slots (conv-type conv-name) epurple-buffer prpl-buffer
    (let* ((account-name (epurple-account-name account))
	   (buffer-name (format "*%s: %s*" account-name conv-name)))
      (with-current-buffer (get-buffer-create buffer-name)
	(epurple-mode)
	(cond ((= conv-type 1) (setq mode-name "Epurple IM"))
	      ((= conv-type 2) (setq mode-name "Epurple Chat")))
	(force-mode-line-update)
	(setq lui-input-function #'epurple-buffer--send)
	(goto-char (point-max))
	(setq epurple--buffer prpl-buffer)
	(current-buffer)))))

(defun epurple-buffer--killed ()
  (catch 'found
    (dolist (account epurple-accounts)
      (let ((prpl-buffers (epurple-account-prpl-buffers account)))
	(when (member epurple--buffer prpl-buffers)
	  (setf (epurple-account-prpl-buffers account)
		(delete epurple--buffer prpl-buffers))
	  (throw 'found nil))))))

(defun epurple-buffer--find (account type name)
  (catch 'found
    (dolist (prpl-buffer (epurple-account-prpl-buffers account))
      (with-struct-slots (conv-type conv-name buffer) epurple-buffer prpl-buffer
	(when (and (= conv-type type) (string= conv-name name))
	  (throw 'found buffer))))))

(defun epurple-buffer--new (account type name)
  (let ((prpl-buffer (make-epurple-buffer))
	(prpl-buffers (epurple-account-prpl-buffers account)))
    (with-struct-slots (conv-type conv-name buffer) epurple-buffer prpl-buffer
      (setq conv-type type)
      (setq conv-name name)
      (setq buffer (epurple-buffer--setup account prpl-buffer))
      (push prpl-buffer prpl-buffers)
      (setf (epurple-account-prpl-buffers account) prpl-buffers)
      buffer)))

;;; External Functions

(defun epurple-buffer-display (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun epurple-buffer-new-msg (data)
  (let-alist data
    (when-let ((account (epurple--find-account-by-username .username)))
      (let ((buffer (epurple-buffer--find account .conv-type .conv-name)))
	(unless buffer
	  (setq buffer (epurple-buffer--new account .conv-type .conv-name))
	  (epurple-buffer-display buffer))
	(epurple-buffer--insert-msg account buffer .sender .msg .time)))))

(defun epurple-buffer-new-conv (account conv-type conv-name)
  (let ((buffer (epurple-buffer--find account conv-type conv-name)))
    (unless buffer
      (setq buffer (epurple-buffer--new account conv-type conv-name))
      (epurple-buffer-display buffer))))

(provide 'epurple-buffer)
