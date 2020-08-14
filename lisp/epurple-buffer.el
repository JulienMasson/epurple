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

(require 'dom)
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

;;; Faces

(defface epurple-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face used to `>'"
  :group 'epurple-faces)

(defface epurple-inline-code-face
  '((t (:foreground "#fb8512" :box (:line-width -1 :color "#3b3b3b"))))
  "Face used to between ``'"
  :group 'epurple-faces)

(defface epurple-block-code-face
  '((t (:background "#2e2e2e")))
  "Face used to between ````'"
  :group 'epurple-faces)

(defface epurple-buddy-offline-face
  '((t (:foreground "#696969")))
  "Face used when buddy is offline"
  :group 'epurple-faces)

(defface epurple-buddy-typing-face
  '((t :inherit warning))
  "Face used when buddy is offline"
  :group 'epurple-faces)

;;; Customization

(defcustom epurple-buffer-secs-timeout (* 5 60)
  "Specify the number of seconds difference for which we should insert header."
  :group 'epurple
  :type 'number)

(defcustom epurple-mrkdwn-blockquote-sign "┃"
  "Used to display > when blockquote"
  :group 'epurple
  :type 'string)

;;; Internal Variables

(defvar-local epurple--buffer nil)

(defconst epurple-blockquote-regexp
  "^[ \t]*\\([A-Z]?>\\)\\([ \t]*\\)\\(.+\\)$")

(defconst epurple-inline-code-regexp
  "\\(?:\\`\\|\\W\\)\\(\\(`\\)\\(\\(?:.\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)")

(defconst epurple-block-code-regexp
  "\\(?:^\\|[[:blank:]]\\)\\(```\\)\\(?:\n\\)?\\(\\(.\\|\n\\)*?\\)\\(\n?```\\)[[:blank:]]*$")

(defconst epurple--icons-dir (expand-file-name (concat (file-name-directory load-file-name)
							      "../icons/")))
(defconst epurple--icons-available (concat epurple--icons-dir "available.png"))
(defconst epurple--icons-offline (concat epurple--icons-dir "offline.png"))

;;; Internal Functions

(defun epurple-buffer--find (account type name)
  (catch 'found
    (dolist (prpl-buffer (epurple-account-prpl-buffers account))
      (with-struct-slots (conv-type conv-name buffer) epurple-buffer prpl-buffer
	(when (and (= conv-type type) (string= conv-name name))
	  (throw 'found buffer))))))

(defun epurple-buffer--insert-header (sender time icon)
  (let* ((ts (format-time-string lui-time-stamp-format time
                                 lui-time-stamp-zone))
	 (ts-str (propertize ts 'face 'lui-time-stamp-face))
	 (icon-str (when icon (propertize "image" 'display icon)))
	 (icon-length (if icon 5 0))
	 (spaces (- lui-fill-column (length ts) icon-length))
	 (fmt (format "%%-%ds%%s" spaces))
	 (inhibit-read-only t)
	 (beg lui-output-marker))
    (save-excursion
      (goto-char lui-output-marker)
      (when icon-str (insert icon-str " "))
      (insert (format fmt sender ts-str))
      (add-text-properties lui-output-marker (point) (list :epurple-time time
							   :epurple-sender sender))
      (insert "\n")
      (set-marker lui-output-marker (point)))))

(defun epurple-buffer--find-icon (account sender)
  (with-struct-slots (alias) epurple-account account
    (when-let* ((buddy (if (string= "(null)" sender)
			  (epurple--find-buddy account alias)
			 (epurple--find-buddy account sender)))
		(icon (epurple-buddy-icon buddy)))
      (when (and icon (not (string= icon "")))
	(create-image icon nil nil :scale 0.17 :ascent 80)))))

(defun epurple-buffer--propertize-sender (account sender)
  (with-struct-slots (alias face) epurple-account account
    (if (or (string= "(null)" sender) (string= alias sender))
	(propertize epurple-nick-name 'face 'epurple-nick-face)
      (propertize sender 'face face))))

(defun epurple-buffer--html-to-text (msg)
  (with-temp-buffer
    (insert msg)
    (dom-texts (libxml-parse-html-region (point-min) (point)))))

(defun epurple-buffer--incf-unread-count (account buffer)
  (when-let ((prpl-buffer (cl-find-if (lambda (prpl-buffer)
					(eq buffer (epurple-buffer-buffer prpl-buffer)))
				      (epurple-account-prpl-buffers account))))
    (with-struct-slots (unread-p unread-count) epurple-buffer prpl-buffer
      (setq unread-p t)
      (setq unread-count (incf unread-count)))))

(defun epurple-buffer--need-header-p (sender time)
  (let (previous-time previous-sender)
    (save-excursion
      (goto-char lui-output-marker)
      (while (and (or (not previous-time) (not previous-sender)) (not (bobp)))
	(forward-line -1)
	(when-let ((properties (text-properties-at (point))))
	  (setq previous-time (plist-get properties :epurple-time))
	  (setq previous-sender (plist-get properties :epurple-sender)))))
    (if (and previous-time previous-sender)
	(or (not (string= previous-sender sender))
	    (> (time-to-seconds (time-subtract time previous-time))
	       epurple-buffer-secs-timeout))
      t)))

(defun epurple-buffer--mrkdwn-blockquote ()
  (while (re-search-forward epurple-blockquote-regexp (point-max) t)
    (when-let ((markup-beg (match-beginning 1))
               (markup-end (match-end 1))
	       (beg (match-beginning 3))
               (end (match-end 3)))
      (put-text-property markup-beg markup-end 'display
			 (propertize epurple-mrkdwn-blockquote-sign
				     'face 'epurple-blockquote-face))
      (put-text-property beg end 'face 'epurple-blockquote-face))))

(defun epurple-buffer--mrkdwn-inline-code ()
  (while (re-search-forward epurple-inline-code-regexp (point-max) t)
    (when-let ((markup-start-beg (match-beginning 2))
               (markup-start-end (match-end 2))
	       (beg (match-beginning 3))
               (end (match-end 3))
	       (markup-end-beg (match-beginning 4))
               (markup-end-end (match-end 4)))
      (put-text-property markup-start-beg markup-start-end 'invisible t)
      (put-text-property beg end 'face 'epurple-inline-code-face)
      (put-text-property markup-end-beg markup-end-end 'invisible t))))

(defun epurple-buffer--mrkdwn-block-code ()
  (while (re-search-forward epurple-block-code-regexp (point-max) t)
    (when-let ((markup-start-beg (match-beginning 1))
               (markup-start-end (match-end 1))
	       (beg (match-beginning 2))
               (end (match-end 2))
	       (markup-end-beg (match-beginning 4))
               (markup-end-end (match-end 4)))
      (put-text-property markup-start-beg markup-start-end 'invisible t)
      (put-text-property beg end 'face 'epurple-block-code-face)
      (put-text-property markup-end-beg markup-end-end 'invisible t))))

(defun epurple-buffer--mrkdwn-fontify (text)
  (with-temp-buffer
    (insert text)
    (dolist (func (list #'epurple-buffer--mrkdwn-blockquote
			#'epurple-buffer--mrkdwn-block-code
			#'epurple-buffer--mrkdwn-inline-code))
      (goto-char (point-min))
      (funcall func))
    (buffer-string)))

(defun epurple-buffer--clean-up (body)
  ;; force new line after point
  (replace-regexp-in-string "\\.\\s-+" ".\n" body))

(defun epurple-buffer--fill-body (beg end)
  (let ((inhibit-read-only t)
	(fill-column lui-fill-column)
	(cur beg))
    (save-excursion
      (goto-char end)
      (save-excursion
	(goto-char beg)
	(while (and (re-search-forward "\n" nil t)
		    (< (point) end))
	  (fill-region cur (point) 'left)
	  (setq cur (point))))
      (set-marker lui-output-marker (point)))))

(defun epurple-buffer--insert-body (body)
  (let* ((beg (marker-position lui-output-marker))
	 (body (epurple-buffer--clean-up body))
	 (body (epurple-buffer--mrkdwn-fontify body)))
    (lui-insert (propertize (concat body "\n") 'lui-format-argument 'body))
    (epurple-buffer--fill-body beg (marker-position lui-output-marker))))

(defun epurple-buffer--insert-msg (account buffer sender msg time)
  (unless (eq (window-buffer (selected-window)) buffer)
    (epurple-buffer--incf-unread-count account buffer))
  (let* ((sender (epurple-buffer--propertize-sender account sender))
	 (icon (epurple-buffer--find-icon account sender))
	 (msg (decode-coding-string msg 'utf-8))
	 (msg (epurple-buffer--html-to-text msg)))
    (with-current-buffer buffer
      (when (epurple-buffer--need-header-p sender time)
	(epurple-buffer--insert-header sender time icon))
      (epurple-buffer--insert-body msg))))

(defun epurple-buffer--send (msg)
  (when-let ((account (epurple--find-account-by-prpl-buffer epurple--buffer)))
    (epurple-send-msg account epurple--buffer msg)))

(defun epurple-buffer--im-header-line (account buddy-name)
  (when-let ((buddy (epurple--find-buddy account buddy-name)))
    (with-struct-slots (name signed-on typing-p) epurple-buddy buddy
      (let* ((name-face (if signed-on (epurple-account-face account)
			  'epurple-buddy-offline-face))
	     (name-str (propertize name 'face name-face))
	     (name-length (length name-str))
	     (typing-str (if typing-p
			     (propertize "Typing ..." 'face 'epurple-buddy-typing-face)
			   ""))
	     (typing-length (if typing-p (length typing-str) 0))
	     (icon-image (create-image (if signed-on
					   epurple--icons-available
					 epurple--icons-offline)
				       nil nil :ascent 80))
	     (icon-length 2)
	     (fmt (format "%%%ds %%s" (+ (/ lui-fill-column 2) (/ (+ name-length icon-length) 2)))))
	(setq header-line-format (format fmt (concat (propertize "x" 'display icon-image)
						     " " name-str)
					 typing-str))
	(force-mode-line-update)))))

(defun epurple-buffer--setup (account prpl-buffer)
  (with-struct-slots (conv-type conv-name) epurple-buffer prpl-buffer
    (let* ((account-name (epurple-account-name account))
	   (buffer-name (format "*%s: %s*" account-name conv-name)))
      (with-current-buffer (get-buffer-create buffer-name)
	(epurple-mode)
	(cond ((= conv-type 1)
	       (epurple-buffer--im-header-line account conv-name)
	       (setq mode-name "Epurple IM"))
	      ((= conv-type 2)
	       (setq mode-name "Epurple Chat")))
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
    (switch-to-buffer-other-window buffer))
  (when-let ((account (epurple--find-account-by-prpl-buffer epurple--buffer)))
    (with-struct-slots (conv-type conv-name) epurple-buffer epurple--buffer
      (epurple-update-conv account conv-type conv-name))))

(defun epurple-buffer-update (account conv-name)
  ;; fow now only IM conv are updated
  (when-let ((buffer (epurple-buffer--find account 1 conv-name)))
    (with-current-buffer buffer
      (epurple-buffer--im-header-line account conv-name))))

(defun epurple-buffer-new-msg (header msg)
  (let-alist header
    (when-let ((account (epurple--find-account-by-username .username)))
      (let* ((name (decode-coding-string .conv-name 'utf-8))
	     (sender (decode-coding-string .sender 'utf-8))
	     (buffer (epurple-buffer--find account .conv-type name)))
	(unless buffer
	  (setq buffer (epurple-buffer--new account .conv-type name)))
	(epurple-buffer--insert-msg account buffer sender msg .time)))))

(defun epurple-buffer-conv (account conv-type conv-name)
  (let* ((name (decode-coding-string conv-name 'utf-8))
	 (buffer (epurple-buffer--find account conv-type name)))
    (unless buffer
      (setq buffer (epurple-buffer--new account conv-type name))
      (epurple-buffer-display buffer))))

(provide 'epurple-buffer)
