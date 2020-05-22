;;; epurple-utils.el --- Utils for epurple

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

(defmacro with-struct-slots (spec-list type inst &rest body)
  (declare (indent defun) (debug (sexp sexp def-body)))
  (macroexp-let2 nil inst inst
    `(cl-symbol-macrolet
         ,(mapcar (lambda (entry)
                    (let* ((slot-var (if (listp entry) (car entry) entry))
			   (slot (if (listp entry) (cadr entry) entry))
			   (idx (cl-struct-slot-offset type slot)))
                      (list slot-var `(aref ,inst ,idx))))
                  spec-list)
       (unless (cl-typep ,inst ',type)
	 (error "%s is not a %s" ',inst ',type))
       ,@body)))

(provide 'epurple-utils)
