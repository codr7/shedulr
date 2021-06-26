(defpackage password
  (:use cl)
  (:import-from whirlog
	        decode-column encode-column string-column)
  (:export check hash password-column new new-salt))

(in-package password)

(defun new-salt ()
  (cl-bcrypt:generate-salt))

(defun hash (in salt)
  (cl-bcrypt:make-password in :salt salt))

(defun new (in)
  (hash in (new-salt)))

(defun check (x y)
  (string= (cl-bcrypt:encode (hash y (cl-bcrypt:salt x))) (cl-bcrypt:encode x)))

(defclass password-column (string-column)
  ())

(defmethod encode-column ((col password-column) val)
  (cl-bcrypt:encode val))

(defmethod decode-column ((col password-column) val)
  (cl-bcrypt:decode val))
