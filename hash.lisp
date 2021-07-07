(defpackage hash
  (:use cl)
  (:import-from whirlog
	        column decode-column encode-column)
  (:export check hash hash-column new new-salt))

(in-package hash)

(defun new-salt ()
  (cl-bcrypt:generate-salt))

(defun hash (in salt)
  (cl-bcrypt:make-password in :salt salt))

(defun new (in)
  (hash in (new-salt)))

(defun check (x y)
  (string= (cl-bcrypt:encode (hash y (cl-bcrypt:salt x))) (cl-bcrypt:encode x)))

(defclass hash-column (column)
  ())

(defmethod encode-column ((col hash-column) (val cl-bcrypt:password))
  (cl-bcrypt:encode val))

(defmethod decode-column ((col hash-column) val)
  (cl-bcrypt:decode val))
