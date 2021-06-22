(defpackage id
  (:use cl)
  (:import-from uuid byte-array-to-uuid make-v4-uuid uuid-to-byte-array)
  (:import-from whirlog
		column compare-column decode-column encode-column init-column init-record name set-column-values)
  (:export id-column new-id))

(in-package id)

(defparameter *length* 16)
  
(defun new-id ()
  (make-v4-uuid))

(defclass id-column (column)
  ())

(defmethod init-column ((col id-column) rec)
  (set-column-values rec (name col) (new-id)))

(defmethod encode-column ((col id-column) val)
  (uuid-to-byte-array val))

(defmethod decode-column ((col id-column) val)
  (byte-array-to-uuid val))

(defmethod compare-column ((col id-column) xs ys)
  (dotimes (i *length*)
    (let ((x (aref xs i)) (y (aref ys i)))
      (cond
	((< x y) (return-from compare-column :lt))
	((> x y) (return-from compare-column :gt)))))
  :eq)
