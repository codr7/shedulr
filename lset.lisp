(defpackage lset
  (:use cl)
  (:import-from sort compare)
  (:import-from whirlog
		column compare-column decode-column encode-column init-column init-record name set-column-values)
  (:export add clear new lset lset-column size tests))

(in-package lset)

(defstruct lset
  (compare #'sort:compare :type function)
  (items nil :type list)
  (size 0 :type integer))

(defun new (&rest args)
  (let ((s (apply #'make-lset args)))
    (setf (lset-size s) (length (lset-items s)))
    s))

(defun size (set)
  (lset-size set))

(defun clear (set)
  (setf (lset-items set) nil (lset-size set) 0))

(defun add (set val)
  (let (ok?)
    (labels ((rec (in)
	       (if in
		   (let ((rhs (first in)))
		     (ecase (funcall (lset-compare set) val rhs)
		       (:lt
			(setf ok? t)
			(cons val in))
		       (:eq in)
		       (:gt (if (rest in)
				(cons rhs (rec (rest in)))
				(progn
				  (setf ok? t)
				  (list rhs val))))))
		   (progn
		     (setf ok? t)			
		     (list val)))))
      (setf (lset-items set) (rec (lset-items set))))
    
    (when ok?
      (incf (lset-size set))
      t)))

(defclass lset-column (column)
  ((item-column :initarg :item-column :reader item-column)))

(defmethod init-column ((col lset-column) rec)
  (set-column-values rec (name col) (new :compare (lambda (x y)
						    (compare-column (item-column col) x y)))))

(defmethod compare-column ((col lset-column) xs ys)
  (labels ((rec (xs ys)
	     (let ((x (first xs)) (y (first ys)))
	       (cond
		 ((and (null x) y) :lt)
		 ((and x (null y)) :gt)
		 ((null x) :eq)
		 (t (ecase (compare-column (item-column col) x y)
		      (:lt :lt)
		      (:gt :gt)
		      (:eq (rec (rest xs) (rest ys)))))))))
    (rec (lset-items xs) (lset-items ys))))

(defun tests ()
  (let ((s (new)))
    (assert (= (lset-size s) 0))
    (assert (add s 1))
    (assert (add s 3))
    (assert (add s 5))
    (assert (not (add s 1)))
    (assert (not (add s 3)))
    (assert (not (add s 5)))
    (assert (add s 4))
    (assert (equal (lset-items s) '(1 3 4 5)))
    (assert (= (lset-size s) 4)))
  
  (let ((s (new :items '(1 3 5))))
    (assert (= (lset-size s) 3))
    (assert (add s 4))
    (assert (equal (lset-items s) '(1 3 4 5)))
    (assert (= (lset-size s) 4))))

