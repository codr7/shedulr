(defpackage ls
  (:use cl)
  (:import-from sort compare)
  (:import-from whirlog
		column compare-column decode-column encode-column init-column init-record name set-column-values)
  (:export clear new ls-column tests add size))

(in-package ls)

(defstruct ls
  (compare #'sort:compare :type function)
  (items nil :type list)
  (size 0 :type integer))

(defun new-ls (&rest args)
  (let ((s (apply #'make-ls args)))
    (setf (ls-size s) (length (ls-items s)))
    s))

(defun size (set)
  (ls-size set))

(defun clear (set)
  (setf (ls-items set) nil (ls-size set) 0))

(defun add (set val)
  (let (ok?)
    (labels ((rec (in)
	       (if in
		   (let ((rhs (first in)))
		     (ecase (funcall (ls-compare set) val rhs)
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
      (setf (ls-items set) (rec (ls-items set))))
    
    (when ok?
      (incf (ls-size set))
      t)))

(defclass ls-column (column)
  ((item-column :initarg :item-column :reader item-column)))

(defmethod init-column ((col ls-column) rec)
  (ls-column-values rec (name col) (new-ls :compare (lambda (x y)
							 (compare-column (item-column col) x y)))))

(defmethod compare-column ((col ls-column) xs ys)
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
    (rec (ls-items xs) (ls-items ys))))

(defun tests ()
  (let ((s (new-ls)))
    (assert (= (ls-size s) 0))
    (assert (add s 1))
    (assert (add s 3))
    (assert (add s 5))
    (assert (not (add s 1)))
    (assert (not (add s 3)))
    (assert (not (add s 5)))
    (assert (add s 4))
    (assert (equal (ls-items s) '(1 3 4 5)))
    (assert (= (ls-size s) 4)))
  
  (let ((s (new-ls :items '(1 3 5))))
    (assert (= (ls-size s) 3))
    (assert (add s 4))
    (assert (equal (ls-items s) '(1 3 4 5)))
    (assert (= (ls-size s) 4))))

