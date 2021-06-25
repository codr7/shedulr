(defpackage shedulr
  (:use cl)
  (:import-from id id-column)
  (:import-from lset lset-column)
  (:import-from whirlog
		column column-value compare-column decode-column do-context encode-column init-column init-record
		let-tables name new-record record-count set-column-values string-column store-record with-db)
  (:export repl))

(in-package shedulr)

(defparameter *version* 1)

(defun repl ()
  (let-tables ((projects
		(project-id :type id :key? t)
		(project-name :type string)
		(project-parents :type (lset id)))
	       (users
		(user-id :type string :key? t)
		(user-password :type string)))
    (with-db ("/tmp/shedulr/" (projects users))
      (when (zerop (record-count users))
	(do-context ()
	  (store-record users (new-record 'user-id "shedulr" 'user-password "shedulr"))
	  
	  (let* ((all-projects (init-record projects (new-record 'project-name "All Projects")))
		 (all-id (column-value all-projects 'project-id)))
	    (store-record projects all-projects)
	    
	    (let ((internal-projects (init-record projects (new-record 'project-name "Internal Projects"))))
	      (lset:add (column-value internal-projects 'project-parents) all-id)
	      (store-record projects internal-projects))
	    
	    (let ((external-projects (init-record projects (new-record 'project-name "External Projects"))))
	      (lset:add (column-value external-projects 'project-parents) all-id)
	      (store-record projects external-projects)))))
      
      (let ((done? (gensym)))
	(labels ((read-form ()
		   (write-string "> ")
		   (let ((f (read *standard-input* nil done?)))
		     (unless (eq f done?)
		       (eval f)
		       (read-form)))))
	  (read-form))))))

