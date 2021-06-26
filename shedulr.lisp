(defpackage shedulr
  (:use cl)
  (:import-from id id-column)
  (:import-from lset lset-column)
  (:import-from password password-column)
  (:import-from whirlog
		column column-value compare-column decode-column do-context encode-column find-record init-column
		init-record let-tables name new-record record-count set-column-values string-column store-record with-db)
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
		(user-password :type password)))
    (with-db ("/tmp/shedulr/" (projects users))
      (do-context ()
	(when (zerop (record-count users))
	  (store-record users (new-record 'user-id "shedulr" 'user-password (password:new "shedulr")))
	  
	  (let* ((all-projects (init-record projects (new-record 'project-name "All Projects")))
		 (all-id (column-value all-projects 'project-id)))
	    (store-record projects all-projects)
	    
	    (let ((internal-projects (init-record projects (new-record 'project-name "Internal Projects"))))
	      (lset:add (column-value internal-projects 'project-parents) all-id)
	      (store-record projects internal-projects))
	    
	    (let ((external-projects (init-record projects (new-record 'project-name "External Projects"))))
	      (lset:add (column-value external-projects 'project-parents) all-id)
	      (store-record projects external-projects))))

	(flet ((login (id password)
		 (let ((found (find-record users `#(,id))))
		   (unless found
		     (error "Unknown user id: ~a" id))
		   (unless (password:check (column-value found 'user-password) password)
		     (error "Wrong password")))))
	  (login "shedulr" "shedulr")))
      
      (let ((done? (gensym)))
	(labels ((read-form ()
		   (write-string "> ")
		   (let ((f (read *standard-input* nil done?)))
		     (unless (eq f done?)
		       (eval f)
		       (read-form)))))
	  (read-form))))))
