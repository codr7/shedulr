(defpackage shedulr
  (:use cl)
  (:import-from id id-column)
  (:import-from ls ls-column)
  (:import-from whirlog
		column column-value compare-column decode-column do-context encode-column init-column init-record
		let-tables name new-record set-column-values string-column store-record with-db)
  (:export repl))

(in-package shedulr)

(defparameter *version* 1)


(defun repl ()
  (let-tables ((projects
		(project-id :type id :key? t)
		(project-name :type string)
		(project-parents :type (ls id)))
	       (users
		(user-email :type string :key? t)
		(user-password :type string)))
    (with-db ("/tmp/shedulr/" (projects users))
      (do-context ()
	(let ((all-projects (init-record projects (new-record 'project-name "All Projects"))))
	  (store-record projects all-projects)
	  (let ((internal-projects (init-record projects (new-record 'project-name "Internal Projects"))))
	    (ls:add (column-value internal-projects 'project-parents) (column-value all-projects 'project-id))
	    (store-record projects internal-projects))))
	
      (let ((done? (gensym)))
	(labels ((read-form ()
		   (write-string "> ")
		   (let ((f (read *standard-input* nil done?)))
		     (unless (eq f done?)
		       (eval f)
		       (read-form)))))
	  (read-form))))))

