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
  (let-tables ((accounts
		(account-id :type id :key? t)
		(account-name :type string)
		(account-parents :type (lset id)))
	       (users
		(user-id :type string :key? t)
		(user-password :type password)))
    (with-db ("/tmp/shedulr/" (accounts users))
      (do-context ()
	(when (zerop (record-count users))
	  (store-record users
			(init-record users
				     (new-record 'user-id "shedulr"
						 'user-password (password:new "shedulr"))))
	  
	  (let* ((all-accounts (init-record accounts (new-record 'account-name "All")))
		 (all-id (column-value all-accounts 'account-id)))
	    (store-record accounts all-accounts)
	    
	    (let ((internal-accounts (init-record accounts (new-record 'account-name "Internal"))))
	      (lset:add (column-value internal-accounts 'account-parents) all-id)
	      (store-record accounts internal-accounts))
	    
	    (let ((external-accounts (init-record accounts (new-record 'account-name "External"))))
	      (lset:add (column-value external-accounts 'account-parents) all-id)
	      (store-record accounts external-accounts))))

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
