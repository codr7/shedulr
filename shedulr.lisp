(defpackage shedulr
  (:use cl)
  (:import-from hash hash-column)
  (:import-from lset lset-column)
  (:import-from time time-column)
  (:import-from unique unique-column)
  (:import-from whirlog
		column column-value compare-column decode-column do-context encode-column find-record init-column
		init-record let-tables name new-record number-column record-column record-count record-key
		set-column-values string-column store-record with-db)
  (:export repl))

(in-package shedulr)

(defparameter *version* 1)
(defparameter *min-time* (time:new 1 1 1 0 0))
(defparameter *max-time* (time:new 9999 12 31 23 59))

(defvar accounts)
(defvar calendars)
(defvar resources)
(defvar timesheets)
(defvar users)

(defvar internal-time)

(defun new-calendar (resource slot-start slot-end total)
  (init-record calendars (new-record 'calendar-resource resource
				     'calendar-slot-start slot-start
				     'calendar-slot-end slot-end
				     'calendar-total total
				     'calendar-free total)))

(defun new-resource (name total)
  (let ((rc (init-record resources (new-record 'resource-name name))))
    (store-record calendars (new-calendar rc *min-time* *max-time* total))
    rc))

(defun new-user (id password)
  (let* ((rc (new-resource id 1))
	 (user (init-record users (new-record 'user-id id
					      'user-password (hash:new password)
					      'user-resource rc))))
    (store-record resources rc)
    user))

(defun login (id password)
  (let ((found (find-record users `#(,id))))
    (unless found
      (error "Unknown user id: ~a" id))
    (unless (hash:check (column-value found 'user-password) password)
      (error "Wrong password"))
    found))

(defun new-account (name)
  (init-record accounts (new-record 'account-name name)))

(defun init-db ()
  (store-record users (new-user "shedulr" "shedulr"))
  
  (let* ((time (new-account "Time"))
	 (time-key (record-key accounts time)))
    (store-record accounts time)
    
    (setf internal-time (new-account "Internal"))
    (lset:add (column-value internal-time 'account-tags) time-key)
    (store-record accounts internal-time)
  
    (let ((external-time (new-account "External")))
      (lset:add (column-value external-time 'account-tags) time-key)
      (store-record accounts external-time))))

(defun new-timesheet (&key user account day minutes)
  (init-record timesheets (new-record 'timesheet-id (record-count timesheets)
				      'timesheet-user user
				      'timesheet-account account
				      'timesheet-created-at (time:now)
				      'timesheet-day day
				      'timesheet-minutes minutes)))
(defun repl ()
  (let-tables ((accounts
		(account-id :type unique :key? t)
		account-name
		(account-description :nil? t)
		(account-tags :type (lset record :table t)))
	       (resources
		(resource-id :type unique :key? t)
		resource-name
		(resource-description :nil? t)
		(resource-tags :type (lset record :table t)))
	       (calendars
		(calendar-resource :type record :table resources :key? t)
		(calendar-slot-start :type time :key? t)
		(calendar-slot-end :type time)
		calendar-total
		calendar-free)
	       (users
		(user-id :key? t)
		(user-password :type hash)
		(user-resource :type record :table resources))
	       (timesheets
		(timesheet-id :key? t)
		(timesheet-user :type record :table users)
		(timesheet-account :type record :table accounts)
		(timesheet-created-at :type time)
		(timesheet-day :type time)
		(timesheet-minutes)))
    (with-db ("/tmp/shedulr/" (accounts resources calendars users timesheets))
      (do-context ()
	(when (zerop (record-count users))
	  (init-db))

	(login "shedulr" "shedulr")
	
	(let ((acc (new-account "Project A")))
	  (lset:add (column-value acc 'account-tags) internal-time)
	  (store-record accounts acc)
	  (store-record timesheets (new-timesheet :user (find-record users #("shedulr"))
						  :account acc
						  :day (time:today)
						  :minutes (* 8 60)))))

      (let ((done? (gensym)))
	(labels ((read-form ()
		   (write-string "> ")
		   (let ((f (read *standard-input* nil done?)))
		     (unless (eq f done?)
		       (eval f)
		       (read-form)))))
	  (read-form))))))
