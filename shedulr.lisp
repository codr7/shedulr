(defpackage shedulr
  (:use cl)
  (:import-from lset lset-column)
  (:import-from password password-column)
  (:import-from time time-column)
  (:import-from unique unique-column)
  (:import-from whirlog
		column column-value compare-column decode-column do-context encode-column find-record init-column
		init-record let-tables name new-record number-column record-count set-column-values string-column
		store-record with-db)
  (:export repl))

(in-package shedulr)

(defparameter *version* 1)

(defvar accounts)
(defvar calendars)
(defvar resources)
(defvar timesheets)
(defvar users)

(defparameter *min-time* (time:new 1 1 1 0 0))
(defparameter *max-time* (time:new 9999 12 31 23 59))
  
(defun new-calendar (resource slot-start slot-end total)
  (init-record calendars (new-record 'calendar-resource-id (column-value resource 'resource-id)
				     'calendar-slot-start slot-start
				     'calendar-slot-end slot-end
				     'calendar-total total
				     'calendar-free total)))

(defun new-resource (name total)
  (let ((res (init-record resources (new-record 'resource-name name))))
    (store-record calendars (new-calendar res *min-time* *max-time* total))
    res))

(defun new-user (id password)
  (let* ((res (new-resource id 1))
	 (user (init-record users (new-record 'user-id id
					      'user-password (password:new password)
					      'user-resource (column-value res 'resource-id)))))
    
    user))

(defun login (id password)
  (let ((found (find-record users `#(,id))))
    (unless found
      (error "Unknown user id: ~a" id))
    (unless (password:check (column-value found 'user-password) password)
      (error "Wrong password"))
    found))

(defun new-account (name)
  (init-record accounts (new-record 'account-name name)))

(defun init-db ()
  (store-record users (new-user "shedulr" "shedulr"))
  
  (let* ((time (new-account "Time"))
	 (time-id (column-value time 'account-id)))
    (store-record accounts time)
    
    (let ((internal-accounts (new-account "Internal")))
      (lset:add (column-value internal-accounts 'account-parents) time-id)
      (store-record accounts internal-accounts))
    
    (let ((external-accounts (new-account "External")))
      (lset:add (column-value external-accounts 'account-parents) time-id)
      (store-record accounts external-accounts))

    (let ((sink (new-account "Sink")))
      (lset:add (column-value sink 'account-parents) time-id)
      (store-record accounts sink))))

(defun new-timesheet (&key user debit-account credit-account day minutes)
  (init-record timesheets (new-record 'timesheet-id (record-count timesheets)
				      'timesheet-user-id (column-value user 'user-id)
				      'timesheet-debit-account-id (column-value debit-account 'account-id)
				      'timesheet-credit-account-id (column-value credit-account 'account-id)
				      'timesheet-created-at (time:now)
				      'timesheet-day day
				      'timesheet-minutes minutes)))
(defun repl ()
  (let-tables ((accounts
		(account-id :type unique :key? t)
		(account-name :type string)
		(account-description :type string)
		(account-parents :type (lset unique)))
	       (resources
		(resource-id :type unique :key? t)
		(resource-name :type string)
		(resource-description :type string)
		(resource-parents :type (lset unique)))
	       (calendars
		(calendar-resource-id :type unique :key? t)
		(calendar-slot-start :type time :key? t)
		(calendar-slot-end :type time)
		(calendar-total :type number)
		(calendar-free :type number))
	       (users
		(user-id :type string :key? t)
		(user-password :type password)
		(user-resource :type unique))
	       (timesheets
		(timesheet-id :type number :key? t)
		(timesheet-user-id :type string)
		(timesheet-debit-account-id :type unique)
		(timesheet-credit-account-id :type unique)
		(timesheet-created-at :type time)
		(timesheet-day :type time)
		(timesheet-minutes :type number)))
    (with-db ("/tmp/shedulr/" (accounts resources calendars users timesheets))
      (do-context ()
	(when (zerop (record-count users))
	  (init-db))

	(login "shedulr" "shedulr")
	
	(let ((x (new-account "x"))
	      (y (new-account "y")))
	  (store-record accounts x)
	  (store-record accounts y)
	  (store-record timesheets (new-timesheet :user (find-record users #("shedulr"))
						  :debit-account x
						  :credit-account y
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
