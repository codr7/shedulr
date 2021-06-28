(defpackage time
  (:use cl)
  (:import-from local-time decode-timestamp encode-timestamp timestamp< timestamp>)
  (:import-from whirlog
		column compare-column decode-column encode-column init-column init-record name set-column-values)
  (:export now time-column today))

(in-package time)

(defun now ()
  (local-time:now))

(defun today ()
  (local-time:today))

(defclass time-column (column)
  ())

(defmethod encode-column ((col time-column) val)
  (multiple-value-bind (nsec sec min hour day month year wday daylight-p tz-offset tz) (decode-timestamp val)
    (declare (ignore wday daylight-p))
    (list nsec sec min hour day month year tz tz-offset)))

(defmethod decode-column ((col time-column) val)
  (destructuring-bind (nsec sec min hour day month year tz tz-offset) val
    (encode-timestamp nsec sec min hour day month year :timezone tz :offset tz-offset)))

(defmethod compare-column ((col time-column) x y)
  (declare (type local-time:timestamp x y))
  
  (cond
    ((timestamp< x y) :lt)
    ((timestamp> x y) :gt)
    (t :eq)))

