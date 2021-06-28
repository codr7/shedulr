(defpackage time
  (:use cl)
  (:import-from local-time
		decode-timestamp encode-timestamp nsec-of timestamp< timestamp> timestamp-to-universal
		universal-to-timestamp)
  (:import-from whirlog
		column compare-column decode-column encode-column init-column init-record name set-column-values)
  (:export *min* *max* new now time-column today))

(in-package time)

(defun now ()
  (local-time:now))

(defun today ()
  (local-time:today))

(defun new (year month day &optional (hour 0) (min 0) (sec 0) (nsec 0))
  (encode-timestamp nsec sec min hour day month year))

(defclass time-column (column)
  ())

(defmethod encode-column ((col time-column) val)
  (cons (timestamp-to-universal val) (nsec-of val)))

(defmethod decode-column ((col time-column) val)
  (universal-to-timestamp (first val) :nsec (second val)))

(defmethod compare-column ((col time-column) x y)
  (ecase (sort:compare (first x) (first y))
    (:lt :lt)
    (:gt :gt)
    (sort:compare (rest x) (rest y))))


