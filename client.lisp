(defpackage client
  (:use cl)
  (:import-from sb-bsd-sockets inet-socket socket-connect socket-make-stream sockopt-tcp-nodelay)
  (:export connect disconnect))

(in-package client)

(defun connect (address port)
  (let ((c (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-connect c address port)
    (sockopt-tcp-nodelay c)
    (socket-make-stream c :input t :output t :buffering :none :serve-events t :auto-close t)))

(defun disconnect (client)
  (close client))
