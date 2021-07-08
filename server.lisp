(defpackage server
  (:use cl)
  (:import-from sb-bsd-sockets inet-socket socket-accept socket-bind socket-close socket-file-descriptor socket-listen
		socket-make-stream sockopt-reuse-address sockopt-tcp-nodelay)
  (:import-from sb-sys add-fd-handler serve-event)
  (:export accept-client start stop tests))

(in-package server)

(defun accept-client (server)
  (let* ((c (socket-accept server))
	 (fd (socket-file-descriptor c))
	 (cs (socket-make-stream c :input t :output t :buffering :none :serve-events t :auto-close t)))
    (sockopt-tcp-nodelay c)

    (add-fd-handler fd :input (lambda (fd)
				(declare (ignore fd))
				(format t "client input~%")))

    (add-fd-handler fd :output (lambda (fd)
				 (declare (ignore fd))
				 (format t "client output~%")))

    cs))

(defun start (address port &key (backlog 10))
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address s) t)
    (socket-bind s address port)
    (socket-listen s backlog)
    (add-fd-handler (socket-file-descriptor s) :input (lambda (fd)
							(declare (ignore fd))
							(format t "accept client~%")
							(accept-client s)))
    s))

(defun stop (server)
  (socket-close server))

(defun tests ()
  (let* ((address '(127 0 0 1))
	 (port 9090)
	 (s (server:start address port))
	 (c (client:connect address port)))
    (serve-event)
    (client:disconnect c)
    (server:stop s)))
