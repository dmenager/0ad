(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)

(defun tcp-server-2 ()
  (let ((socket (usocket:socket-listen usocket:*wildcard-host*
				       3000
				       :reuse-address '())))
    (loop
       (format t "Waiting for input on ~A~%" socket)
       (usocket:wait-for-input socket)
       (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
	 (format t "Connection made to client ~%")
	 (handle-request stream)
	 (close stream)))))

(defun handle-request (stream)
  (let ((line (read-line stream)))
    (format t "Handling request ~%")
    (format t "You said: ~S~%" line))
  (terpri stream)
  (force-output stream))

(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))
