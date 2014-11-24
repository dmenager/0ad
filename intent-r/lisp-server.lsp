(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)

(defvar *STATES* '())
(defvar *ACTIONS* '())

(defun tcp-server (port) 
  (let ((socket (usocket:socket-listen usocket:*wildcard-host*
				       port
				       :reuse-address t)))
    
    (format t "Waiting for input on ~A~%" socket)
    (usocket:wait-for-input socket)
    (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
      (format t "Connection made to client ~%")
      (loop 
	 (handle-request stream)
	 (clear-input stream)))))

(defun handle-request (stream)
  (let ((line (read-line stream nil 'the-end)))
    (format t "Handling request ~%")
    (format t "You said: ~S~%" line))
    ;(format stream "~A~%" (meanings (to-syms line)))
    ;(format t "Perception: ~A~%~%" (meanings (to-syms line))))
  (force-output stream))

(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))

#|Discovers reward function|#

; mdpr = mdp w/o reward funtion
; phi = feature mapping -- (state -> state features)
; mue = expert's feature expectations
; returns reward
(defun discover-r (mdpr phi mue)
  
  (let ((p (make-hash-table)))
    ;add elements to p
    (map '() 
	 #'(lambda (state)	    
	    (setf (gethash state p) 
		  (nth (+ 1 (random (length *ACTIONS*))) 
		       *ACTIONS*))
	    )
	 *STATES*)
    (mu p)))

#|Compute feature expectation of policy, pi|#

; p = policy
(defun mu (p)
  ;expected value of the sum of the discount * phi
  (let* ((err .01)
	 (gamma .5)
	 ;e-horizon time
	 (He (log (* err (- 1 gamma)) gamma)))
    
    (mapcar #'(lambda (&rest x) (reduce #'+ x))  
	    (map 'list 
		 #'(lambda (st)
		     (do ((i 0 (1+ i))
			  (>= i He))
			 (map 'list 
			      #'(lambda (comp)
				  (* (expt gamma i) comp)) 
			      st)))
		 (act p)))))
  
#|get a random percent|#
(defun random-percent ()
  (/ (+ 0 (random (+ 1 (- 1 0))))) 100)

; p = policy
; returns state
(defun act (p) )


#| SCRATCH 

; test I did for mu functionality
(mapcar #'(lambda (&rest x)
		     (reduce #'+ x)) '(1 2 3 4) '(1 2 3 4)) |#
