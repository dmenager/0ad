(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)

(defvar *thread-variables* '())

(defstruct mdpr 
  (states '() :type list) 
  (actions '() :type list) 
  ;(t-graph :include graph) 
  (gamma 0.0 :type short-float))

; consider changing this to a hash table of states to action
(defstruct graph (nodes '() :type list))

(defstruct node 
  (name 0 :type integer)
  (value '() :type list)
  (edges '() :type list))

(defstruct edge  
  (to 0 :type integer)
  (probability 0.00 :type short-float))

(defun tcp-test-client (port)
  (setq conn (usocket:socket-connect usocket:*wildcard-host* port)))

(defun tcp-test-send ()
  (format (usocket:socket-stream conn) "Hello World!~%")
  (force-output (usocket:socket-stream conn)))

(defun tcp-server (port) 
  ; clear residual thread variables
  (setq *thread-variables* '())
  (let ((socket (usocket:socket-listen usocket:*wildcard-host*
				       port
				       :reuse-address t))
	(count 0))
    (loop
       (format *standard-output* "Waiting for input on ~A~%" socket)
       (usocket:wait-for-input socket)
       (let* ((connection (usocket:socket-accept socket))
	      (stream (usocket:socket-stream connection))
	      (thread-index count))
	 (format *standard-output* "Connection made to client ~%")
	 (setq *thread-variables* 
	       (append *thread-variables* (list (list '() t))))
	 
	 ; handle the request on thread and let main accept new clients
	 (sb-thread:make-thread 
	  (lambda(std-out cnt)
	    (let ((*standard-output std-out))
              ; check connection timeout
	      (sb-thread:make-thread 
	       (lambda(std-output cnt)
		 (let ((*standard-output* std-output))
		   (connection-timeout connection cnt std-output))) 
	       :arguments (list std-out thread-index))
	      
	      (loop while (not (null (second (nth cnt *thread-variables*)))) do
		   (handle-request stream cnt std-out)
		   (clear-input stream))
	      (sb-thread:return-from-thread '()))) 
	  :arguments (list *standard-output* thread-index))
	 (incf count)))))

#| Service a request from a client |#

; stream = socket stream to client
; t-v = thread variables
; t-idx = index of thread variables for thread
; ostream = reference to standard out 
(defun handle-request (stream t-idx ostream)
  (let ((line (read-line stream nil 'the-end))
	(*standard-output* ostream))
    (setf (first (nth t-idx *thread-variables*)) t)
    (format *standard-output* "Handling request ~%")
    (format *standard-output* "You said: ~S~%" line))
  (force-output stream))

#| Watch for timeouts |#

; connection = socekt connection
; t-v = thread variables
; t-idx = index of thread variables for thread
; ostream = reference to standard out
(defun connection-timeout (connection t-idx ostream)
  (block looper    
    (loop
       ; wait a minute!
       (sleep 60)
       ;when the connection hasn't been used for a minute, close it
       (when (eq (first (nth t-idx *thread-variables*)) '())
	 (format ostream "Connection timeout. Closing ~A.~%" connection)
	 (usocket:socket-close connection)	 
	 (setf (second (nth t-idx *thread-variables*)) '())
	 (return-from looper))
       (setf (first (nth t-idx *thread-variables*)) '()))
    (sb-thread:return-from-thread '())))

(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))

#| Initialize MDP and expert's feature expectations |#

; init-file = states, actions, transition probabilities, m trajectories
; return reward function   
(defun init-apprentice (init-file)
  ; fill state space
  
  ; fill action space
  
  ; create transition probabilites
  
  ; create MDP/R
  (let ((mdp-r (make-mdpr :states *STATES*
			  :actions *ACTIONS*
			  :t-graph (make-graph :nodes (make-node)))))

  ; create expert's feature expectations

  ;return reward function
  ))

#| Discovers reward function |#

; mdpr = mdp w/o reward funtion -- simulator
; phi = feature mapping -- (state -> state features)
; mue = expert's feature expectations
; returns reward
(defun discover-reward (mdpr phi mue)
  
  (let ((p (make-hash-table)))
    ;add elements to p
    (map '() 
	 #'(lambda (state)	    
	    (setf (gethash state p) 
		  (nth (+ 1 (random (length (mdpr-actions mdpr))))) 
		       (mdpr-actions mdpr)))
	 *STATES*)
    (mu p)))

#| Compute feature expectation of policy, pi |#

; mdpr = MDP simulator
; p = policy
; returns feature expectations of p
(defun mu (mdpr p)
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
  
#| Get a random percent |#

(defun random-percent ()
  (/ (+ 0 (random (+ 1 (- 1 0))))) 100)

; p = policy
; returns state
(defun act (p) )


#| SCRATCH 

; test I did for mu functionality
(mapcar #'(lambda (&rest x)
		     (reduce #'+ x)) '(1 2 3 4) '(1 2 3 4))

; server printfs
(format *standard-output* 
		 "THREAD VARIABLES ~A~%COUNT ~d~%" 
		 *thread-variables* 
		 thread-index)

(format ostream "conn-timeout: Thread Variables ~A~%conn-timeout: t-idx ~d~%" 
	    *thread-variables*
	    t-idx)

(format ostream "Using count: ~d~%" t-idx) |#
