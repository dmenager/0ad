(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)
(ql:quickload :trivial-timers)

(defvar *thread-variables* '())

(defstruct node 
  (state-name 0 :type integer)
  (state '() :type list)
  (edges '() :type list))

; consider changing this to a hash table of states to action
(defstruct graph (nodes '() :type list))

(defstruct (mdpr (:include graph)) 
  (states '() :type list) 
  (actions '() :type list)  
  (gamma 0.0 :type short-float))

(defstruct edge  
  (to 0 :type integer)
  (option-name 'nil :type symbol)
  (probability 0.00 :type short-float))

(defstruct option 
  (name 'nil :type symbol)
  (policy '() :type list)
  (termination-conditions '() :type list))

(defstruct term-cond
  (state-name 'nil :type symbol)
  (termination-prob 0.00 :type short-float))

(defstruct state
  (name '() :type integer)
  (data '() :type list))

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
    (unwind-protect 
	 (progn
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
		   (let* ((*standard-output* std-out)
			  (timer (trivial-timers:make-timer 
				  #'(lambda ()
				      (format *standard-output* 
					      "Connection timeout. Closing ~A.~%" 
					      connection)
				      (usocket:socket-close connection)	 
				      (setf (second (nth cnt *thread-variables*)) '())
				      (setf (first (nth cnt *thread-variables*)) '())))))
		     
		     ; 5 minute timeout
		     (trivial-timers:schedule-timer timer (* 5 60))
		     
		     (loop while (not (null (second (nth cnt *thread-variables*)))) do
			  (handle-request stream cnt std-out timer)
			  (clear-input stream)))) 
		 :arguments (list *standard-output* thread-index))
		(incf count))))
      (progn
	; wait for data to implement message handlers before the next 2 lines
	;(format stream "Connection closed due to inactivity.~%")
	;(force-output stream)
	(clear-input stream)
	(clear-output stream)
	(usocket:socket-close socket)))))

#| Service a request from a client |#

; stream = socket stream to client
; t-idx = index of thread variables for thread
; ostream = reference to standard out
; timer = timeout timer 
(defun handle-request (stream t-idx ostream timer)
  (trivial-timers:schedule-timer timer (* 5 60))
  (let ((line (read-line stream nil 'the-end))
	(*standard-output* ostream))
    (setf (first (nth t-idx *thread-variables*)) t)
    (format *standard-output* "Handling request ~%")
    (format *standard-output* "You said: ~S~%" line))
  (force-output stream))


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
  
  ; create MDP/R
  (let ((mdp-r (make-mdpr :states '()
			  :actions '()
			  :nodes '())))
    
  ; fill state/action space
  (setf (mdpr-states mdp-r) (make-state-space '()))
  (setf (mdpr-actions mdp-r) (make-action-space '()))
  
  ; create transition probabilites
  (make-nodes mdp-r)
  ; create expert's feature expectations

  ;return reward function
  (format t "~A~%" mdp-r)))

#| Define the state space for the MDP |#

; states = state list to fill
(defun make-state-space (states)
  (with-open-file (client-data "testStates.txt"
			       :direction :input
			       :if-does-not-exist :error)
    (let ((count 0))
      
      (do ((line (read-line client-data nil) 
		 (read-line client-data nil)))
	  ((null line))
	(let ((list (to-syms line)))
	  (setq states (reverse (cons (make-state :name count :data list)
				      (reverse states)))))
	(incf count)))
    states))

#| Define action space for MDP |#

; actions = action list to fill
; TODO: Use proper termination condition probabilities
(defun make-action-space (actions)
  ;open server client file
  (with-open-file (client-data "testInit.txt"
			       :direction :input
			       :if-does-not-exist :error)
    
    (do ((line (read-line client-data nil) 
	       (read-line client-data nil)))
	((null line))
      (let* ((list (first (to-syms line)))
	     (op (make-option :name (first list) 
			      :policy (second list) 
			      :termination-conditions '())))
	(map '() 
	     #'(lambda (state)
		 (setf (option-termination-conditions op) 
		       (reverse (cons (make-term-cond :state-name state) 
			     (reverse (option-termination-conditions op))))))
	     (third list))
	(setq actions (reverse (cons op (reverse actions)))))))
  actions)

#| build transition graph |#

; mdpr = mdpr simulation
(defun make-nodes (mdpr)
  ; make a node for each state
  (let ((count 0))
    (map '() 
	 #'(lambda (s)
	       (setf (mdpr-nodes mdpr) 
		     (reverse (cons (make-node :state-name count :state (state-data s)) 
				    (reverse (mdpr-nodes mdpr)))))
	     (incf count))
	 (mdpr-states mdpr))))

#| Assign the transition probabilities for each node |#

; TODO: figure real transition probabilities
; TODO: (edge-to)
(defun make-transition-probs (mdpr))

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
  (/ (+ 0 (random (+ 1 (- 100 0)))) 100))

#| Simulate an action in the MDP-R|#

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
