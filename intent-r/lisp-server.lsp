(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)

(defstruct mdpr 
  (states '() :type list) 
  (actions '() :type list) 
  (t-graph :include graph) 
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
  (force-output stream))

(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))

#| Initialize MDP and expert's feature expectations|#

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

#|Discovers reward function|#

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

#|Compute feature expectation of policy, pi|#

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
