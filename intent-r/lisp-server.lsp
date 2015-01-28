(ql:quickload :usocket)
(ql:quickload :trivial-timers)

(defvar *thread-variables* '())

(defvar *atomic-actions* '((:action attack :with '() :who '()) 
			   (:action barter :who '()) 
			   (:action create :what '()) 
			   (:action escort :who '()) 
			   (:action gather :what '()) 
			   (:action graze :what '()) 
			   (:action heal :who '()) 
			   (:action lock :what '()) 
			   (:action loot :what '()) 
			   (:action move :who '()) 
			   (:action patrol :what '()) 
			   (:action repair :what '()) 
			   (:action scout :what '()) 
			   (:action trade :what '() :who '())))

(defvar *entities* '((:name 'swordsman :type 'infantry :owner '())
		     (:name 'spearman :type 'infantry :owner '())
		     (:name 'javelinist :type 'infantry :owner '())
		     (:name 'archer :type 'infantry :owner '())
		     (:name 'slinger :type 'infantry :owner '())
		     (:name 'swordsman :type 'cavalry :owner '())
		     (:name 'spearman :type 'cavalry :owner '())
		     (:name 'javelinist :type 'cavalry :owner '())
		     (:name 'archer :type 'cavalry :owner '())
		     (:name 'female :type 'support :owner '())
		     (:name 'trader :type 'support :owner '())
		     (:name 'healer :type 'support :owner '())
		     (:name 'onager :type 'siege :owner '())
		     (:name 'ballista :type 'siege :owner '())
		     (:name 'ram :type 'siege :owner '())
		     (:name 'merchant-ship :type 'ship :owner '())
		     (:name 'light-warship :type 'ship :owner '())
		     (:name 'medium-warship :type 'ship :owner '())
		     (:name 'heavy-warship :type 'ship :owner '())
		     (:name 'animals :type 'herd)
		     (:name 'animals :type 'hunt)
		     (:name 'tree :type 'resource)
		     (:name 'stone :type 'resource)
		     (:name 'ore :type 'resource)
		     (:name 'civic-centre :type 'structure :owner '())
		     (:name 'house :type 'structure :owner '())
		     (:name 'farmstead :type 'structure :owner '())
		     (:name 'field :type 'structure :owner '())
		     (:name 'corral :type 'structure :owner '())
		     (:name 'mill :type 'structure :owner '())
		     (:name 'outpost :type 'structure :owner '())
		     (:name 'pallisade-wall :type 'structure :owner '())
		     (:name 'pallisade-joint :type 'structure :owner '())
		     (:name 'pallisade-gate :type 'structure :owner '())
		     (:name 'dock :type 'structure :owner '())
		     (:name 'market :type 'structure :owner '())
		     (:name 'barracks :type 'structure :owner '())
		     (:name 'temple :type 'structure :owner '())
		     (:name 'defense-tower :type 'structure :owner '())
		     (:name 'wall :type 'structure :owner '())
		     (:name 'wall-turret :type 'structure :owner '())
		     (:name 'city-gate :type 'structure :owner '())
		     (:name 'commercial-port :type 'structure :owner '())
		     (:name 'naval-shipyard :type 'structure :owner '())
		     (:name 'embassy :type 'structure :owner '())))

(defstruct node 
  (state-name 0 :type integer)
  (state '() :type list)
  (edges '() :type list))

; consider changing this to a hash table of states to action
(defstruct graph 
  (nodes '() :type list)
  (cur-state 0 :type integer)
  (start-state 0 :type integer))

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

(defstruct action
  (sub-actions '() :type list))

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
	(usocket:socket-close socket)))))

#| Service a request from a client |#

; stream = socket stream to client
; t-idx = index of thread variables for thread
; ostream = reference to standard out
; timer = timeout timer 
(defun handle-request (stream t-idx ostream timer)
  (trivial-timers:schedule-timer timer (* 5 60))
  (with-open-file (clientData (concatenate 'string 
					   "clientData/clientData"
					   (write-to-string t-idx) ".txt")
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)
    (let ((line (read-line stream nil 'the-end))
	  (*standard-output* ostream))
      (setf (first (nth t-idx *thread-variables*)) t)
      (format *standard-output* "Handling request ~%")
      (format *standard-output* "You said: ~S~%" line)
      (format clientData "You said: ~S~%" line))
    (force-output stream)
    (force-output clientData)))


(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))

#| Initialize MDP and expert's feature expectations |#

; init-file = states, transition probabilities, m trajectories
; return reward function   
(defun init-apprentice (init-file)
  
  ; create MDP/R
  (let ((mdp-r (make-mdpr :states '()
			  :actions '()
			  :nodes '()
			  :cur-state 0
			  :start-state 0)))
    
  
    ; update :owner for each player in game
    ; fill state/action space
    (setf (mdpr-states mdp-r) (make-state-space '()))
    (setf (mdpr-actions mdp-r) (subsets *atomic-actions* (mdpr-actions mdp-r)))
  
    ; create transition probabilites
    (make-nodes mdp-r)
    ; create expert's feature expectations

    ;return reward function
    mdp-r
    
    ;(discover-reward mdp-r '() '())
  ))

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

#| Define action space for MDP...not in use! |#

; actions = action list to fill
; TODO: Use proper termination condition probabilities
(defun make-action-space-2 (actions)
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

#| Define action space for MDP |#

; list = list of atomic actions
(defun make-action-space (actions)
  (let ((set (remove-if-not #'(lambda (action)
				(getf action :with)) 
			    actions))
	(applicable (remove-if-not  
		     #'(lambda (entity)
			 (or (eq 'infantry (eval (getf entity :type))) 
			     (eq 'cavalry (eval (getf entity :type)))
			     (eq 'siege (eval (getf entity :type)))
			     (eq 'support (eval (getf entity :type)))
			     (eq 'ship (eval (getf entity :type)))))
		     *entities*)))

    (map '() 
	 #'(lambda (x) 
		 (setq *atomic-actions* 
		       (remove x *atomic-actions* :test #'equal)))
	 set)
   
    (map '() 
	 #'(lambda (action)
	     (map '() 
		  #'(lambda (entity)
		      (format t "~S~%" entity)		      
		       (setf (getf action :with) entity)
		       (push action *atomic-actions*))
		  applicable))
	 set)
    *atomic-actions*))
    ;(subsets *atomic-actions* list)))

(defun subsets (list into)
  (let ((len (length list)))
    (do ((counter 0 (1+ counter)))
	((>= counter len))
      (do ((counter2 counter (1+ counter2)))
	  ((>= counter2 len))
	(setq into
	      (reverse (cons (sublist list counter counter2)
			     into)))))
    into))
(defun sublist (list start end)
  (if (or (null list) (> start (length list)) (> end (length list)) (> start end))
      '()
      (let ((sub (loop for x from start to end
		    collect (nth x list))))
	sub)))

#| build transition graph |#

; mdpr = mdpr simulation
(defun make-nodes (mdpr)
  ; make a node for each state
  (let ((count 0))
    (map 'list 
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
	    (setf (gethash (state-name state) p) 
		  (nth (+ 1 (random (length (mdpr-actions mdpr)))) 
		       (mdpr-actions mdpr))))
	 (mdpr-states mdpr))
    (format t "Pi: ~A~%" p)))
    ;(mu p)))

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
; mdpr = mdpr
; returns state
(defun act (p mdpr) 
  )


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

(format ostream "Using count: ~d~%" t-idx) 

; Filling action space
(remove-if-not #'(lambda (x)
			    (getf x :a)) '((:a 1) (:b 2)))
; modifying plist
(let ((x '(:a 1 :b 2)))
	   (setf (getf x :a) 'artist)
	   x)|#
