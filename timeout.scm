
(define %timer-period 1)

(define (timer-period . arg)
  (if (pair? arg)
      (let ((old %timer-period)
	    (new (car arg)))
	(if (number? new)
	    (begin
	      (set! %timer-period new)
	      old)
	    (error "illegal timer period" new)))
      %timer-period))

(define-constant ms/s 1000.0)
(define %tmo-epsilon 0.5) ;; this used be be a define-constant for efficiency
;;
;; However it seems to be important to export it if %timer-period is configurable.
(define (timer-epsilon . arg)
  (if (pair? arg)
      (let ((old %tmo-epsilon)
	    (new (car arg)))
	(if (number? new)
	    (begin
	      (set! %tmo-epsilon new)
	      old)
	    (error "illegal timer epsilon" new)))
      %tmo-epsilon))

(define-inline (intern-timer-time x) x)

(define-inline (tmo-current-time) (/ (current-milliseconds) ms/s))

(define-inline (make-timer-treetype)
  (make-llrb-treetype
   #f ;; KEY?
   (lambda (k p) (< (abs (- k p)) %tmo-epsilon)) ;; EQUAL
   (lambda (k p) (< k p)) ;; LESS
   ))

(define long-running (make-table (make-timer-treetype)))

(define-inline (execute-timer-job! entry)
  ;; (format (current-error-port) "Timer job ~a\n" job)
  (and-let*
   ((job (timer-entry-job entry)))
   ;; BEWARE: relying on interrupts disabled for cancelations not to
   ;; find the job active while we are busy actually executing it.
   (set-cdr! entry #f)
   (cond
    ((thread? job)
     (let ((state (thread-state job)))
       (case state
	 ((dead terminated))
	 (else (thread-signal! job %simple-timeout)))))
    ((q:isa? job) (q:send/anyway! job %simple-timeout))
    ;; BEWARE: job must be sure to never run into an exception!
    ((procedure? job) (job)))))

(define make-timer-entry cons)

(define (timer-entry? x) (and (pair? x) (cdr x)))

(define (timer-entry-delay x) (car x))
(define (timer-entry-job x) (cdr x))

(define (cancel-timer-task! x)
  ;; relying on interrupts disabled here; may not be interrupted
  ;; between reading cdr and setting it.
  (and-let*
   (((pair? x))	;; timer-entry?
    (task (cdr x)))
   (set-cdr! x #f)
   task))

(define current-timer-queue (q:make 'timer-queue capacity: 10))

(define (start-timer-entry! entry)
  (and (timer-entry? entry) (q:send/anyway! current-timer-queue entry)))

(define (register-timer-task! time object)
  (let ((entry (make-timer-entry time object)))
    (q:send/anyway! current-timer-queue entry)
    entry))

(define-inline (handle-timer-entry! last now entry)
  (and-let*
   ((job (timer-entry-job entry))
    (delta (timer-entry-delay entry)))
   (let ((due (intern-timer-time (+ last delta))))
     ;;(format (current-error-port) "Timer job ~a last ~a now ~a due ~a delta ~a\n" job last now due delta)
     (if (> due now)
	 (table-update! long-running due (lambda (x) (cons entry x)) (lambda () '()))
	 (execute-timer-job! entry)))))

(define (timer-handler)
  (let again ((last (intern-timer-time (tmo-current-time)))
	      (last-entries '()))
    ;;(format (current-error-port) "Timer handler\n")
    (thread-sleep! %timer-period)
    (let ((now (intern-timer-time (tmo-current-time))))
      (for-each
       (lambda (e) (handle-timer-entry! last now e))
       last-entries)
      (let loop ()
	(receive
	 (due entries) (table-min long-running (lambda () (values #f #f)))
	 (if (and due (< due now))
	     (begin
	       (table-delete! long-running due)
	       (for-each execute-timer-job! entries)
	       (loop)))))
      (again now (q:receive-all! current-timer-queue)))))

(thread-start! timer-handler)
