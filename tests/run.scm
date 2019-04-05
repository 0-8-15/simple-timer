(cond-expand
 (chicken-4
  (require-library chicken srfi-18 extras ports)
  (require-library simple-timer))
 (else))

(module
 simple-timer-tests
 *
 (import (except scheme force delay))
 (cond-expand
  (chicken-4
   (import (except chicken promise?) srfi-18 ports extras)
   (import (only data-structures identity))
   (use matchable)
   (use test))
  (else
   (import
    (chicken base)
    (chicken port)
    (chicken condition)
    srfi-18 srfi-28
    matchable
    test)))
 (import simple-timer)

(define (dbg l v) (format (current-error-port) "D ~a ~s\n" l v) v)

(define (single-shot)
  (let ((mutex (make-mutex 'single-shot)))
    (mutex-lock! mutex #f #f)
    (values
     (let ((results #f))
       (let ((mutex mutex))
	 (lambda ()
	   (or results
	       (begin
		 (mutex-lock! mutex #f #f)
		 (set! results (mutex-specific mutex))
		 (mutex-unlock! mutex)
		 (set! mutex #f)
		 results)))))
     (lambda args
       (let ((try mutex))
	 (set! mutex #f) ;; fail on second call
	 (if try
	     (let ((handle
		    (lambda (proc args)
		      (match
		       args
		       (() proc)
		       ((x)
			(cond
			 ((eq? proc values) (lambda () x))
			 (else (lambda () (proc x)))))
		       (x (lambda () (apply proc x)))))))
	       (mutex-specific-set!
		try
		(match
		 args
		 (() values)
		 (((? procedure? proc) . args) (handle proc args))
		 ((#f ex)
		  (lambda () (raise ex)))
		 ((_ . args) (handle values args))))
	       (mutex-unlock! try)
	       #t)
	     #f))))))

(test-begin "simple-timer")

(test-group
 "adjusting timer environment"
 (let ((before (timer-period)))
   (timer-epsilon 0.01)
   (test "timer period settable" 0.01 (begin (timer-period 0.01) (timer-period)))
   (format #t "   ...waiting ~as for the new period becoming effective\n" before)
   (thread-sleep! before)))

(test-group
 "timers working"
 (test
  "catch timer-condition on current thread"
  #t
  (handle-exceptions
   ex
   (cond
    ((timer-condition? ex) #t)
    (else ex))
   (register-timer-task! 0.01 (current-thread))
   (thread-sleep! 0.03)
   'timer-exceptions-should-have-occurred-before))

 (test
  "timers invoked"
  '(#t #t)
  (let ((a #f) (b #f))
    (receive
     (got done) (single-shot)
     (register-timer-task! 0.03 (lambda () (set! a #t)))
     (register-timer-task! 0.03 (lambda () (set! b #t)))
     (register-timer-task! 0.05 (lambda () (done list a b)))
     ((got)))))

  (test
  "canceled timer not invoked"
  '(#t #f)
  (let ((a #f) (b #f))
    (receive
     (got done) (single-shot)
     (register-timer-task! 0.03 (lambda () (set! a #t)))
     (or (cancel-timer-task!
	  (register-timer-task! 0.03 (lambda () (set! b #t))))
	 (error "too late to cancel timer"))
     (register-timer-task! 0.04 (lambda () (done list a b)))
     ((got)))))

  )

(test-end)

(test-exit)

)
