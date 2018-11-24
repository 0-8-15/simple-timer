
(cond-expand
 (chicken-4 (require-library srfi-18 pigeon-hole llrb-tree lolevel))
 (else ))

(declare
 ;; This is not safe with interrupts disabled.  We rely use the cdr of
 ;; the TASK to communicate that a task is still active and may be
 ;; successfully removed.  If this was interrupted between reading the
 ;; cdr's value and writing over it, the user side might continue
 ;; believing the timer was canceled and still seeing it invoked.  The
 ;; spots are documented in the source.
 ;;
 ;; Instead of relying on disabled interrupts (which in itself might
 ;; be a dangerous assumption if we'd take posix signals into
 ;; account), we should better use low level code to ensure this
 ;; assumptions holds than enabling interrupts.
 ;;
 ;; Otherwise it's pointless here:
 (disable-interrupts)

 (no-bound-checks)
 (no-procedure-checks)
 (local)
 (inline)
 (safe-globals)
 (specialize)
 (strict-types))

(module
 simple-timer
 (timer-condition?
  make-timer-condition
  ;;
  timer-period ;; global variable with parameter-alike interface
  timer-epsilon ;; global variable with parameter-alike interface
  register-timer-task! ;; schedule job at time, return opaque reference to scheduled task
  cancel-timer-task! ;; cancel a reference from register-timer-task!
  ;; undocumented, possibly pointless
  start-timer-entry!
  )
 (import (except scheme force delay))
 (cond-expand
  (chicken-4
   (import chicken))
  (else
   (import
    (chicken type)
    (except (chicken base))
    srfi-12
    (chicken time))))
 (import srfi-18)
 (import llrb-tree)

(import (prefix pigeon-hole q:))

(define-record simple-timeout)

(define %simple-timeout (make-simple-timeout))

(define (make-timer-condition) %simple-timeout)

(define (timer-condition? x) (eq? x %simple-timeout))

(include "timeout.scm")

)
