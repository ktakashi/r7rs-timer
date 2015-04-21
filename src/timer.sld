;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; timer.sld - R7RS+SRFI portable timer API
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; this library requires SRFi-18, 19, 69 and 114
(define-library (timer)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 18)
	  (srfi 19)
	  (srfi 69)
	  (srfi 114))
  ;; APIs
  (export make-timer timer?
	  timer-cancel!
	  timer-schedule! timer-reschedule!
	  timer-task-remove! timer-task-exists?)
  (begin
    ;; priority queue
    ;; hope it'll soon enough be a SRFI.
    ;; for now very naive implementation
    (define-record-type <priority-queue>
      (make-raw-priority-queue comparator storage size)
      priority-queue?
      (comparator priority-queue-comparator)
      (storage    priority-queue-storage)
      ;; queue and storage size.
      (size       priority-queue-size priority-queue-size-set!))
     
    (define (make-priority-queue comparator)
      ;; storage can be destructively modified
      ;; pre allocate vector storage
      (make-raw-priority-queue comparator (list (vector 10 #f)) 0))

    ;; we define only what we need and very naive way
    ;; helper, R7RS even doesn't have any sorting procedure...
    ;; i'm lazy to implement fancy algorithm so use quicksort
    (define (vector-sort! comp vec end)
      (define (quicksort comp vec lo hi)
	(define (partition comp vec lo hi)
	  (define (swap! vec a b)
	    (let ((t (vector-ref vec b)))
	      (vector-set! vec b (vector-ref vec a))
	      (vector-set! vec a t)))
	  ;; lazy enough to choose pivot. so do with following assumption
	  ;; - input vector is already sorted but the last element
	  ;; - thus middle is the median value
	  (let* ((pi (quotient (+ hi lo) 2))
		 (pv (vector-ref vec pi))
		 (end (- hi 1)))
	    (swap! vec pi end) 
	    (let loop ((i lo) (si lo))
	      (if (= i hi)
		  (begin (swap! vec si end) si)
		  (loop (+ i 1)
			(cond ((comp (vector-ref vec i) pv)
			       (swap! vec i si)
			       (+ si 1))
			      (else si)))))))
	(if (< lo hi)
	    (let ((pivot (partition comp vec lo hi)))
	      (quicksort comp vec lo pivot) ;; hi is exclusive
	      (quicksort comp vec (+ pivot 1) hi))
	    vec))
      (quicksort comp vec 0 end))
    ;; this still only takes O(log N)
    (define (priority-queue-push! pq obj)
      (let ((storage (priority-queue-storage pq))
	    (size    (priority-queue-size pq)))
	(if (= (vector-length (car storage)) size)
	    ;; re-allocate it
	    (let ((new-vec (make-vector (+ size 10))))
	      (vector-copy! new-vec 0 (car storage) 0 size)
	      (set-car! storage new-vec)
	      ;; retry
	      (priority-queue-push! pq obj))
	    (let ((vec (car storage))
		  (new-size (+ size 1))
		  (comparator (priority-queue-comparator pq)))
	      (vector-set! vec size obj)
	      (priority-queue-size-set! pq new-size)
	      ;; we do this with reverse order so that
	      ;; we just need to reduce the size when we need to pop
	      (vector-sort! (lambda (a b)
			      (> (comparator-compare comparator a b) 0))
			    vec new-size)))))
    ;; O(1)
    (define (priority-queue-pop! pq)
      (let ((storage (priority-queue-storage pq))
	    (size    (priority-queue-size pq)))
	(let ((v (vector-ref (car storage) (- size 1))))
	  (priority-queue-size-set! pq (- size 1))
	  v)))
    ;; O(1)
    (define (priority-queue-min pq)
      (let ((storage (priority-queue-storage pq))
	    (size    (priority-queue-size pq)))
	(vector-ref (car storage) (- size 1))))
    ;; O(N)
    (define (priority-queue-remove! pq obj)
      (define (set-mark! vec size comparator)
	(let loop ((i 0))
	  (cond ((= i size) (error "queue doesn't contain the given value" obj))
		((comparator-equal? comparator (vector-ref vec i) obj)
		 (vector-set! vec i #f)
		 i)
		(else (loop (+ i 1))))))
      (let ((storage (priority-queue-storage pq))
	    (size    (priority-queue-size pq))
	    (comparator (priority-queue-comparator pq)))
	(let ((index (set-mark! (car storage) size comparator)))
	  (vector-copy! (car storage) index (car storage) (+ index 1) size)
	  (priority-queue-size-set! pq (- size 1)))))
    (define (priority-queue-empty? pq) (zero? (priority-queue-size pq)))

    ;; timer task
    (define-record-type <timer-task>
      (make-raw-timer-task id thunk next period running?)
      timer-task?
      (id     	timer-task-id)
      (thunk  	timer-task-thunk)
      (next   	timer-task-next timer-task-next-set!) ;; time object
      (period 	timer-task-period timer-task-period-set!)
      (running? timer-task-running? timer-task-running-set!))
    (define (make-timer-task id thunk next period)
      (make-raw-timer-task id thunk next period #f))

    (define task-comparator
      (let ()
	(define (task=? a b)
	  (time=? (timer-task-next a) (timer-task-next b)))
	(define (task<? a b)
	  (time=? (timer-task-next a) (timer-task-next b)))
	(make-comparator timer-task? task=?
			 (make-comparison=/< task=? task<?)
			 #f)))

    ;; timer
    (define-record-type <timer>
      (make-raw-timer queue done? lock waiter worker next-id active)
      timer?
      (queue   timer-queue)
      (done?   timer-done? timer-done-set!)
      (lock    timer-lock)
      (waiter  timer-waiter)
      (worker  timer-worker timer-worker-set!)
      (next-id timer-next-id timer-next-id-set!)
      ;; task table
      (active  timer-active))
    (define (%make-timer)
      (make-raw-timer (make-priority-queue task-comparator)
		      #f
		      (make-mutex)
		      (make-condition-variable)
		      #f 1 (make-hash-table eqv?)))

    ;; utility macro to wait condition variable
    (define-syntax wait-cv
      (syntax-rules ()
	((_ mutex cv)
	 (wait-cv mutex cv #f))
	((_ mutex cv timeout)
	 (let ((m mutex)
	       (c cv)
	       (to timeout))
	   (when (mutex-unlock! m c to)
	     (mutex-lock! mutex))))))

    (define (milliseconds->sec&nano msec)
      (let ((sec (quotient msec 1000))
	    (nsec (* (modulo msec 1000) 1000000)))
	(values sec nsec)))

    (define default-error-handler raise)
    (define make-timer
      (case-lambda
       (() (make-timer default-error-handler))
       ((error-handler)
	(define (timer-start! t)
	  (define (main-loop t)
	    (unless (timer-done? t)
	      (let ((queue (timer-queue t)))
		(if (priority-queue-empty? queue)
		    (wait-cv (timer-lock t) (timer-waiter t))
		    (let* ((first (priority-queue-min queue))
			   (now   (current-time))
			   (next  (timer-task-next first)))
		      (if (time>=? now next)
			  (let ((first (priority-queue-pop! queue)))
			    (timer-task-running-set! first #t)
			    (mutex-unlock! (timer-lock t))
			    (guard (e (else (error-handler e)))
			      ((timer-task-thunk first)))
			    (mutex-lock! (timer-lock t))
			    (if (timer-task-running? first)
				(let ((p (timer-task-period first)))
				  (timer-task-running-set! first #f)
				  (if (and (time? p)
					   (or (positive? (time-nanosecond p))
					       (positive? (time-second p))))
				      (let ((next (add-duration next p)))
					(timer-task-next-set! first next)
					(priority-queue-push! queue first))
				      (hash-table-delete!
				       (timer-active t)
				       (timer-task-id first))))
				(hash-table-delete! (timer-active t)
						   (timer-task-id first))))
			  (wait-cv (timer-lock t) (timer-waiter t)
				   (timer-task-next first))))))
	      (main-loop t)))
	  (lambda ()
	    (dynamic-wind
		(lambda () (mutex-lock! (timer-lock t)))
		(lambda () (main-loop t))
		(lambda () (mutex-unlock! (timer-lock t))))))
	(let ((t (%make-timer)))
	  (timer-worker-set! t (thread-start! (make-thread (timer-start! t))))
	  t))))

    (define (timer-cancel! t)
      (mutex-lock! (timer-lock t))
      (timer-done-set! t #t)
      (condition-variable-broadcast! (timer-waiter t))
      (mutex-unlock! (timer-lock t))
      (thread-join! (timer-worker t)))

    (define (check-positive who v msg)
      (when (negative? v) (error who msg v)))
    (define (millisecond->time-duration msec)
      (let-values (((sec nsec) (milliseconds->sec&nano msec)))
	(make-time time-duration nsec sec)))
    (define (current-time+millisecond msec)
      (let ((t (current-time)))
	(if (zero? msec)
	    t
	    (add-duration t (millisecond->time-duration msec)))))

    (define (check-period who period)
      (or (and (number? period) (check-positive who period "negative period"))
	  (and (time? period) (eq? (time-type period) time-duration))
	  (error who "positive or time-duration is required" period)))

    (define timer-schedule!
      (case-lambda
       ((timer thunk first) (timer-schedule! timer thunk first 0))
       ((timer thunk first period)
	
	(define (allocate-timer-id timer)
	  (let ((c (timer-next-id timer)))
	    (timer-next-id-set! timer (+ c 1))
	    c))
	(define (check v msg) (check-positive 'timer-schedule! v msg))
	(unless (time? first) (check first "negative delay"))
	(check-period 'timer-schedule! period)

	(mutex-lock! (timer-lock timer))
	(let* ((id (allocate-timer-id timer))
	       (first (if (time? first) first (current-time+millisecond first)))
	       (p    (cond ((time? period) period)
		     ((zero? period) period)
		     (else (millisecond->time-duration period))))
	       (task (make-timer-task id thunk first p)))
	  (hash-table-set! (timer-active timer) id task)
	  (priority-queue-push! (timer-queue timer) task)
	  (condition-variable-broadcast! (timer-waiter timer))
	  (mutex-unlock! (timer-lock timer))
	  id))))

    (define timer-reschedule!
      (case-lambda
       ((timer id first) (timer-schedule! timer id first 0))
       ((timer id first period)
	
	(define (check v msg) (check-positive 'timer-reschedule! v msg))
	(unless (time? first) (check first "negative delay"))
	(check-period 'timer-reschedule! period)

	(let ((lock (timer-lock timer)))
	  (mutex-lock! lock)
	  (let ((task (hash-table-ref/default (timer-active timer) id #f)))
	    ;; task has next
	    (when task
	      (let ((old (timer-task-next task))
		    (next (if (time? first)
			      first
			      (current-time+millisecond first)))
		    (p    (cond ((time? period) period)
				((zero? period) period)
				(else (millisecond->time-duration period))))
		    (queue (timer-queue timer)))
		;; should be able to delete here...
		(priority-queue-remove! queue task)
		;; update period
		(timer-task-period-set! task p)
		(timer-task-next-set! task next)
		;; now reschedule it
		(priority-queue-push! queue task)
		;; let them know
		(condition-variable-broadcast! (timer-waiter timer))))
	    (mutex-unlock! lock)))
	id)))

    (define (timer-task-remove! timer id)
      (let ((lock (timer-lock timer)))
	(mutex-lock! lock)
	(let ((task (hash-table-ref/default (timer-active timer) id #f)))
	  (cond ((not task) (mutex-unlock! lock) #f)
		(else
		 (if (timer-task-running? task)
		     (timer-task-running-set! task #f)
		     (priority-queue-remove! (timer-queue timer) task))
		 (hash-table-delete! (timer-active timer) id)
		 (condition-variable-broadcast! (timer-waiter timer))
		 (mutex-unlock! lock)
		 #t)))))

    (define (timer-task-exists? timer id)
      (let ((lock (timer-lock timer)))
	(mutex-lock! lock)
	(let ((r (hash-table-exists? (timer-active timer) id)))
	  (mutex-unlock! lock)
	  r)))

    )
  
  )
