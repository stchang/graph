#lang racket/base
(require "gen-queue.rkt")
(require (prefix-in r: data/heap))
(provide (all-defined-out))

;; priority queue such that on dequeue of element x,
;; pop all copies of x from the queue

(struct priority (elements)
  #:methods gen:queue
  [(define (enqueue! pq x) (r:heap-add! (priority-elements pq) x))
   (define (peek pq) (r:heap-min (priority-elements pq)))
   (define (dequeue! pq) 
     (define hp (priority-elements pq))
     (define x (r:heap-min hp))
     (begin0 x
             (let loop ()
               (r:heap-remove-min! hp)
               (when (and (not (zero? (r:heap-count hp))) 
                          (equal? (r:heap-min hp) x))
                 (loop)))))
   (define (empty? pq) (zero? (r:heap-count (priority-elements pq))))
   (define (in-queue pq) ; consumes the pq
     (make-do-sequence
      (λ ()
       (values
        dequeue! ; pos->element
        values   ; next-pos (dequeue! already removed element, so this is just id)
        pq       ; pos: use entire queue as pos
        (λ (x) (not (empty? pq))) ; continue-with-pos?
        #f #f
        ))))])
     
(define (mk-empty-priority <=) (priority (r:make-heap <=)))