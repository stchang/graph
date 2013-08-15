#lang racket

(require "../graph-weighted.rkt"
         "../graph-fns-maxflow.rkt"
         "../gen-graph.rkt")
(require rackunit)

(define g26.1
  (mk-weighted-graph/directed
   '((16 s v1)
     (13 s v2)
     (10 v1 v2)
     (4 v2 v1)
     (12 v1 v3)
     (14 v2 v4)
     (9 v3 v2)
     (7 v4 v3)
     (20 v3 t)
     (4 v4 t))))

(define flow26.1 (maxflow g26.1 's 't))
(define (validate-flow g f s t)
  (define (out v) 
    (for/sum ([(e flow) (in-hash f)] #:when (equal? v (first e))) flow))
  (define (in v) 
    (for/sum ([(e flow) (in-hash f)] #:when (equal? v (second e))) flow))
  (for ([(e flow) (in-hash f)])
    (check-true (<= flow (apply edge-weight g e))))
  (check-equal? (out s) (in t))   ;; s outgoing = t incoming
  ;; flow conservation
  (for ([v (in-vertices g)] #:unless (or (equal? v s) (equal? v t)))
    (check-equal? (in v) (out v))))

(validate-flow g26.1 flow26.1 's 't)

(check-equal? (for/sum ([(e flow) (in-hash flow26.1)] 
                        #:when (equal? 's (first e))) flow)
              23)