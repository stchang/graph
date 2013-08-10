#lang racket

(provide (all-defined-out))

(define (lists->sets lsts) (apply set (map (λ (lst) (apply set lst)) lsts)))
(define (hash-values-inexact->exact h)
  (for ([v (in-hash-keys h)]) 
    (hash-update! h v (λ (v) (if (equal? +inf.0 v) v (inexact->exact v))))))