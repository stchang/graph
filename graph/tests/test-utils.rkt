#lang racket

(require "../main.rkt")
(require rackunit)

(provide (all-defined-out))

(define (lists->sets lsts) (apply set (map (λ (lst) (apply set lst)) lsts)))
(define (hash-values-inexact->exact h)
  (for ([v (in-hash-keys h)]) 
    (hash-update! h v (λ (v) (if (equal? +inf.0 v) v (inexact->exact v))))))

;; tsort utils ----------------------------------------------------------------
(define (check-tsorted g tsorted)
  (for ([e (in-edges g)])
    (define rst (cdr (member (first e) tsorted)))
    (check-false (null? (member (second e) rst)))))


(define (do-tsort-tests g)
  (check-true (dag? g))
  (define tsorted (tsort g))
  (check-tsorted g tsorted))