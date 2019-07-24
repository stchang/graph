#lang racket

(require "../main.rkt")
(require rackunit)

(provide (all-defined-out))

(define (lists->sets lsts) (apply set (map (λ (lst) (apply set lst)) lsts)))
(define (hash-values-inexact->exact h)
  (for ([v (in-hash-keys h)]) 
    (hash-update! h v (λ (v) (if (equal? +inf.0 v) v (inexact->exact v))))))
(define-syntax-rule (check-hash-equal?/exact h1 h2)
  (for ([(k v) (in-hash h1)])
    (if (equal? +inf.0 v)
        (check-equal? +inf.0 (hash-ref h2 k))
        (check-equal? (inexact->exact v)
                      (inexact->exact (hash-ref h2 k))))))

;; check sequences for equality, treating them as sets
;; (assumes no duplicates)
(define-syntax-rule (check-seqs-as-equal-sets? s1 s2)
  (check-equal? (apply set (sequence->list s1)) 
                (apply set (sequence->list s2))))

(define-syntax-rule (check-expected-msts to-check expected ...)
  (let ([res to-check])
    (check-true (or (equal? (lists->sets res) (lists->sets expected)) ...))))
                    

;; tsort utils ----------------------------------------------------------------
(define (check-tsorted g tsorted)
  (for ([e (in-edges g)])
    (define rst (cdr (member (first e) tsorted)))
    (check-false (null? (member (second e) rst)))))


(define (do-tsort-tests g)
  (check-true (dag? g))
  (define tsorted (tsort g))
  (check-tsorted g tsorted))