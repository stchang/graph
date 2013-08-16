#lang racket
(require "../graph-weighted.rkt"
         "../graph-fns-allpairs-shortestpaths.rkt"
         "test-utils.rkt")

(require rackunit)


;; all pairs shortest paths tests ------------------------------------------

(define g25.1
  (mk-weighted-graph/directed
   '((3 1 2) (-4 1 5) (8 1 3)
     (7 2 5) (1 2 4)
     (4 3 2)
     (-5 4 3) (2 4 1)
     (6 5 4))))

(define weights25.1slow (all-pairs-shortest-paths/slow g25.1))
(define weights25.1faster (all-pairs-shortest-paths/faster g25.1))

(check-equal? weights25.1slow weights25.1faster)

(hash-values-inexact->exact weights25.1slow)

(check-equal?
 weights25.1slow
 (make-hash '(((1 1) . 0)
              ((1 2) . 1)
              ((1 3) . -3)
              ((1 4) . 2)
              ((1 5) . -4)
              ((2 1) . 3)
              ((2 2) . 0)
              ((2 3) . -4)
              ((2 4) . 1)
              ((2 5) . -1)
              ((3 1) . 7)
              ((3 2) . 4)
              ((3 3) . 0)
              ((3 4) . 5)
              ((3 5) . 3)
              ((4 1) . 2)
              ((4 2) . -1)
              ((4 3) . -5)
              ((4 4) . 0)
              ((4 5) . -2)
              ((5 1) . 8)
              ((5 2) . 5)
              ((5 3) . 1)
              ((5 4) . 6)
              ((5 5) . 0))))

(define g25.2
  (mk-weighted-graph/directed
   '((-1 1 5)
     (1 2 1) (2 2 4)
     (2 3 2) (-8 3 6)
     (-4 4 1) (3 4 5)
     (7 5 2)
     (5 6 2) (10 6 3))))

(define weights25.2slow (all-pairs-shortest-paths/slow g25.2))
(define weights25.2faster (all-pairs-shortest-paths/faster g25.2))

;; something wrong with all-pairs-shortest-paths/faster
;(check-equal? weights25.2slow weights25.2faster)

(define weights25.1fw (floyd-warshall g25.1))

(hash-values-inexact->exact weights25.1fw)
(check-equal? weights25.1slow weights25.1fw)

(define weights25.2fw (floyd-warshall g25.2))

(hash-values-inexact->exact weights25.2slow)
(hash-values-inexact->exact weights25.2fw)

(check-equal? weights25.2slow weights25.2fw)

(require "../graph-unweighted.rkt")
(define g25.5
  (mk-unweighted-graph/directed '((2 3) (2 4) (3 2) (4 3) (4 1))))

(check-equal? 
 (transitive-closure g25.5)
 (make-hash '(((1 1) . #t)
              ((1 2) . #f)
              ((1 3) . #f)
              ((1 4) . #f)
              ((2 1) . #t)
              ((2 2) . #t)
              ((2 3) . #t)
              ((2 4) . #t)
              ((3 1) . #t)
              ((3 2) . #t)
              ((3 3) . #t)
              ((3 4) . #t)
              ((4 1) . #t)
              ((4 2) . #t)
              ((4 3) . #t)
              ((4 4) . #t))))


;; johnson --------------------------------------------------------------------
(define weights25.1johnson (johnson g25.1))

(check-equal? weights25.1johnson weights25.1fw)