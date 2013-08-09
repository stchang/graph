#lang racket
(require "../graph-weighted.rkt"
         "../graph-fns-singlesource-shortestpath.rkt"
         "../graph-fns-basic.rkt"
         "test-utils.rkt")

(require rackunit)


;; single-source shortest path tests ------------------------------------------

;; neg weight cycle
(define g24.1
  (mk-weighted-graph/directed '((3 s a) (5 s c) (2 s e)
                                (-4 a b)
                                (6 c d) (-3 d c)
                                (3 e f) (-6 f e)
                                (4 b g) (8 d g) (7 f g)
                                (2 h i) (3 i j) (-8 j h))))

(check-exn exn:fail? (λ () (bellman-ford g24.1 's)))

(define g24.2
  (mk-weighted-graph/directed '((3 s t) (5 s y)
                                (6 t x) (2 t y)
                                (2 x z)
                                (7 z x) (3 z s)
                                (6 y z) (4 y x) (1 y t))))

(define-values (d24.2 π24.2) (bellman-ford g24.2 's))
(check-equal? d24.2
              (make-hash '((x . 9) (y . 5) (z . 11) (t . 3) (s . 0))))

(define g24.3
  (mk-weighted-graph/directed '((6 s t) (7 s y)
                                (5 t x) (8 t y) (-4 t z)
                                (-2 x t)
                                (7 z x) (2 z s)
                                (9 y z) (-3 y x))))

(define-values (d24.3 π24.3) (bellman-ford g24.3 's))
(check-equal? d24.3
              (make-hash '((x . 4) (y . 7) (z . -2) (t . 2) (s . 0))))


(define g24.5 (mk-weighted-graph/directed '((5 r s) (3 r t)
                                            (6 s x) (2 s t)
                                            (7 t x) (4 t y) (2 t z)
                                            (1 x z) (-1 x y)
                                            (-2 y z))))

(check-true (dag? g24.5))

(define-values (d24.5 π24.5) (dag-shortest-paths g24.5 's))

(check-equal? d24.5
              (make-hash '((r . +inf.0) (s . 0) (t . 2) (x . 6) (y . 5) (z . 3))))

