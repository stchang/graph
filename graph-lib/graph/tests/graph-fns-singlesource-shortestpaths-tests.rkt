#lang racket
(require "../graph-weighted.rkt"
         "../graph-fns-singlesource-shortestpaths.rkt"
         "../graph-fns-basic.rkt"
         "test-utils.rkt")

(require rackunit)


;; single-source shortest path tests ------------------------------------------

;; bellman-ford ---------------------------------------------------------------

;; neg weight cycle
(define g24.1
  (mk-weighted-graph/directed '((3 s a) (5 s c) (2 s e)
                                (-4 a b)
                                (6 c d) (-3 d c)
                                (3 e f) (-6 f e)
                                (4 b g) (8 d g) (7 f g)
                                (2 h i) (3 i j) (-8 j h))))

(check-exn exn:fail? (λ () (bellman-ford g24.1 's)))

(define g24.1b
  (mk-directed-graph 
   '((s a) (s c) (s e) (a b) (c d) (d c) (e f) (f e) (b g) (d g) (f g) (h i) (i j) (j h))
   '(3 5 2 -4 6 -3 3 -6 4 8 7 2 3 -8)))

(check-exn exn:fail? (λ () (bellman-ford g24.1b 's)))


(define g24.2
  (mk-weighted-graph/directed '((3 s t) (5 s y)
                                (6 t x) (2 t y)
                                (2 x z)
                                (7 z x) (3 z s)
                                (6 y z) (4 y x) (1 y t))))

(define-values (d24.2 π24.2) (bellman-ford g24.2 's))
(check-equal? d24.2
              (make-hash '((x . 9) (y . 5) (z . 11) (t . 3) (s . 0))))

(define g24.4
  (mk-weighted-graph/directed '((6 s t) (7 s y)
                                (5 t x) (8 t y) (-4 t z)
                                (-2 x t)
                                (7 z x) (2 z s)
                                (9 y z) (-3 y x))))

(define-values (d24.4 π24.4) (bellman-ford g24.4 's))
(check-equal? d24.4
              (make-hash '((x . 4) (y . 7) (z . -2) (t . 2) (s . 0))))


(check-false (dag? g24.1))
(check-false (dag? g24.2))
(check-false (dag? g24.4))

(define g24.2b
  (mk-directed-graph 
   '((s t) (s y) (t x) (t y) (x z) (z x) (z s) (y z) (y x) (y t))
   '(3 5 6 2 2 7 3 6 4 1)))

(define-values (d24.2b π24.2b) (bellman-ford g24.2b 's))
(check-equal? d24.2b
              (make-hash '((x . 9) (y . 5) (z . 11) (t . 3) (s . 0))))

(define g24.4b
  (mk-directed-graph
   '((s t) (s y) (t x) (t y) (t z) (x t) (z x) (z s) (y z) (y x))
   '(6 7 5 8 -4 -2 7 2 9 -3)))

(define-values (d24.4b π24.4b) (bellman-ford g24.4b 's))
(check-equal? d24.4b
              (make-hash '((x . 4) (y . 7) (z . -2) (t . 2) (s . 0))))


(check-false (dag? g24.1b))
(check-false (dag? g24.2b))
(check-false (dag? g24.4b))


;; dag-shortest-paths ---------------------------------------------------------

(define g24.5 (mk-weighted-graph/directed '((5 r s) (3 r t)
                                            (6 s x) (2 s t)
                                            (7 t x) (4 t y) (2 t z)
                                            (1 x z) (-1 x y)
                                            (-2 y z))))

(check-true (dag? g24.5))

(define-values (d24.5 π24.5) (dag-shortest-paths g24.5 's))
(define-values (d24.5b π24.5b) (bellman-ford g24.5 's))

(check-equal? d24.5 d24.5b)
(check-equal? d24.5
              (make-hash '((r . +inf.0) (s . 0) (t . 2) (x . 6) (y . 5) (z . 3))))

(check-equal? π24.5 π24.5b)
(check-equal? π24.5
              (make-hash '((z . y) (x . s) (y . x) (t . s) (s . #f) (r . #f))))

;; dijkstra -------------------------------------------------------------------
(define g24.6 (mk-weighted-graph/directed '((10 s t) (5 s y)
                                            (1 t x) (2 t y)
                                            (4 x z)
                                            (6 z x) (6 z s)
                                            (3 y t) (9 y x) (2 y z))))

(define-values (d24.6 π24.6) (dijkstra g24.6 's))

(check-equal? d24.6
              (make-hash '((z . 7) (x . 9) (y . 5) (s . 0) (t . 8))))
(check-equal? π24.6
              (make-hash '((z . y) (x . t) (y . s) (s . #f) (t . y))))

(define g24.8 
  (mk-weighted-graph/directed '((0 v0 v1) (0 v0 v2) (0 v0 v3) (0 v0 v4) (0 v0 v5)
                                (5 v1 v3) (4 v1 v4)
                                (0 v2 v1)
                                (-3 v3 v5) (-1 v3 v4)
                                (-3 v4 v5)
                                (-1 v5 v1) (1 v5 v2))))

;; from wikipedia article
(define g/wiki 
  (mk-undirected-graph '((1 2) (1 3) (1 6) (2 3) (2 4) (3 4) (3 6) (4 5) (5 6))
                       '(7 9 14 10 15 11 2 6 9)))
(define-values (dwik πwik) (dijkstra g/wiki 1))
(check-equal? dwik (make-hash '((1 . 0) (2 . 7) (3 . 9) (4 . 20) (5 . 20) (6 . 11))))
(check-equal? πwik (make-hash '((1 . #f) (2 . 1) (3 . 1) (4 . 3) (5 . 6) (6 . 3))))

;; boost example
;; http://www.boost.org/doc/libs/1_55_0/libs/graph/example/dijkstra-example.cpp

(define g/boo
  (mk-directed-graph '((A C) (B B) (B D) (B E) (C B) (C D) (D E) (E A) (E B))
                     '(1 2 1 2 7 3 1 1 1)))
(define-values (dboo πboo) (dijkstra g/boo 'A))
(check-equal? dboo (make-hash '((A . 0) (B . 6) (C . 1) (D . 4) (E . 5))))
(check-equal? πboo (make-hash '((A . #f) (B . E) (C . A) (D . C) (E . D))))


;; linear programming test ----------------------------------------------------
(define-values (d24.8 π24.8) (bellman-ford g24.8 'v0))
(check-equal? d24.8
              (make-hash '((v5 . -4) (v3 . 0) (v1 . -5) (v4 . -1) (v0 . 0) (v2 . -3))))