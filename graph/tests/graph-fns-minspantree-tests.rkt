#lang racket
(require "../graph-weighted.rkt"
         "../graph-fns-minspantree.rkt"
         "test-utils.rkt")

(require rackunit)

;; mst tests ------------------------------------------------------------------
(define g23.1 
  (mk-weighted-graph/undirected
   '((4 a b) (8 a h) (11 b h) (8 b c) (7 c d) (9 d e) (14 d f) 
     (10 e f) (4 c f) (2 c i) (7 h i) (6 g i) (1 h g) (2 g f))))

;; both (a h) or (b c) are ok
(check-expected-msts (mst-kruskal g23.1)
                     '((a b) (a h) (c i) (c f) (f g) (g h) (c d) (d e))
                     '((a b) (b c) (c i) (c f) (f g) (g h) (c d) (d e)))

(check-expected-msts (mst-prim g23.1 'a)
                     '((a b) (b c) (c i) (c f) (f g) (g h) (c d) (d e))
                     '((a b) (a h) (c i) (c f) (f g) (g h) (c d) (d e)))

;; since g23.1 is an undirected graph, the previous test is correct but this test
;; additionally checks that edges are going in the right direction
(check-true (or (equal? (apply set (mst-prim g23.1 'a))
                        (apply set '((a b) (b c) (c i) (c f) (f g) (g h) (c d) (d e))))
                (equal? (apply set (mst-prim g23.1 'a))
                        (apply set '((a b) (a h) (c i) (c f) (f g) (g h) (c d) (d e))))))

;; from wikipedia (2014-07-01)
(define g/wik (mk-undirected-graph '((a b) (a d) (b d) (c d)) '(2 1 2 3)))
(check-expected-msts (mst-prim g/wik 'a) '((a d) (b d) (c d))
                                         '((a d) (b a) (d c)))

;; http://scanftree.com/Data_Structure/prim's-algorithm
(define g/cpp (mk-undirected-graph
               '((0 1) (0 2) (0 3) (1 2) (1 4) (2 3) (2 4) (2 5) (3 5) (4 5))
               '(3 1 6 5 3 5 6 4 2 6)))
(check-expected-msts (mst-prim g/cpp 0) '((0 1) (0 2) (1 4) (2 5) (5 3)))

;; boost example
;; http://www.boost.org/doc/libs/1_38_0/boost/graph/prim_minimum_spanning_tree.hpp
;; http://www.boost.org/doc/libs/1_38_0/libs/graph/example/prim-example.cpp
(define g/boost (mk-undirected-graph 
                 '((0 2) (1 3) (1 4) (2 1) (2 3) (3 4) (4 0))
                 '(1 1 2 7 3 1 1)))

(check-expected-msts (mst-prim g/boost 0) '((1 3) (2 0) (3 4) (4 0)))