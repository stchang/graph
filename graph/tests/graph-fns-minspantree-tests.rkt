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
(check-true (or (equal? (lists->sets (mst-kruskal g23.1))
                        (lists->sets '((a b) (a h) (c i) (c f) (f g) (g h) (c d) (d e))))
                (equal? (lists->sets (mst-kruskal g23.1))
                        (lists->sets '((a b) (b c) (c i) (c f) (f g) (g h) (c d) (d e))))))

(check-true (or (equal? (lists->sets (mst-prim g23.1 'a))
                        (lists->sets '((a b) (b c) (c i) (c f) (f g) (g h) (c d) (d e))))
                (equal? (lists->sets (mst-prim g23.1 'a))
                        (lists->sets '((a b) (a h) (c i) (c f) (f g) (g h) (c d) (d e))))))
