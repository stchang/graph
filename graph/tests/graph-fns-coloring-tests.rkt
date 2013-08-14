#lang racket

(require "../graph-unweighted.rkt" 
         "../gen-graph.rkt" 
         "../graph-fns-coloring.rkt")

(require rackunit)

; graph coloring tests --------------------------------------------------------

;; greedy coloring ------------------------------------------------------------
(define bipartite1
  (mk-unweighted-graph/adj
   '((1 6 7 8)
     (2 5 7 8)
     (3 5 6 8)
     (4 5 6 7)
     (5 2 3 4)
     (6 1 3 4)
     (7 1 2 4)
     (8 1 2 3))))

(define-values (num-colors1best coloring1best) ; best order
  (coloring/greedy bipartite1 #:order (λ _ (build-list 8 add1))))
(define (verify-num-colors-matches num-colors color)
  (= num-colors (add1 (apply max (sequence->list (in-hash-values color))))))
(check-true (verify-num-colors-matches num-colors1best coloring1best))
(check-true (valid-coloring? bipartite1 coloring1best))
(check-equal? 
 coloring1best
 (make-hash '((8 . 1) (7 . 1) (6 . 1) (5 . 1) (4 . 0) (3 . 0) (2 . 0) (1 . 0))))
(define-values (num-colors1worst coloring1worst) ; worst order
  (coloring/greedy 
   bipartite1 
   #:order 
   (λ _ (apply append (map list 
                           (build-list 4 add1) 
                           (build-list 4 (λ (x) (+ x 5))))))))
(check-true (verify-num-colors-matches num-colors1worst coloring1worst))
(check-true (valid-coloring? bipartite1 coloring1worst))
(check-equal? 
 coloring1worst
 (make-hash '((8 . 3) (7 . 2) (6 . 1) (5 . 0) (4 . 3) (3 . 2) (2 . 1) (1 . 0))))

;; testing order-smallest-last
(define g-smallest-last
  (mk-unweighted-graph/adj
   '((a b c e)
     (b c a e)
     (c e b d)
     (d c)
     (e))))
(check-equal? (order-smallest-last g-smallest-last)
              '(a b c d e))