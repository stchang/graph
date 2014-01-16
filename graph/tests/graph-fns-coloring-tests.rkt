#lang racket

(require "../graph-unweighted.rkt" 
         "../gen-graph.rkt" 
         "../graph-fns-coloring.rkt")

(require rackunit)

; graph coloring tests --------------------------------------------------------
(define g-2color (mk-unweighted-graph/undirected '((A B) (B C) (B D))))
(define g-2color-coloring (coloring g-2color 2))
(check-false (not g-2color-coloring))
(check-true (valid-coloring? g-2color g-2color-coloring))

(define g-3color1 (mk-unweighted-graph/undirected '((A B) (B C) (B D) (C D))))
(define g-3color1-2coloring (coloring g-3color1 2))
(check-false g-3color1-2coloring)
(define g-3color1-3coloring (coloring g-3color1 3))
(check-false (not g-3color1-3coloring))
(check-true (valid-coloring? g-3color1 g-3color1-3coloring))

(define g-3color2 (mk-unweighted-graph/undirected
                   '((A B) (A C) (B C) (B D) (C D) (D E) (D F) (E F))))
(define g-3color2-2coloring (coloring g-3color2 2))
(check-false g-3color2-2coloring)
(define g-3color2-3coloring (coloring g-3color2 3))
(check-false (not g-3color2-3coloring))
(check-true (valid-coloring? g-3color2 g-3color2-3coloring))

;; graph that has to backtrack to find a valid 3coloring
(define g-3color3 (mk-unweighted-graph/undirected
                   '((A B) (B C) (C D) (D A) (D B) (B E) (C E) (E F))))
(define g-3color3-2coloring (coloring g-3color3 2))
(check-false g-3color3-2coloring)
(define g-3color3-3coloring (coloring g-3color3 3 #:order (λ _ '(A B E F C D))))
(check-false (not g-3color3-3coloring))
(check-true (valid-coloring? g-3color3 g-3color3-3coloring))

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

(define bipartite1-2coloring (coloring bipartite1 2))
(check-false (not bipartite1-2coloring))
(check-true (valid-coloring? bipartite1 bipartite1-2coloring))


;; greedy coloring ------------------------------------------------------------

(define-values (num-colors1best coloring1best) ; best order
  (coloring/greedy bipartite1 #:order (λ _ (build-list 8 add1))))
(define (check-coloring g num-colors the-coloring)
  (check-true (valid-coloring? g the-coloring))
  (check-true
   (= num-colors (add1 (apply max (sequence->list (in-hash-values the-coloring)))))))
(check-coloring bipartite1 num-colors1best coloring1best)
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
(check-coloring bipartite1 num-colors1worst coloring1worst)
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

;; more greedy coloring tests -------------------------------------------------
;; from: http://blog.jverkamp.com/2014/01/15/graph-coloring/
;;  and: http://www.reddit.com/r/dailyprogrammer/comments/1tj0kl/122313_challenge_130_hard_coloring_frances/

(define butterfly (mk-unweighted-graph/undirected '((1 2) (2 3) (1 3) (3 4) (3 5) (4 5))))
(define-values (buttr-num-colors buttr-coloring) (coloring/greedy butterfly))
(check-equal? buttr-num-colors 3)
(check-coloring butterfly buttr-num-colors buttr-coloring)
(check-coloring butterfly 3 (coloring/brelaz butterfly))

(define loop-graph (mk-unweighted-graph/undirected '((1 4) (4 5) (5 2) (3 4) (3 6) (6 1))))
(define-values (loop-num-colors loop-coloring) (coloring/greedy loop-graph))
(check-equal? loop-num-colors 2)
(check-coloring loop-graph loop-num-colors loop-coloring)
(check-coloring loop-graph 2 (coloring/brelaz loop-graph))

; Given a string or input port, read a graph
; First line is number of following lines
; The rest of the lines have a node id than one or more ids of that node's neighbors
(define (read-graph [str/in (current-input-port)])
  (define in (if (string? str/in) (open-input-string str/in) str/in))
  (define node-count (read in))
 
  (define g (mk-unweighted-graph/undirected '()))
 
  (for* ([i (in-range node-count)]
         [line (in-lines in)])
    (define nums (map string->number (string-split line)))
    (when (> (length nums) 1)
      (for ([n (in-list (rest nums))])
        (add-edge! g (first nums) n))))
 
  g)

(define france (with-input-from-file "france.txt" read-graph))
(define-values (france-num-colors france-coloring) (coloring/greedy france))
(check-equal? france-num-colors 4)
(check-coloring france france-num-colors france-coloring)
(check-coloring france 4 (coloring/brelaz france))