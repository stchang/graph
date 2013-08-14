#lang racket

(require "hash-utils.rkt"
         "utils.rkt"
         "gen-graph.rkt")

(provide (all-defined-out))

;; graph coloring algorithms

;; naive coloring algorithm using backtracking
;; returns hash of vertex to color, or #f if coloring is not possible
(define (coloring G num-colors #:order [order (λ (x) x)])
  (define-hash color)
  (let loop ([vs (order (in-vertices G))])
    (cond 
      [(null? vs) color]
      [else
       (define u (car vs))
       (let color-select-loop ([try-col 0])
         (and 
          (not (= try-col num-colors)) ; fail
          (cond 
            [(set-member? ; calc used colors
              (for/set ([v (in-neighbors G u)]) (color v num-colors))  
              try-col)
             (color-select-loop (add1 try-col))]
            [else
             (color-set! u try-col) ; try this color and try to color other vs
             (or (loop (cdr vs))    ; if it works, great
                 (color-select-loop (add1 try-col)))])))]))) ; else, backtrack


;; Assigns to vertex v the smallest color not used by neighbors,
;;  bumping up the number of colors if necessary
;; Always produces a valid coloring but the optimality depends on the order in
;;  which the vertices are considered.
;; Uses "smallest-last" ordering by default. Otherwise, #:order must be a
;;  procedure that sorts the vertices.
(define (coloring/greedy G #:order [order 'smallest-last])
  (define num-colors 0)
  (define-hash color)
  (define vs (in-vertices G))
  (define ordered-vs 
    (if (eq? order 'smallest-last)
        (order-smallest-last G)
        (order vs)))
  (define num-vs (length vs)) ; default color, ie "uncolored"
  (for ([u ordered-vs])
    (define used-colors (for/set ([v (in-neighbors G u)]) (color v num-vs)))
    ;; find smallest color unused by adjacent vertices
    (define smallest-color
      (for/last ([smallest-color (in-range num-vs)]
                 #:final (not (set-member? used-colors smallest-color)))
        smallest-color))
    (color-set! u smallest-color)
    (when (= smallest-color num-colors) (add1! num-colors)))
  (values num-colors color))

(define (valid-coloring? G coloring)
  (define (color v) (hash-ref coloring v))
  (not (for/or ([e (in-edges G)]) (= (color (first e)) (color (second e))))))

(require (prefix-in r: data/heap))
;; returns the vertices of the graph in "smallest-last" order
;; ie v with smallest degree is last, remove v, then repeat for 2nd to last, etc
;; only works when graph is undirected
(define (order-smallest-last G)
  (define-hash deg)
  (for ([u (in-vertices G)])
    (deg-set! u (length (sequence->list (in-neighbors G u)))))
  (define H (r:make-heap (λ (x y) (< (deg x) (deg y)))))
  (r:heap-add-all! H (in-vertices G))
  (define in-H (apply set (in-vertices G)))
  (let loop ([res null])
    (cond 
      [(set-empty? in-H) res]
      [else
       ;; keep popping until we find min that is "in H"
       (let remove-loop ()
         (unless (set-member? in-H (r:heap-min H)) 
           (r:heap-remove-min! H) (remove-loop)))
       (define min-v (r:heap-min H))
       (r:heap-remove-min! H)
       (set! in-H (set-remove in-H min-v))
       ;; subtract degrees from neighbors of min-v
       (for ([v (in-neighbors G min-v)]) 
         (deg-set! v (sub1 (deg v)))
         (r:heap-add! H v))
       (loop (cons min-v res))])))