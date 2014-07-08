#lang racket/base

(require racket/set racket/function racket/unsafe/ops)

(require "graph-property.rkt"
         "utils.rkt"
         "gen-graph.rkt")

(provide (all-defined-out))

;; graph coloring algorithms

;; naive coloring algorithm using backtracking
;; returns hash of vertex to color, or #f if coloring is not possible
(define (coloring G num-colors #:order [order (λ (x) x)])
  (define-vertex-property G color)
  (let loop ([vs (order (get-vertices G))])
    (cond 
      [(null? vs) (color->hash)]
      [else
       (define u (car vs))
       (let color-select-loop ([try-col 0])
         (and 
          (not (= try-col num-colors)) ; fail
          (cond 
            [(set-member? ; calc used colors
              (for/set ([v (in-neighbors G u)]) (color v #:default num-colors))  
              try-col)
             (color-select-loop (unsafe-add1 try-col))]
            [else
             (color-set! u try-col) ; try this color and try to color other vs
             (or (loop (cdr vs))    ; if it works, great
                 (color-select-loop (unsafe-add1 try-col)))])))]))) ; else, backtrack


;; Assigns to vertex v the smallest color not used by neighbors,
;;  bumping up the number of colors if necessary
;; Always produces a valid coloring but the optimality depends on the order in
;;  which the vertices are considered.
;; Uses "smallest-last" ordering by default. Otherwise, #:order must be a
;;  procedure that sorts the vertices.
(define (coloring/greedy G #:order [order 'smallest-last])
  (define num-colors 0)
  (define-vertex-property G color)
  (define vs (get-vertices G))
  (define ordered-vs 
    (if (eq? order 'smallest-last)
        (order-smallest-last G)
        (order vs)))
  (define num-vs (length vs)) ; default color, ie "uncolored"
  (for ([u ordered-vs])
    (define used-colors 
      (for/set ([v (in-neighbors G u)]) (color v #:default num-vs)))
    ;; find smallest color unused by adjacent vertices
    (define smallest-color
      (for/last ([smallest-color (in-range num-vs)]
                 #:final (not (set-member? used-colors smallest-color)))
        smallest-color))
    (color-set! u smallest-color)
    (when (= smallest-color num-colors) (unsafe-add1! num-colors)))
  (values num-colors (color->hash)))

;; Color a graph greedily, using the Brelaz vertex ordering heuristic
;; (This function is separate from coloring/greedy because the order the
;;  vertices is decided in an online manner)
;; Essentially color nodes using this algorithm:
;; - Color nodes with the most colored neighbors first
;; - Break ties using the nodes with the most unoclored neighbors
(define (coloring/brelaz g)
  (define-vertex-property G color)
  (define colored? (curry hash-has-key? (color->hash)))
  
  ; Used to break ties as mentioned above
  (define (count-colored-neighbors n) 
    (length (filter colored? (get-neighbors g n))))
 
  (define (count-uncolored-neighbors n)
    (length (filter (negate colored?) (get-neighbors g n))))
 
  (define graph-size (length (get-vertices g)))
 
  ; Each time, color the node with the highest current brélaz-number (see above)
  (for ([i (in-range graph-size)])
    (define next-node 
      (unsafe-car
       (sort
        (filter (negate colored?) (get-vertices g))
        (λ (n1 n2) 
          (or (> (count-colored-neighbors n1) (count-colored-neighbors n2))
              (and (= (count-colored-neighbors n1) (count-colored-neighbors n2))
                   (> (count-uncolored-neighbors n1) (count-uncolored-neighbors n2))))))))
    
    (for/first ([i (in-naturals)]
                #:unless 
                (member i (map (λ (n) (color n #:default #f)) 
                               (get-neighbors g next-node))))
      (color-set! next-node i)))
 
  (color->hash))

(define (valid-coloring? G coloring)
  (define (color v) (hash-ref coloring v))
  (not (for/or ([e (in-edges G)]) (= (color (unsafe-car e)) (color (unsafe-car (unsafe-cdr e)))))))

(require (prefix-in r: data/heap))
;; returns the vertices of the graph in "smallest-last" order
;; This is the Welsh-Powell heuristic
;; ie v with smallest degree is last, remove v, then repeat for 2nd to last, etc
;; only works when graph is undirected
(define (order-smallest-last G)
  (define-vertex-property G deg 
    #:init (length (get-neighbors G $v)))
  (define H (r:make-heap (λ (x y) (< (deg x) (deg y)))))
  (r:heap-add-all! H (get-vertices G))
  (define in-H (apply set (get-vertices G)))
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