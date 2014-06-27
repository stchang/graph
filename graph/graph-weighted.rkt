#lang racket

(require "gen-graph.rkt")
(require "adjlist-utils.rkt")

(require racket/generator)

; weighted, adjacency-list graph

(provide mk-weighted-graph/undirected 
         mk-weighted-graph/directed
         weighted-graph?)

;; A WeightedGraph is a (weighted-graph AdjacencyList Weights)
(struct weighted-graph (adjlist weights) 
  #:methods gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur) 
     (and (equal?-recur (weighted-graph-adjlist g1) (weighted-graph-adjlist g2))
          (equal?-recur (weighted-graph-weights g1) (weighted-graph-weights g2))))
   (define (hash-proc g hash-recur) 
     (+ (* 3 (hash-recur (weighted-graph-adjlist g)))
        (* 5 (hash-recur (weighted-graph-weights g)))))
   (define (hash2-proc g hash2-recur)
     (+ (* 6 (hash2-recur (weighted-graph-adjlist g)))
        (* 7 (hash2-recur (weighted-graph-weights g)))))]
  #:methods gen:graph
  [(define (get-vertices g) (get-weighted-graph-vertices g))
   (define (in-vertices g) (in-weighted-graph-vertices g))
   (define (get-neighbors g v) (sequence->list (in-weighted-graph-neighbors g v)))
   (define (in-neighbors g v) (in-weighted-graph-neighbors g v))
   (define (edge-weight g u v) ; ok to return infty for non-existent edge?
;     (unless (and (has-vertex? g u) (has-vertex? g v))
;       (error 'edge-weight "non-existent edge ~a ~a" u v))
     (hash-ref (weighted-graph-weights g) (list u v) +inf.0))
   (define (add-directed-edge! g u v [weight #f])
     (define adj (weighted-graph-adjlist g))
     (add-edge@ adj u v)
     (add-vertex@ adj v)
     (when weight (hash-set! (weighted-graph-weights g) (list u v) weight)))
   (define (add-edge! g u v [weight #f])
     (define adj (weighted-graph-adjlist g))
     (add-edge@ adj u v)
     (add-edge@ adj v u)
     (when weight (hash-set! (weighted-graph-weights g) (list u v) weight)))
   (define (remove-directed-edge! g u v)
     (define adj (weighted-graph-adjlist g))
     (remove-edge@ adj u v)
     (hash-remove! (weighted-graph-weights g) (list u v)))
   (define (remove-edge! g u v)
     (define adj (weighted-graph-adjlist g))
     (remove-edge@ adj u v)
     (remove-edge@ adj v u)
     (hash-remove! (weighted-graph-weights g) (list u v))
     (hash-remove! (weighted-graph-weights g) (list v u)))
   (define (add-vertex! g v)
     (add-vertex@ (weighted-graph-adjlist g) v))
   (define (remove-vertex! g v)
     (define wgts (weighted-graph-weights g))
     (remove-vertex@ (weighted-graph-adjlist g) v)
     (for ([e (hash-keys wgts)] #:when (member v e))
       (hash-remove! wgts e)))
   (define (rename-vertex! g old new)
     (when (member new (get-vertices g))
       (error 'rename-vertex! "new vertex ~a already exists in the graph" new))
     (rename-vertex@ (weighted-graph-adjlist g) old new))
   (define (has-vertex? g v) (and (member v (get-vertices g)) #t))
   (define (has-edge? g u v)
     (and (has-vertex? g u) (has-vertex? g v)
          (member v (get-neighbors g u))
          #t))
   ;; returns edges as a sequence
   (define (in-edges g)
     (in-generator 
      (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
        (yield (list u v)))))
   (define (get-edges g) (sequence->list (in-edges g)))
   (define (graph-copy g)
     (struct-copy weighted-graph g 
                  [adjlist (hash-copy (weighted-graph-adjlist g))]
                  [weights (hash-copy (weighted-graph-weights g))]))
   (define (transpose G)
     (define G^T (mk-weighted-graph/undirected null))
     (for ([u (in-vertices G)])
       (add-vertex! G^T u)
       (for ([v (in-neighbors G u)])
        (add-directed-edge! G^T v u (edge-weight G u v))))
     G^T)])


;; An AdjacencyList is a [MutableHashOf Vertex -> [Setof Vertex]]
;;   and is the internal graph representation
;; (internal adjlist functions and names have a @ suffix)

;; A Vertex is any value comparable with equal?

;; An Edge is a (list Vertex Vertex)

;; A Weights is a [MutableHashOf Edge -> Number]


;; undirected graph constructor
;; [Listof (list Number Vertex Vertex)] -> WeightedGraph
(define (mk-weighted-graph/undirected es)
  (define adj (make-hash))
  (define weights (make-hash))
  (for ([w+e es])
    (cond [(list? w+e) 
           (define e (cdr w+e))              (define w (car w+e))
           (apply add-edge@ adj e)           (hash-set! weights e w)
           (apply add-edge@ adj (reverse e)) (hash-set! weights (reverse e) w)]
          [else (add-vertex@ adj w+e)])) ; neighborless vertex
  (weighted-graph adj weights))

;; directed graph constructor
(define (mk-weighted-graph/directed es)
  (define adj (make-hash))
  (define weights (make-hash))
  (for ([w+e es]) 
    (cond [(list? w+e) 
           (define e (cdr w+e)) (define w (car w+e)) 
           (apply add-edge@ adj e) 
           (add-vertex@ adj (second e))
           (hash-set! weights e w)]
          [else (add-vertex@ adj w+e)]))
  (weighted-graph adj weights))

;; returns vertices as a list
;; - analogous to hash-keys vs in-hash-keys
(define (get-weighted-graph-vertices g) (hash-keys (weighted-graph-adjlist g)))
;; returns vertices as a sequence
(define (in-weighted-graph-vertices g) (in-hash-keys (weighted-graph-adjlist g)))

;; returns neighbors as a sequence
(define (in-weighted-graph-neighbors g v) 
  (in-set 
   (hash-ref (weighted-graph-adjlist g) v 
             (λ () (error 'in-vertices "vertex ~a not in graph ~a" v g)))))
