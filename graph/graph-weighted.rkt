#lang racket

(require "gen-graph.rkt")
(require "adjlist-utils.rkt")

(require racket/generator)

; weighted, adjacency-list graph

(provide (except-out (all-defined-out)))

;; A WeightedGraph is a (weighted-graph AdjacencyList Weights)
(struct weighted-graph (adjlist weights) #:transparent
  #:methods gen:graph
  [(define (in-vertices g) (in-weighted-graph-vertices g))
   (define (in-neighbors g v) (in-weighted-graph-neighbors g v))
   (define (edge-weight g u v) ; ok to return infty for non-existent edge?
     (hash-ref (weighted-graph-weights g) (list u v) +inf.0))
   (define (add-directed-edge! g u v [weight #f])
     (define adj (weighted-graph-adjlist g))
     (add-edge@ adj u v)
     (add-vertex@ adj v))
   (define (add-edge! g u v [weight #f])
     (define adj (weighted-graph-adjlist g))
     (add-edge@ adj u v)
     (add-edge@ adj v u))
   (define (add-vertex! g v)
     (add-vertex@ (weighted-graph-adjlist g) v))
   (define (has-vertex? g v) (and (member v (in-vertices g)) #t))
   (define (has-edge? g u v)
     (and (has-vertex? g u) (has-vertex? g v)
          (member v (sequence->list (in-neighbors g u)))
          #t))
   ;; returns edges as a sequence
   (define (in-edges g)
     (in-generator 
      (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
        (yield (list u v)))))])


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
(define (in-weighted-graph-vertices g) (hash-keys (weighted-graph-adjlist g)))

;; returns neighbors as a sequence
(define (in-weighted-graph-neighbors g v) 
  (in-set 
   (hash-ref (weighted-graph-adjlist g) v 
             (λ () (error 'in-vertices "vertex ~a not in graph ~a" v g)))))



