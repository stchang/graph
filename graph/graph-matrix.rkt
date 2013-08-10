#lang racket

(require "gen-graph.rkt")
(require racket/generator)

; weighted, adjacency matrix graph

(provide (except-out (all-defined-out) add-vertex@))

;; (internal graph functions and names have a @ suffix)

;; A MatrixGraph is a (matrix-graph Weights)
(struct matrix-graph (vertices weights) #:transparent
  #:methods gen:graph
  [(define (in-vertices g) (set->list (matrix-graph-vertices g)))
   (define (in-neighbors g v) (error))
   (define (in-edges g) (error))
   (define (edge-weight g u v) 
     (if (equal? u v) 0
         (hash-ref (matrix-graph-weights g) (list u v) +inf.0)))])

;; A Vertex is any value comparable with equal?

;; An Edge is a (list Vertex Vertex)

;; A Weights is a [MutableHashOf Edge -> Number]


;; undirected graph constructor
;; [Listof (list Number Vertex Vertex)] -> WeightedGraph
#;(define (mk-weighted-matrix-graph/undirected es)
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
(define (mk-weighted-matrix-graph/directed es)
  (define vs (set))
  (define weights (make-hash))
  (for ([w+e es]) 
    (cond [(list? w+e) 
           (define e (cdr w+e)) (define w (car w+e)) 
           (set! vs (set-union vs (apply set e)))
           (hash-set! weights e w)]
          [else (set! vs (set-add vs w+e))]))
  (matrix-graph vs weights))

;; directed graph constructor
(define (mk-unweighted-matrix-graph/directed es)
  (define vs (set))
  (define weights (make-hash))
  (for ([e es]) 
    (cond [(list? e) 
           (set! vs (set-union vs (apply set e)))
           (hash-set! weights e 1)]
          [else (set! vs (set-add vs e))]))
  (matrix-graph vs weights))


;(define (mk-empty-matrix-graph) (matrix-graph (set) (make-hash)))

;; returns vertices as a list
;(define (in-weighted-graph-vertices g) (hash-keys (weighted-graph-adjlist g)))

;; returns neighbors as a sequence
#;(define (in-weighted-graph-neighbors g v) 
  (in-set (hash-ref (weighted-graph-adjlist g) v (set))))

;; returns edges as a sequence
#;(define (in-weighted-graph-edges g)
  (in-generator 
   (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
     (yield (list u v)))))
  

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)
;(define (add-edge@ adj u v) (hash-update! adj u (λ (vs) (set-add vs v)) (set)))
;(define (add-vertex@ adj v) (hash-update! adj v (λ (vs) vs) (set)))
(define (add-vertex@ vs v) (set! vs (set-add vs v)))
(define (add-vertices@ vs . new-vs) (set! vs (set-union vs (apply set new-vs))))

(define (add-edge! G u v w) (hash-set! (matrix-graph-weights G) (list u v) w))