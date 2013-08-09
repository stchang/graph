#lang racket

(require "gen-graph.rkt")
(require racket/generator)

; unweighted, adjacency-list graph

(provide (except-out (all-defined-out)))

;; A Graph is a (graph AdjacencyList)
(struct unweighted-graph (adjlist) #:transparent 
  #:methods gen:graph
  [(define (in-vertices g) (in-unweighted-graph-vertices g))
   (define (in-neighbors g v) (in-unweighted-graph-neighbors g v))
   (define (in-edges g) (in-unweighted-graph-edges g))
   (define (edge-weight g u v) (error 'edge-weight "unweighted graph"))])

;; An AdjacencyList is a [MutableHashOf Vertex -> Vertex]
;;   and is the internal graph representation

;; (internal graph functions and names have a @ suffix)

;; A Vertex is any value comparable with equal?

;; undirected graph constructor
;; [Listof (list Vertex Vertex)] -> Graph
(define (mk-unweighted-graph/undirected es)
  (define adj (make-hash))
  (for ([e es])
    (cond [(list? e) (apply add-edge@ adj e)
                     (apply add-edge@ adj (reverse e))]
          [else (add-vertex@ adj e)])) ; neighborless vertices
  (unweighted-graph adj))

;; directed graph constructor
(define (mk-unweighted-graph/directed es)
  (define adj (make-hash))
  (for ([e es]) 
    (cond [(list? e) (apply add-edge@ adj e)
                     (add-vertex@ adj (second e))]
          [else (add-vertex@ adj e)]))
  (unweighted-graph adj))

(define (mk-unweighted-graph/adj adj)
  (define adj-hash (make-hash))
  (for ([vs adj]) (hash-set! adj-hash (car vs) (apply set (cdr vs))))
  (unweighted-graph adj-hash))


;; returns vertices as a list
(define (in-unweighted-graph-vertices g) (hash-keys (unweighted-graph-adjlist g)))

;; returns neighbors as a sequence
(define (in-unweighted-graph-neighbors g v)
  (in-set (hash-ref (unweighted-graph-adjlist g) v (set))))

;; returns edges as a sequence
(define (in-unweighted-graph-edges g)
  (in-generator 
   (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
     (yield (list u v)))))

  

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)
(define (add-edge@ adj u v) (hash-update! adj u (λ (vs) (set-add vs v)) (set)))
(define (add-vertex@ adj v) (hash-update! adj v (λ (vs) vs) (set)))


(define (transpose G)
  (define adj^T (make-hash))
  (for ([u (in-vertices G)])
    (add-vertex@ adj^T u)
    (for ([v (in-neighbors G u)])
      (add-edge@ adj^T v u)))
  (unweighted-graph adj^T))
