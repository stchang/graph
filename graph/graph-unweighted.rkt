#lang racket/base

(require "gen-graph.rkt"
         "adjlist-utils.rkt")

(require racket/sequence racket/set racket/unsafe/ops)

; unweighted, adjacency-list graph

(provide mk-unweighted-graph/undirected
         mk-unweighted-graph/directed
         mk-unweighted-graph/adj
         unweighted-graph?)

(define-syntax-rule (get-adjlist g) (unsafe-struct*-ref g 0))

;; A Graph is a (graph AdjacencyList)
(struct unweighted-graph (adjlist) 
   #:methods gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur) 
     (equal?-recur (get-adjlist g1) (get-adjlist g2)))
   (define (hash-proc g hash-recur) (* 3 (hash-recur (get-adjlist g))))
   (define (hash2-proc g hash2-recur) (* 4 (hash2-recur (get-adjlist g))))]
  #:methods gen:graph
  [(define (get-vertices g) (get-unweighted-graph-vertices g))
   (define (in-vertices g) (in-unweighted-graph-vertices g))
   (define (get-neighbors g v) (sequence->list (in-unweighted-graph-neighbors g v)))
   (define (in-neighbors g v) (in-unweighted-graph-neighbors g v))
   (define (vertex=? g u v) (equal? u v))
   (define (edge-weight g u v) 
     (if (member (list u v) (get-edges g)) 1 +inf.0))
   (define (add-directed-edge! g u v [weight #f])
     (define adj (get-adjlist g))
     (add-edge@ adj u v)
     (add-vertex@ adj v))
   (define (add-edge! g u v [weight #f])
     (define adj (get-adjlist g))
     (add-edge@ adj u v)
     (add-edge@ adj v u))
   (define (remove-directed-edge! g u v)
     (define adj (get-adjlist g))
     (remove-edge@ adj u v))
   (define (remove-edge! g u v)
     (define adj (get-adjlist g))
     (remove-edge@ adj u v)
     (remove-edge@ adj v u))
   (define (add-vertex! g v) (add-vertex@ (get-adjlist g) v))
   (define (remove-vertex! g v) (remove-vertex@ (get-adjlist g) v))
   (define (rename-vertex! g old new)
     (when (member new (get-vertices g))
       (error 'rename-vertex! "new vertex ~a already exists in the graph" new))
     (rename-vertex@ (get-adjlist g) old new))
   (define (has-vertex? g v) (and (member v (get-vertices g)) #t))
   (define (has-edge? g u v)
     (and (has-vertex? g u) (has-vertex? g v)
          (member v (get-neighbors g u))
          #t))
   ;; returns edges as a sequence
   (define (in-edges g) (in-list (get-edges g)))
   (define (get-edges g) 
     (for*/list ([u (in-vertices g)] [v (in-neighbors g u)]) (list u v)))
   (define (graph-copy g)
     (struct-copy unweighted-graph g [adjlist (hash-copy (get-adjlist g))]))
   (define (transpose G)
     (define adj^T (make-hash))
     (for ([u (in-vertices G)])
       (add-vertex@ adj^T u)
       (for ([v (in-neighbors G u)])
         (add-edge@ adj^T v u)))
     (unweighted-graph adj^T))])


;; An AdjacencyList is a [MutableHashOf Vertex -> [Setof Vertex]]
;;   and is the internal graph representation
;; (internal adjlist functions and names have a @ suffix)

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
                     (add-vertex@ adj (unsafe-car (unsafe-cdr e)))]
          [else (add-vertex@ adj e)]))
  (unweighted-graph adj))

(define (mk-unweighted-graph/adj adj)
  (define adj-hash (make-hash))
  (for ([vs adj]) 
    (define u (unsafe-car vs)) 
    (define us (unsafe-cdr vs))
    (hash-set! adj-hash u (set-union (hash-ref adj-hash u (set))
                                     (apply set us)))
    (for ([v us] #:unless (hash-has-key? adj-hash v)) 
      (hash-set! adj-hash v (set))))
  (unweighted-graph adj-hash))


;; returns vertices as a list
;; - analogous to hash-keys vs in-hash-keys
(define (get-unweighted-graph-vertices g) (hash-keys (get-adjlist g)))
;; returns vertices as a sequence
(define (in-unweighted-graph-vertices g) (in-hash-keys (get-adjlist g)))


;; returns neighbors as a sequence
(define (in-unweighted-graph-neighbors g v)
  (in-set 
   (hash-ref (get-adjlist g) v 
             (λ () (error 'in-vertices "vertex ~a not in graph ~a" v g)))))
