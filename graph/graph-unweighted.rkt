#lang racket

(require "gen-graph.rkt")
(require racket/generator)

; unweighted, adjacency-list graph

(provide (except-out (all-defined-out)))

;; A Graph is a (graph AdjacencyList)
(struct unweighted-graph (adjlist) #:transparent 
  #:methods gen:graph
  [(define (in-vertices g) (in-graph-vertices g))
   (define (in-neighbors g v) (in-graph-neighbors g v))
   (define (in-edges g) (in-graph-edges g))
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
(define (in-graph-vertices g) (hash-keys (unweighted-graph-adjlist g)))

;; returns neighbors as a sequence
(define (in-graph-neighbors g v) (in-set (hash-ref (unweighted-graph-adjlist g) v (set))))

;; returns edges as a sequence
(define (in-graph-edges g)
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

;;; ----------------------------------------------------------------------------
;;; bfs and dfs
;
;(require data/queue)
;
;(define WHITE 'white)
;(define BLACK 'black)
;(define GRAY 'gray)
;
;;; bfs : Graph Vertex -> 
;;; s is the source vertex
;(define (bfs G s)
;  (define-hashes color d π)
;  (define (white? v) (eq? WHITE (color v)))
;  
;  (for ([u (in-vertices G)])
;    (cond [(equal? s u) (color-set! s GRAY)
;                        (d-set!     s 0)
;                        (π-set!     s #f)]
;          [else         (color-set! u WHITE)
;                        (d-set!     u +inf.0)
;                        (π-set!     u #f)]))
;  
;  (define Q (make-queue))
;  (enqueue! Q s)
;  (let loop () (when (non-empty-queue? Q)
;    (define u (dequeue! Q))
;    (for ([v (in-neighbors G u)] #:when (white? v))
;      (color-set! v GRAY)
;      (d-set!     v (add1 (d u)))
;      (π-set!     v u)
;      (enqueue! Q v))
;    (color-set! u BLACK)
;    (loop)))
;  (values color d π))
;
;;; returns shortest path in G from source s to v
;(define (shortest-path G s v)
;  (define-values (color d π) (bfs G s))
;  (reverse
;   (let loop ([v v])
;     (if (equal? v s) (list s)
;         (let ([πv (hash-ref π v (λ() (error 'shortest-path "no vertex ~a in graph ~a" v G)))])
;           (if πv (cons v (loop πv))
;               (error 'shortest-path "no path from ~a to ~a in graph ~a" s v G)))))))
;
;
;(define (dfs G #:order [order (λ (vs) vs)])
;  ;; d[u] = discovery time, f[u] = finishing time
;  (define-hashes color d π f)
;  (define (white? v) (eq? WHITE (color v)))
;  (for ([u (in-vertices G)]) 
;    (color-set! u WHITE)
;    (π-set!     u #f))
;  (define time 0)
;  
;  (for ([u (order (in-vertices G))] #:when (white? u))
;    (let dfs-visit ([u u])
;      (color-set! u GRAY)
;      (add1! time)
;      (d-set! u time)
;      (for ([v (in-neighbors G u)] #:when (white? v))
;        (π-set! v u)
;        (dfs-visit v))
;      (color-set! u BLACK)
;      (add1! time)
;      (f-set! u time)))
;  
;  (values color d π f))
;
;(define (dag? G)
;  (define-hashes color)
;  (define (white? v) (eq? WHITE (color v)))
;  (define (gray? v) (eq? GRAY (color v)))
;  (define (black? v) (eq? BLACK (color v)))
;  (for ([u (in-vertices G)]) (color-set! u WHITE))
;  
;  (for/and ([u (in-vertices G)] #:when (white? u))
;    (let dfs-visit ([u u])
;      (color-set! u GRAY)
;      (begin0
;        (for/and ([v (in-neighbors G u)])
;          (and (not (gray? v))
;               (or (black? v)
;                   (dfs-visit v))))
;        (color-set! u BLACK)))))
;
;(define (tsort G)
;  (define-values (color d π f) (dfs G))
;  (sort (hash-keys f) > #:key (λ (k) (hash-ref f k))))
;

;  
;;; tarjan algorithm
;(define (scc G)
;  (define i 0)
;  (define-hashes index lowlink)
;  (define S null)
;  (define (S-push x) (set! S (cons x S)))
;;  (define (S-pop) (begin0 (car S) (set! S (cdr S))))
;  
;  (define SCC null)
;  
;  (define (strongconnect v)
;    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
;    
;    (for ([w (in-neighbors G v)])
;      (cond [(not (hash-has-key? index w))
;             (strongconnect w)
;             (lowlink-set! v (min (lowlink v) (lowlink w)))]
;            [(member w S)
;             (lowlink-set! v (min (lowlink v) (index w)))]))
;    
;    (when (= (lowlink v) (index v)) ;; v is root node, so create an SCC
;      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
;      (set! SCC (cons (cons v new-scc) SCC))
;      (set! S (cdr S-rst))))
;  
;  (for ([v (in-vertices G)] #:unless (hash-has-key? index v)) (strongconnect v))
;  
;  SCC
;  )
