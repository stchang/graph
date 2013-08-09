#lang racket

(require "hash-utils.rkt" "utils.rkt" "gen-graph.rkt")
(require racket/generator data/union-find data/heap)

; weighted, adjacency-list graph

(provide (except-out (all-defined-out)))

;; (internal graph functions and names have a @ suffix)

;; A WeightedGraph is a (weighted-graph AdjacencyList Weights)
(struct weighted-graph (adjlist weights) #:transparent
  #:methods gen:graph
  [(define (in-vertices g) (in-graph-vertices g))
   (define (in-neighbors g v) (in-graph-neighbors g v))
   (define (in-edges g) (in-graph-edges g))
   (define (edge-weight g u v) (hash-ref (weighted-graph-weights g) (list u v)))])

;; An AdjacencyList is a [MutableHashOf Vertex -> Vertex]
;;   and is the internal graph representation

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
(define (in-graph-vertices g) (hash-keys (weighted-graph-adjlist g)))

;; returns neighbors as a sequence
(define (in-graph-neighbors g v) (in-set (hash-ref (weighted-graph-adjlist g) v (set))))

;; returns edges as a sequence
(define (in-graph-edges g)
  (in-generator 
   (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
     (yield (list u v)))))
  

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)
(define (add-edge@ adj u v) (hash-update! adj u (λ (vs) (set-add vs v)) (set)))
(define (add-vertex@ adj v) (hash-update! adj v (λ (vs) vs) (set)))


;
;;; min span tree (mst) --------------------------------------------------------
;
;;; kruskal --------------------------------------------------------------------
;;; uses data/union-find
;
;(define (mst-kruskal G)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
;  (define A null) (define (A-add! e) (set! A (cons e A)))
;  
;  ;; hash mapping vertex to it's representative set
;  ;; different vertices may map to the same rep set
;  (define-hash dset)
;  
;  (for ([v (in-vertices G)]) (dset-set! v (uf-new v)))
;  
;  (define sorted-edges 
;    (sort (sequence->list (in-edges G)) < #:key (λ (e) (apply w e))))
;
;  (for ([e sorted-edges])
;    (match-define (list u v) e)
;    (unless (equal? (uf-find (dset u)) (uf-find (dset v)))
;      (A-add! e)
;      (uf-union! (dset u) (dset v))))
;  
;  A)
;
;;; prim -----------------------------------------------------------------------
;;; uses priority queue in data/heap
;
;;; r is root vertex
;(define (mst-prim G r)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
;  (define-hashes key π in-Q?)
;  
;  (for ([u (in-vertices G)]) 
;    (key-set! u +inf.0) (π-set! u #f) (in-Q?-set! u #t))
;  
;
;  (define Q (make-heap (λ (u v) (< (key u) (key v)))))
;  
;  ;; delay adding the vertices to Q to save on re-heapifying after key changes
;  ;; separately keep from of Q "membership" with in-Q? hash
;  ;(heap-add-all! Q (in-vertices G))
;
;  (key-set! r 0)
;  (heap-add! Q r)
;  
;  (let loop ([u (heap-min Q)])
;    ;; remove all (possibly duplicate) copies of u and mark u as not in Q
;    (let remove-loop ()
;      (heap-remove-min! Q)
;      (when (and (not (zero? (heap-count Q))) 
;                 (equal? (heap-min Q) u))
;        (remove-loop)))
;    (in-Q?-set! u #f)
;    
;    (for ([v (in-neighbors G u)]
;          #:when 
;          (and (in-Q? v) (< (w u v) (key v))))
;      (π-set! v u)
;      (key-set! v (w u v))
;      (heap-add! Q v)) ; add v to Q when its key changes
;    
;    (unless (zero? (heap-count Q)) (loop (heap-min Q))))
;  
;  (for/list ([v (in-vertices G)] #:unless (equal? v r)) (list v (π v))))
;  
;
;;; single-source shortest path ------------------------------------------------
;
;;; s = source
;(define (bellman-ford G s)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
;  ;; init
;  (define-hashes d π)
;  (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
;  (d-set! s 0)
;  
;  ;; compute result
;  (for* ([i (in-range 1 (sub1 (length (in-vertices G))))]
;         [e (in-edges G)])
;    (match-define (list u v) e)
;    ;; relax
;    (when (> (d v) (+ (d u) (apply w e)))
;      (d-set! v (+ (d u) (w u v)))
;      (π-set! v u)))
;  
;  ;; check for invalid graph (ie neg weight cycle)
;  (for/and ([e (in-edges G)])
;    (match-define (list u v) e)
;    (when (> (d v) (+ (d u) (w u v))) 
;      (error 'bellman-ford "negative weight cycle")))
;  
;  (values d π))
;
;(define (dag-shortest-paths G s)
;  (define 
;
;  ;;; ----------------------------------------------------------------------------
;;;; bfs and dfs
;;
;;(require data/queue)
;;
;;(define WHITE 'white)
;;(define BLACK 'black)
;;(define GRAY 'gray)
;;
;;;; bfs : Graph Vertex -> 
;;;; s is the source vertex
;;(define (bfs G s)
;;  (define-hashes color d π)
;;  (define (white? v) (eq? WHITE (color v)))
;;  
;;  (for ([u (in-vertices G)])
;;    (cond [(equal? s u) (color-set! s GRAY)
;;                        (d-set!     s 0)
;;                        (π-set!     s #f)]
;;          [else         (color-set! u WHITE)
;;                        (d-set!     u +inf.0)
;;                        (π-set!     u #f)]))
;;  
;;  (define Q (make-queue))
;;  (enqueue! Q s)
;;  (let loop () (when (non-empty-queue? Q)
;;    (define u (dequeue! Q))
;;    (for ([v (in-neighbors G u)] #:when (white? v))
;;      (color-set! v GRAY)
;;      (d-set!     v (add1 (d u)))
;;      (π-set!     v u)
;;      (enqueue! Q v))
;;    (color-set! u BLACK)
;;    (loop)))
;;  (values color d π))
;;
;;;; returns shortest path in G from source s to v
;;(define (shortest-path G s v)
;;  (define-values (color d π) (bfs G s))
;;  (reverse
;;   (let loop ([v v])
;;     (if (equal? v s) (list s)
;;         (let ([πv (hash-ref π v (λ() (error 'shortest-path "no vertex ~a in graph ~a" v G)))])
;;           (if πv (cons v (loop πv))
;;               (error 'shortest-path "no path from ~a to ~a in graph ~a" s v G)))))))
;;
;;
;;(define (dfs G #:order [order (λ (vs) vs)])
;;  ;; d[u] = discovery time, f[u] = finishing time
;;  (define-hashes color d π f)
;;  (define (white? v) (eq? WHITE (color v)))
;;  (for ([u (in-vertices G)]) 
;;    (color-set! u WHITE)
;;    (π-set!     u #f))
;;  (define time 0)
;;  
;;  (for ([u (order (in-vertices G))] #:when (white? u))
;;    (let dfs-visit ([u u])
;;      (color-set! u GRAY)
;;      (add1! time)
;;      (d-set! u time)
;;      (for ([v (in-neighbors G u)] #:when (white? v))
;;        (π-set! v u)
;;        (dfs-visit v))
;;      (color-set! u BLACK)
;;      (add1! time)
;;      (f-set! u time)))
;;  
;;  (values color d π f))
;;
;;(define (dag? G)
;;  (define-hashes color)
;;  (define (white? v) (eq? WHITE (color v)))
;;  (define (gray? v) (eq? GRAY (color v)))
;;  (define (black? v) (eq? BLACK (color v)))
;;  (for ([u (in-vertices G)]) (color-set! u WHITE))
;;  
;;  (for/and ([u (in-vertices G)] #:when (white? u))
;;    (let dfs-visit ([u u])
;;      (color-set! u GRAY)
;;      (begin0
;;        (for/and ([v (in-neighbors G u)])
;;          (and (not (gray? v))
;;               (or (black? v)
;;                   (dfs-visit v))))
;;        (color-set! u BLACK)))))
;;
;;(define (tsort G)
;;  (define-values (color d π f) (dfs G))
;;  (sort (hash-keys f) > #:key (λ (k) (hash-ref f k))))
;;
;;
;;(define (transpose G)
;;  (define adj^T (make-hash))
;;  (for ([u (in-vertices G)])
;;    (add-vertex@ adj^T u)
;;    (for ([v (in-neighbors G u)])
;;      (add-edge@ adj^T v u)))
;;  (graph adj^T))
;;  
;;;; tarjan algorithm
;;(define (scc G)
;;  (define i 0)
;;  (define-hashes index lowlink)
;;  (define S null)
;;  (define (S-push x) (set! S (cons x S)))
;;;  (define (S-pop) (begin0 (car S) (set! S (cdr S))))
;;  
;;  (define SCC null)
;;  
;;  (define (strongconnect v)
;;    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
;;    
;;    (for ([w (in-neighbors G v)])
;;      (cond [(not (hash-has-key? index w))
;;             (strongconnect w)
;;             (lowlink-set! v (min (lowlink v) (lowlink w)))]
;;            [(member w S)
;;             (lowlink-set! v (min (lowlink v) (index w)))]))
;;    
;;    (when (= (lowlink v) (index v)) ;; v is root node, so create an SCC
;;      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (eq? w v)))))
;;      (set! SCC (cons (cons v new-scc) SCC))
;;      (set! S (cdr S-rst))))
;;  
;;  (for ([v (in-vertices G)] #:unless (hash-has-key? index v)) (strongconnect v))
;;  
;;  SCC
;;  )
