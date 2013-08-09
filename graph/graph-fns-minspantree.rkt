#lang racket

(require "hash-utils.rkt" "gen-graph.rkt")
(require data/union-find data/heap)

;; minimum spanning tree fns

(provide (except-out (all-defined-out)))


;; kruskal --------------------------------------------------------------------
;; uses data/union-find

(define (mst-kruskal G)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
  (define (w u v) (edge-weight G u v))
  (define A null) (define (A-add! e) (set! A (cons e A)))
  
  ;; hash mapping vertex to it's representative set
  ;; different vertices may map to the same rep set
  (define-hash dset)
  
  (for ([v (in-vertices G)]) (dset-set! v (uf-new v)))
  
  (define sorted-edges 
    (sort (sequence->list (in-edges G)) < #:key (λ (e) (apply w e))))

  (for ([e sorted-edges])
    (match-define (list u v) e)
    (unless (equal? (uf-find (dset u)) (uf-find (dset v)))
      (A-add! e)
      (uf-union! (dset u) (dset v))))
  
  A)

;; prim -----------------------------------------------------------------------
;; uses priority queue in data/heap

;; r is root vertex
(define (mst-prim G r)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
  (define (w u v) (edge-weight G u v))
  (define-hashes key π in-Q?)
  
  (for ([u (in-vertices G)]) 
    (key-set! u +inf.0) (π-set! u #f) (in-Q?-set! u #t))
  

  (define Q (make-heap (λ (u v) (< (key u) (key v)))))
  
  ;; delay adding the vertices to Q to save on re-heapifying after key changes
  ;; separately keep from of Q "membership" with in-Q? hash
  ;(heap-add-all! Q (in-vertices G))

  (key-set! r 0)
  (heap-add! Q r)
  
  (let loop ([u (heap-min Q)])
    ;; remove all (possibly duplicate) copies of u and mark u as not in Q
    (let remove-loop ()
      (heap-remove-min! Q)
      (when (and (not (zero? (heap-count Q))) 
                 (equal? (heap-min Q) u))
        (remove-loop)))
    (in-Q?-set! u #f)
    
    (for ([v (in-neighbors G u)]
          #:when 
          (and (in-Q? v) (< (w u v) (key v))))
      (π-set! v u)
      (key-set! v (w u v))
      (heap-add! Q v)) ; add v to Q when its key changes
    
    (unless (zero? (heap-count Q)) (loop (heap-min Q))))
  
  (for/list ([v (in-vertices G)] #:unless (equal? v r)) (list v (π v))))
  
