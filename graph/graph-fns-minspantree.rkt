#lang racket

(require "hash-utils.rkt" 
         "gen-graph.rkt"
         "graph-fns-basic.rkt"
         (only-in "../queue/priority.rkt" mk-empty-priority))
(require data/union-find)

(provide (all-defined-out))

;; minimum spanning tree fns

;; kruskal --------------------------------------------------------------------
;; uses data/union-find

(define (mst-kruskal G)
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
;; uses priority queue based on in data/heap

;; r is root vertex
(define (mst-prim G r)
  (define (w u v) (edge-weight G u v))
  (define-hashes key π in-Q?)

  (do-bfs G r #:init-queue (mk-empty-priority (λ (u v) (< (key u) (key v))))
    #:init
      (for ([u (in-vertices G)])
        (key-set! u +inf.0) (π-set! u #f) (in-Q?-set! u #t))
      (key-set! r 0)
    ;; default bfs skips the visit if v has been enqueued (ie it's not "white")
    ;; but here we want to skip only if v has been dequeued (ie it's "black")
    #:visit? (to from) (in-Q? from)
    #:pre-visit (to from)
      (when (< (w to from) (key from)) ; relax
        (π-set! from to)
        (key-set! from (w to from)))
    #:visit (u) (in-Q?-set! u #f)
    #:return (for/list ([v (in-vertices G)] #:unless (equal? v r)) (list (π v) v))))
