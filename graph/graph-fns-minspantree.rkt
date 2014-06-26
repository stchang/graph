#lang racket

(require "graph-property.rkt"
         "gen-graph.rkt"
         "graph-fns-basic.rkt"
         (only-in "../queue/priority.rkt" mk-empty-priority))
(require data/union-find)

(provide (all-defined-out))

;; minimum spanning tree fns

;; kruskal --------------------------------------------------------------------
;; uses data/union-find

(define (mst-kruskal G)
  (define (w e) (apply edge-weight G e))
  (define A null) (define (A-add! e) (set! A (cons e A)))
  
  ;; map vertex to it's representative set
  ;; (different vertices may map to the same rep set)
  (define-vertex-property G dset #:init (uf-new $v))
  
  (define sorted-edges (sort (get-edges G) < #:key w))

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
  (define-vertex-property G key #:init +inf.0)
  (define-vertex-property G π #:init #f)
  (define-vertex-property G in-Q? #:init #t)

  (do-bfs G r #:init-queue (mk-empty-priority (λ (u v) (< (key u) (key v))))
    #:init (key-set! r 0)
    ;; default bfs skips the visit if v has been discovered (ie it's not "white")
    ;; but here we want to skip only if v has been dequeued (ie it's "black")
    #:visit? (to from) (in-Q? from)
    #:discover (to from)
      (when (< (w to from) (key from)) ; relax
        (π-set! from to)
        (key-set! from (w to from)))
    #:visit (u) (in-Q?-set! u #f)
    #:return (for/list ([v (in-vertices G)] #:unless (equal? v r)) (list (π v) v))))
