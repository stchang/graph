#lang racket

(require "hash-utils.rkt" 
         "gen-graph.rkt"
         "../queue/gen-queue.rkt"
         (only-in "../queue/priority.rkt" mk-empty-priority)
         "graph-fns-basic.rkt")
(require data/union-find)

;; minimum spanning tree fns

(provide (except-out (all-defined-out)))


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

;  (define Q (mk-empty-priority (λ (u v) (< (key u) (key v)))))
;
;  (define (init G r)
;    (for ([u (in-vertices G)])
;      (key-set! u +inf.0) (π-set! u #f) (in-Q?-set! u #t))
;    (key-set! r 0))
;    
;  ;; default bfs skips the visit if v has been enqueued (ie it's not "white")
;  ;; but here we want to skip only if v has been dequeued (ie it's "black")
;  (define (visit? G s u v) (in-Q? v))
;
;  (define (pre-visit G r u v)
;    (when (< (w u v) (key v)) ; relax
;      (π-set! v u)
;      (key-set! v (w u v))))
;  
;  (define (visit G s u) (in-Q?-set! u #f))
;      
;    
;  (define (finish G r)
;    (for/list ([v (in-vertices G)] #:unless (equal? v r)) (list (π v) v)))
;  
;  (bfs/generalized G r #:init-queue Q 
;                       #:init init
;                       #:visit? visit?
;                       #:pre-visit pre-visit
;                       #:visit visit
;                       #:return finish))
               