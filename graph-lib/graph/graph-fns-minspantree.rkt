#lang racket/base

(require data/union-find racket/unsafe/ops)

(require "graph-property.rkt"
         "gen-graph.rkt"
         "graph-fns-basic.rkt"
         "utils.rkt"
         "graph-weighted.rkt"
         (only-in "../queue/priority.rkt" mk-empty-priority)
         (only-in "../queue/fifo.rkt" mk-empty-fifo))

(provide mst-kruskal mst-prim)

(define-syntax-rule (first x) (unsafe-car x))
(define-syntax-rule (second x) (unsafe-car (unsafe-cdr x)))

;; minimum spanning tree fns

;; kruskal --------------------------------------------------------------------
;; uses data/union-find

(define (mst-kruskal G)
  (define wgt (if (weighted-graph? G) (λ (e) (apply edge-weight G e)) (λ _ 1)))
  (define sorted-edges 
    (if (weighted-graph? G) (sort (get-edges G) < #:key wgt) (get-edges G)))
  
  ;; map vertex to it's representative set
  ;;  (different vertices may map to the same rep set)
  (define-vertex-property G dset #:init (uf-new $v))
  
  (for/fold ([A null]) ([e sorted-edges]
    #:unless (vertex=? G (uf-find (dset (first e))) 
                         (uf-find (dset (second e)))))
    (uf-union! (dset (first e)) (dset (second e)))
    (cons e A)))

;; prim -----------------------------------------------------------------------
;; uses priority queue based on in data/heap

;; The "visited" vertices (ie vertices that have been enqueued then dequeued)
;; represent the current mst.
;; The vertices in the priority queue (ie heap) represent the candidate "next"
;; vertices to add to the mst.
;; It might seem like the visited? check is not needed, since we're adding the 
;; lowest weight edge on each iteration and then only enqueueing if there's
;; improvement, but the check is needed to ensure all the edges form a mst.
;; If we omit the check, I think we're only guaranteed a cover set.
;;
;; If a vertex v is already in the mst, then:
;; - ((π v) v) is the edge connected v to the rest of the tree
;; - (cur-min-wgt v) is the weight of that edge
;; If a vertex v is a candidate vertex (ie, in the heap), then:
;; - (π v) is the vertex that would connect v to the mst
;; - where (π v) is chosen among all possible edges from v to the mst bc it has
;;   the lowest weight, where (cur-min-wgt v) is that weight
;;
;; On each iteration:
;; - candidate vertex with lowest weight connecting edge is added to the mst
;; - neighbors of that vertex are added as candidates
;;
;; Notes:
;; - a vertex can be "enqueued" agani even if it's already in the heap, 
;;   if another edge of that vertex is later discovered to be cheaper than the
;;   currently known cheapest edge
;; - thus re-enqueueing a vertex that's already in the heap effectively
;;   "re-heapifies" the heap with the new cost information
(define (mst-prim G root-v)
  (define wgt (if (weighted-graph? G) (λ (u v) (edge-weight G u v)) (λ _ 1)))
  ; (cur-min-wgt v) is current known min wgt edge connecting v to the mst
  (define-vertex-property G cur-min-wgt #:init +inf.0) 
  (define-vertex-property G π #:init #f)

  (define hp 
    (if (weighted-graph? G)
        (mk-empty-priority (λ (u v) (< (cur-min-wgt u) (cur-min-wgt v))))
        (mk-empty-fifo)))
  
  (do-bfs G root-v #:init-queue: hp
    #:init: (cur-min-wgt-set! root-v 0)
    ;; default bfs skips visit if v is discovered (ie it's been seen before) 
    ;; (ie enqueued or visited) (ie not "white")
    ;; but we skip only if v has been visited (ie in the mst) (ie it's "black")
    ;; but we re-enqueue if we discover lower cost information for the vertex
    #:enqueue?: (and (not ($visited? $v))
                     (< (wgt $from $v)
                        (cur-min-wgt $v)))
    #:on-enqueue: (cur-min-wgt-set! $v (wgt $from $v)) 
                  (π-set! $v $from)
    ;; return list of edges in the mst
    #:return: (for/list ([v (in-vertices G)] #:unless (vertex=? G v root-v))
               (list (π v) v))))
