#lang racket

(require "graph-property.rkt"
         "gen-graph.rkt"
         "graph-fns-basic.rkt" 
         "utils.rkt"
         (only-in "../queue/priority.rkt" mk-empty-priority))

(provide (all-defined-out))

;; single-source shortest path ------------------------------------------------

;; s = source
;; no neg weight cycles
(define (bellman-ford G s)
  (define (w u v) (edge-weight G u v))

  ;; init
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)
  (d-set! s 0)
  
  ;; compute result
  (for* ([_ (in-vertices G)]
         [e (in-edges G)])
    (match-define (list u v) e)
    ;; relax
    (when (> (d v) (+ (d u) (w u v)))
      (d-set! v (+ (d u) (w u v)))
      (π-set! v u)))
  
  ;; check for invalid graph (ie neg weight cycle)
  (for ([e (in-edges G)])
    (match-define (list u v) e)
    (when (> (d v) (+ (d u) (w u v)))
      (error 'bellman-ford "negative weight cycle")))
  
  (values (d->hash) (π->hash)))

(define (dag-shortest-paths G s)
  (define (w u v) (edge-weight G u v))
  (define tsorted (tsort G))
  
  ;; init
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)
  (d-set! s 0)

  (for* ([u tsorted]
         [v (in-neighbors G u)])
    ;; relax
    (when (> (d v) (+ (d u) (w u v)))
      (d-set! v (+ (d u) (w u v)))
      (π-set! v u)))
  
  (values (d->hash) (π->hash)))

;; Once a vertex is "visited" (ie dequeued), we know we have it's shortest path,
;; so we don't need to maintain a "seen" list. It's sufficient to check only for
;; shortest path improvement as criteria to add to the queue, and we're still
;; guaranteed termination.
;;
;; It's ok to re-enqueue a vertex that's already in the heap. It just means 
;; we've found an improvement and re-enqueueing essentially "re-heapifies"
;; the heap.
;; Notes:
;; - can't have negative weight edges
(define (dijkstra G s) 
  ;; (d v) represents current known shortest path from s to v
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)
  (define (w u v) (edge-weight G u v))

  (do-bfs G s #:init-queue (mk-empty-priority (λ (u v) (< (d u) (d v))))
    #:init (d-set! s 0)
    #:enqueue? (> (d $v) (+ (d $from) (w $from $v)))
    #:on-enqueue 
      (d-set! $v (+ (d $from) (w $from $v)))
      (π-set! $v $from)
    #:return (values (d->hash) (π->hash))))