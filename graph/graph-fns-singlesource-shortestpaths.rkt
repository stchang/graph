#lang racket

(require "hash-utils.rkt" 
         "gen-graph.rkt"
         "graph-fns-basic.rkt" 
         (only-in "../queue/priority.rkt" mk-empty-priority)
         "graph-fns-basic.rkt")

(provide (except-out (all-defined-out)))


;; single-source shortest path ------------------------------------------------

;; s = source
;; no neg weight cycles
(define (bellman-ford G s)
  (define (w u v) (edge-weight G u v))

  ;; init
  (define-hashes d π)
  (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
  (d-set! s 0)
  
  ;; compute result
  (for* ([_ (in-vertices G)]
         [e (in-edges G)])
    (match-define (list u v) e)
    ;; relax
    (when (> (d v) (+ (d u) (apply w e)))
      (d-set! v (+ (d u) (w u v)))
      (π-set! v u)))
  
  ;; check for invalid graph (ie neg weight cycle)
  (for ([e (in-edges G)])
    (match-define (list u v) e)
    (when (> (d v) (+ (d u) (w u v)))
      (error 'bellman-ford "negative weight cycle")))
  
  (values d π))

(define (dag-shortest-paths G s)
  (define (w u v) (edge-weight G u v))
  (define tsorted (tsort G))
  
  ;; init
  (define-hashes d π)
  (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
  (d-set! s 0)

  (for* ([u tsorted]
         [v (in-neighbors G u)])
    ;; relax
    (when (> (d v) (+ (d u) (w u v)))
      (d-set! v (+ (d u) (w u v)))
      (π-set! v u)))
  
  (values d π))

;; no negative weight edges
(define (dijkstra G s) 
  ;; (d v) represents intermediate known shortest path from s to v
  (define-hashes d π)
  (define (w u v) (edge-weight G u v))

  (do-bfs G s #:init-queue (mk-empty-priority (λ (u v) (< (d u) (d v))))
    #:init
      (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
      (d-set! s 0)
    #:visit? (to from) (> (d from) (+ (d to) (w to from)))
    #:pre-visit (to from)
      (d-set! from (+ (d to) (w to from)))
      (π-set! from to)
    #:return (values d π)))

          
          
;  (define Q (mk-empty-priority (λ (u v) (< (d u) (d v)))))
;  (define (init G s)
;    (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
;    (d-set! s 0))
;    
;  (define (visit? G s u v) (> (d v) (+ (d u) (w u v))))
;    
;  (define (pre-visit G s u v)
;    (d-set! v (+ (d u) (w u v)))
;    (π-set! v u))
;    
;  (define (finish G s) (values d π))
;  
;  (bfs/generalized G s #:init-queue Q
;                       #:init init
;                       #:visit? visit?
;                       #:pre-visit pre-visit
;                       #:return finish))