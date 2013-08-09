#lang racket

(require "hash-utils.rkt" 
         "gen-graph.rkt"
         "graph-fns-basic.rkt")
(require data/heap)

(provide (except-out (all-defined-out)))


;; single-source shortest path ------------------------------------------------

;; s = source
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

(define (dijkstra G s)
  (define (w u v) (edge-weight G u v))

  ;; init
  (define-hashes d π)
  (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
  (d-set! s 0)

  ;(define S null)
  
  (define Q (make-heap (λ (u v) (< (d u) (d v)))))
  
  (heap-add! Q s)
  
  (let loop ([u (heap-min Q)])
    ;; remove all (possibly duplicate) copies of u and mark u as not in Q
    (let remove-loop ()
      (heap-remove-min! Q)
      (when (and (not (zero? (heap-count Q))) 
                 (equal? (heap-min Q) u))
        (remove-loop)))
    
   ; (set! S (cons u S))
    
    (for ([v (in-neighbors G u)])
      ;; relax
      (when (> (d v) (+ (d u) (w u v)))
        (d-set! v (+ (d u) (w u v)))
        (π-set! v u)
        (heap-add! Q v))) ; add v to Q when its d changes
    
    (unless (zero? (heap-count Q)) (loop (heap-min Q))))

  (values d π))