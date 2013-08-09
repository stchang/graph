#lang racket

(require "hash-utils.rkt" "gen-graph.rkt")

(provide (except-out (all-defined-out)))


;; single-source shortest path ------------------------------------------------

;; s = source
(define (bellman-ford G s)
;  (define weights (weighted-graph-weights G))
;  (define (w u v) (hash-ref weights (list u v)))
  (define (w u v) (edge-weight G u v))
  ;; init
  (define-hashes d π)
  (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
  (d-set! s 0)
  
  ;; compute result
  (for* ([i (in-range 1 (sub1 (length (in-vertices G))))]
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

;(define (dag-shortest-paths G s)
;  (define 
