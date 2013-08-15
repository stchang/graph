#lang racket

(require "gen-graph.rkt"
         "hash-utils.rkt"
         "graph-fns-basic.rkt")

(provide (all-defined-out))
         
;; s is source, t is sink
;; this is the edmonds-karp algorithm O(VE^2)
;;   ie, ford-fulkerson + fewest-vertices-path to find augmenting path
(define (maxflow G s t)
  (define-hash f)
  (define (cf u v) (- (edge-weight G u v) (f (list u v) 0)))
  (define G-residual (graph-copy G))
  (let apath-loop ([augmenting-path (fewest-vertices-path G-residual s t)])
    (when augmenting-path
      (define-values (apath-rescap critical-edges) ; ie, cf(p)
        (let min-rescap-loop ([vs augmenting-path] 
                              [apath-rescap +inf.0] ; rescap of apath
                              [crit-edges null]) ; critical edges
          (cond
            [(null? (cdr vs)) (values apath-rescap crit-edges)]
            [else
             (define u (first vs)) (define v (second vs))
             (define rescap (cf u v))
             (cond 
               [(= rescap apath-rescap)
                (min-rescap-loop (cdr vs) rescap (cons (list u v) crit-edges))]
               [(< rescap apath-rescap)
                (min-rescap-loop (cdr vs) rescap (list (list u v)))]
               [else
                (min-rescap-loop (cdr vs) apath-rescap crit-edges)])])))
      (let flow-update-loop ([vs augmenting-path])
        (unless (null? (cdr vs))
          (define u (first vs)) (define v (second vs))
          (f-set! (list u v) (+ (f (list u v) 0) apath-rescap))
          (f-set! (list v u) (- (f (list u v))))
          (add-directed-edge! G-residual v u)
          (flow-update-loop (cdr vs))))
      (for ([e critical-edges])
        (remove-directed-edge! G-residual (first e) (second e)))
      (apath-loop (fewest-vertices-path G-residual s t))))
  ;; filter out negative flows
  (for/hash ([(k v) (in-hash f)] #:unless (negative? v)) (values k v)))