#lang racket

(require "gen-graph.rkt"
         "graph-property.rkt"
         "graph-fns-basic.rkt")

(provide (all-defined-out))
         
;; only works for directed graphs, otherwise edges get counted twice
;; s is source, t is sink
;; this is the edmonds-karp algorithm O(VE^2)
;;   ie, ford-fulkerson + fewest-vertices-path to find augmenting path
(define (maxflow G s t)
  (define G-residual (graph-copy G))
  (define-edge-property G-residual f)
  (define (cf u v) (- (edge-weight G-residual u v) (f u v #:default 0)))
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
          (f-set! u v (+ (f u v #:default 0) apath-rescap))
          (f-set! v u (- (f u v)))
          (add-directed-edge! G-residual v u)
          (flow-update-loop (cdr vs))))
      (for ([e critical-edges])
        (remove-directed-edge! G-residual (first e) (second e)))
      (apath-loop (fewest-vertices-path G-residual s t))))
  ;; filter out negative flows
  (for/hash ([(k v) (in-hash f)] #:when (positive? v)) (values k v)))

; linear time bipartite check (via 2-coloring)
(define (bipartite? G)
  (define L null) (define (add-to-L! v) (set! L (cons v L))) ; #f
  (define R null) (define (add-to-R! v) (set! R (cons v R))) ; #t
  (define-vertex-property G color) ; key = vertices, values = #t/#f
  (define-graph-property not-bipartite? #f)
  (do-dfs G #:break get-not-bipartite?
   #:prologue (parent v) 
   (color-set! v (and parent (not (color parent))))
   (if (color v) (add-to-L! v) (add-to-R! v))
   #:process-unvisited? (from to) (and from (xor (not (color from)) (color to)))
   #:process-unvisited (from to) (set! not-bipartite? #t))
  (and (not not-bipartite?) (list L R)))
   
(define (maximum-bipartite-matching G)
  (define L-R (bipartite? G))
  (unless L-R (error 'maximum-bipartite-matching "not given a bipartite graph"))
  (define L (second L-R)) (define R (first L-R))
  (define s (gensym)) (define t (gensym))
  (define G-prime (graph-copy G))
  (for ([u L]) 
    (add-directed-edge! G-prime s u)
    (for ([v (in-neighbors G u)]) ; neighbors should all be in R
      (remove-directed-edge! G-prime v u)))
  (for ([v R]) (add-directed-edge! G-prime v t))
  (define res (maxflow G-prime s t))
  (for/list ([e (in-hash-keys res #;(maxflow G-prime s t))]
             #:unless (or (eq? (first e) s) (eq? (second e) t))) e))
  