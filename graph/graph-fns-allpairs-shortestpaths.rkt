#lang racket/base

(require racket/list racket/match)

(require "graph-property.rkt"
         "gen-graph.rkt"
         "graph-fns-basic.rkt"
         "graph-fns-singlesource-shortestpaths.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; All all-pairs, shortest paths algorithms assume:
;; - all self edges have weight 0
;; - no negative weight cycles
;; - non-existent edges have weight +inf.0

(define (extend-shortest-paths W lw w)
  (define-edge-property W new-L
    #:for-each
    (new-L-set! $from $to +inf.0)
    (for ([k (in-vertices W)])
      (new-L-set! $from $to (min (new-L $from $to) (+ (lw $from k) (w k $to))))))
  (new-L->hash))

;; ie, the matrix multiplication method
(define (all-pairs-shortest-paths/slow W)
  (define (w u v) (if (vertex=? W u v) 0 (edge-weight W u v)))
  ;; Ls is a hash mapping a vertex to a function that maps an edge to its weight
  (define-vertex-property W Ls)
  (define vs (get-vertices W))
  (Ls-set! (car vs) w)
  (for/last ([m-1 vs] ; traverse two offset vs list in parallel
             [m (drop-right (cdr vs) 1)]) ; drop last bc (L m-1) == (L m)
    (define new-L-hash (extend-shortest-paths W (Ls m-1) w))
    (Ls-set! m (λ (u v) (if (vertex=? W u v) 0 
                            (hash-ref new-L-hash (list u v) +inf.0))))
    new-L-hash))

;; ie, the repeat matrix squaring method
;; something wrong with this --- see test for g25.2
(define (all-pairs-shortest-paths/faster W)
  (define (w u v) (if (vertex=? W u v) 0 (edge-weight W u v)))
  (define-vertex-property W Ls)
  (define vs (get-vertices W))
  (Ls-set! (car vs) w)
  ;; want every (2^n)th vertex
  (let loop ([base 1] [m (car vs)] [last-L null])
    (define to-drop (sub1 (expt 2 base)))
    (cond 
      [(> (length vs) to-drop)
       (define 2m (car (drop vs to-drop)))
       (define new-L (extend-shortest-paths W (Ls m) (Ls m)))
       (Ls-set! 2m (λ (u v) (if (vertex=? W u v) 0 (hash-ref new-L (list u v) +inf.0))))
       (loop (add1 base) 2m new-L)]
      [else last-L])))

(define (floyd-warshall W)
  (define (w u v) (if (vertex=? W u v) 0 (edge-weight W u v)))
  (define v0 (gensym)) ; create new vertex
  (define-vertex-property W Ds)
  (Ds-set! v0 w)
  (define vs (get-vertices W))
  (for/last ([k-1 (cons v0 vs)] [k vs])
    (define-edge-property W new-D
      #:init (min ((Ds k-1) $from $to) (+ ((Ds k-1) $from k) ((Ds k-1) k $to))))
    (Ds-set! k (λ (u v) (if (vertex=? W u v) 0 (new-D u v #:default +inf.0))))
    (new-D->hash)))

;; uses Bellman-Ford to eliminate negative edges,
;;   then runs dijkstra for each vertex in G
;; should be faster than floyd-warshall for sparse graphs
(define (johnson _g)
  (define G (graph-copy _g)) ; copy input _g because we will change weights
  (define G-prime (graph-copy G))
  (define vs-in-G (in-vertices G))
  (define s (gensym))
  (for ([v vs-in-G]) (add-directed-edge! G-prime s v 0))
  (define-values (δbf πbf) (bellman-ford G-prime s))
  (define-vertex-property G h #:init (hash-ref δbf $v))
  (for ([e (in-edges G)])
    (match-define (list u v) e)
    (add-directed-edge! G u v (+ (edge-weight _g u v) (h u) (- (h v)))))
  (define-vertex-property G dij #:init (let-values ([(δ _) (dijkstra G $v)]) δ))
  (define-edge-property G D
    #:init (+ (hash-ref (dij $from) $to) (h $to) (- (h $from))))
  (D->hash))

(define (transitive-closure G)
  ;; Ts maps vertices to an edge property hash
  (define-vertex-property G Ts)

  ;; add Ts[v0]
  (define v0 (gensym))
  (define-edge-property G new-T 
    #:init (or (vertex=? G $from $to) (has-edge? G $from $to)))
  (Ts-set! v0 (new-T->hash))
  
  (define vs (get-vertices G))
  (for/last ([k-1 (cons v0 vs)] [k vs])
    (define-edge-property G new-T
      #:init
      (or (hash-ref (Ts k-1) (list $from $to))
          (and (hash-ref (Ts k-1) (list $from k))
               (hash-ref (Ts k-1) (list k $to)))))
    (define new-T-hash (new-T->hash))
    (Ts-set! k new-T-hash)
    new-T-hash))