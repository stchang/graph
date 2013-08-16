#lang racket

(require "hash-utils.rkt"
         "gen-graph.rkt"
         "graph-fns-basic.rkt"
         "graph-fns-singlesource-shortestpaths.rkt")

(provide (all-defined-out))

;; All all-pairs, shortest paths algorithms assume:
;; - all self edges have weight 0
;; - no negative weight cycles
;; - non-existent edges have weight +inf.0

(define (extend-shortest-paths vs lw w)
  (define-hash new-L)
  (for* ([i vs]
         [j vs])
    (new-L-set! (list i j) +inf.0)
    (for ([k vs])
      (new-L-set! (list i j) (min (new-L (list i j)) (+ (lw i k) (w k j))))))
  new-L)

;; ie, the matrix multiplication method
(define (all-pairs-shortest-paths/slow W)
  (define (w u v) (if (equal? u v) 0 (edge-weight W u v)))
  ;; Ls is a hash mapping a vertex to a function that maps an edge to its weight
  (define-hash Ls)
  (define vs (in-vertices W))
  (Ls-set! (car vs) w)
  (for/last ([m-1 vs] ; traverse two offset vs list in parallel
             [m (drop-right (cdr vs) 1)]) ; drop last bc (L n-1) == (L n)
    (define new-L (extend-shortest-paths vs (Ls m-1) w))
    (Ls-set! m (λ(u v) (if (equal? u v) 0 (hash-ref new-L (list u v) +inf.0))))
    new-L))

;; ie, the repeat matrix squaring method
;; something wrong with this --- see test for g25.2
(define (all-pairs-shortest-paths/faster W)
  (define (w u v) (if (equal? u v) 0 (edge-weight W u v)))
  (define-hash Ls)
  (define vs (in-vertices W))
  (Ls-set! (car vs) w)
  ;; want every (2^n)th vertex
  (let loop ([base 1] [m (car vs)] [last-L null])
    (define to-drop (sub1 (expt 2 base)))
    (cond 
      [(> (length vs) to-drop)
       (define 2m (car (drop vs to-drop)))
       (define new-L (extend-shortest-paths vs (Ls m) (Ls m)))
       (Ls-set! 2m (λ(u v) (if (equal? u v) 0 (hash-ref new-L (list u v) +inf.0))))
       (loop (add1 base) 2m new-L)]
      [else last-L])))

(define (floyd-warshall W)
  (define (w u v) (if (equal? u v) 0 (edge-weight W u v)))
  (define vs (in-vertices W))
  (define v0 (gensym))
  (define-hash Ds)
  (Ds-set! v0 w)
  (for/last ([k-1 (cons v0 vs)] [k vs])
    (define-hash new-D)
    (for* ([i vs] [j vs])
      (new-D-set! (list i j)
                  (min ((Ds k-1) i j) (+ ((Ds k-1) i k) ((Ds k-1) k j)))))
    (Ds-set! k (λ (u v) (if (equal? u v) 0 (hash-ref new-D (list u v) +inf.0))))
    new-D))

;; uses Bellman-Ford to eliminate negative edges,
;;   then runs dijkstra for each vertex in G
;; should be faster than floyd-warshall for sparse graphs
(define (johnson _g)
  (define G (graph-copy _g)) ; copy input _g because we will change weights
  (define G-prime (graph-copy G))
  (define vs-in-G (in-vertices G))
  (define s (gensym))
  (define-hashes h D)
  (for ([v vs-in-G]) (add-directed-edge! G-prime s v 0))
  (define-values (δbf πbf) (bellman-ford G-prime s))
  (for ([v vs-in-G]) (h-set! v (hash-ref δbf v)))
  (for ([e (in-edges G)])
    (match-define (list u v) e)
    (add-directed-edge! G u v (+ (edge-weight _g u v) (h u) (- (h v)))))
  (for ([u vs-in-G])
    (define-values (δu πu) (dijkstra G u))
    (for ([v vs-in-G])
      (D-set! (list u v) (+ (hash-ref δu v) (h v) (- (h u))))))
  D)

(define (transitive-closure G)
  (define vs (in-vertices G))
  (define-hash Ts)

  ;; add Ts[v0]
  (define v0 (gensym))
  (define-hash new-T)
  (for* ([i vs] [j vs])
    (if (or (equal? i j) (has-edge? G i j)) 
        (new-T-set! (list i j) #t) 
        (new-T-set! (list i j) #f)))
  (Ts-set! v0 new-T)
  
  (for/last ([k-1 (cons v0 vs)] [k vs])
    (define-hash new-T)
    (for* ([i vs] [j vs])
      (new-T-set! (list i j) (or (hash-ref (Ts k-1) (list i j))
                                 (and (hash-ref (Ts k-1) (list i k))
                                      (hash-ref (Ts k-1) (list k j))))))
    (Ts-set! k new-T)
    new-T))