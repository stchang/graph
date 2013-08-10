#lang racket

(require "hash-utils.rkt"
         "graph-matrix.rkt"
         "gen-graph.rkt")

(provide (all-defined-out))

(define (extend-shortest-paths vs lw w)
  (define-hash new-L)
  (for* ([i vs]
         [j vs])
    (new-L-set! (list i j) +inf.0)
    (for ([k vs])
      (new-L-set! (list i j) (min (new-L (list i j)) (+ (lw i k) (w k j))))))
  new-L)

(define (all-pairs-shortest-paths/slow W)
  (define (w u v) (edge-weight W u v))
  ;; Ls is a hash mapping a vertex to a hash that maps an edge to its weight
  (define-hash Ls)
  (define vs (in-vertices W))
  (Ls-set! (car vs) (matrix-graph-weights W))
  (for/last ([m-1 vs] ; traverse two offset vs list in parallel
             [m (drop-right (cdr vs) 1)]) ; drop last bc (L n-1) == (L n)
    (Ls-set! m (extend-shortest-paths 
               vs 
               (λ (u v) (if (equal? u v) 0 
                            (hash-ref (Ls m-1) (list u v) +inf.0)))
               w))
    (Ls m)))

;; something wrong with this --- see test for g25.2
(define (all-pairs-shortest-paths/faster W)
  (define-hash Ls)
  (define vs (in-vertices W))
  (Ls-set! (car vs) (matrix-graph-weights W))
  ;; want every (2^n)th vertex
  (let loop ([base 1] [m (car vs)])
    (define to-drop (sub1 (expt 2 base)))
    (cond 
      [(> (length vs) to-drop)
       (define 2m (car (drop vs to-drop)))
       (Ls-set! 2m (extend-shortest-paths 
                    vs 
                    (λ (u v) (if (equal? u v) 0 
                                 (hash-ref (Ls m) (list u v) +inf.0)))
                    (λ (u v) (if (equal? u v) 0 
                                 (hash-ref (Ls m) (list u v) +inf.0)))))
       (loop (add1 base) 2m)]
      [else (Ls m)])))

(define (floyd-warshall W)
  (define vs (in-vertices W))
  (define v0 (gensym))
  (define-hash Ds)
  (Ds-set! v0 (matrix-graph-weights W))
  (for/last ([k-1 (cons v0 vs)]
             [k vs])
    (define-hash new-D)
    (for* ([i vs] [j vs])
      (new-D-set! 
       (list i j)
       (min (if (equal? i j) 0 (hash-ref (Ds k-1) (list i j) +inf.0))
            (+ (if (equal? i k) 0 (hash-ref (Ds k-1) (list i k) +inf.0))
               (if (equal? k j) 0 (hash-ref (Ds k-1) (list k j) +inf.0))))))
    (Ds-set! k new-D)
    new-D))