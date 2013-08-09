#lang racket

(require "hash-utils.rkt")
(require "utils.rkt")
(require "gen-graph.rkt")

(provide (except-out (all-defined-out)))

;; ----------------------------------------------------------------------------
;; bfs and dfs

(require data/queue)

(define WHITE 'white)
(define BLACK 'black)
(define GRAY 'gray)

;; bfs : Graph Vertex -> 
;; s is the source vertex
(define (bfs G s)
  (define-hashes color d π)
  (define (white? v) (eq? WHITE (color v)))
  
  (for ([u (in-vertices G)])
    (cond [(equal? s u) (color-set! s GRAY)
                        (d-set!     s 0)
                        (π-set!     s #f)]
          [else         (color-set! u WHITE)
                        (d-set!     u +inf.0)
                        (π-set!     u #f)]))
  
  (define Q (make-queue))
  (enqueue! Q s)
  (let loop () (when (non-empty-queue? Q)
    (define u (dequeue! Q))
    (for ([v (in-neighbors G u)] #:when (white? v))
      (color-set! v GRAY)
      (d-set!     v (add1 (d u)))
      (π-set!     v u)
      (enqueue! Q v))
    (color-set! u BLACK)
    (loop)))
  (values color d π))

;; returns shortest path in G from source s to v
(define (shortest-path G s v)
  (define-values (color d π) (bfs G s))
  (reverse
   (let loop ([v v])
     (if (equal? v s) (list s)
         (let ([πv (hash-ref π v (λ() (error 'shortest-path "no vertex ~a in graph ~a" v G)))])
           (if πv (cons v (loop πv))
               (error 'shortest-path "no path from ~a to ~a in graph ~a" s v G)))))))


(define (dfs G #:order [order (λ (vs) vs)])
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes color d π f)
  (define (white? v) (eq? WHITE (color v)))
  (for ([u (in-vertices G)]) 
    (color-set! u WHITE)
    (π-set!     u #f))
  (define time 0)
  
  (for ([u (order (in-vertices G))] #:when (white? u))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (add1! time)
      (d-set! u time)
      (for ([v (in-neighbors G u)] #:when (white? v))
        (π-set! v u)
        (dfs-visit v))
      (color-set! u BLACK)
      (add1! time)
      (f-set! u time)))
  
  (values color d π f))

(define (dag? G)
  (define-hashes color)
  (define (white? v) (eq? WHITE (color v)))
  (define (gray? v) (eq? GRAY (color v)))
  (define (black? v) (eq? BLACK (color v)))
  (for ([u (in-vertices G)]) (color-set! u WHITE))
  
  (for/and ([u (in-vertices G)] #:when (white? u))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (begin0
        (for/and ([v (in-neighbors G u)])
          (and (not (gray? v))
               (or (black? v)
                   (dfs-visit v))))
        (color-set! u BLACK)))))

(define (tsort G)
  (define-values (color d π f) (dfs G))
  (sort (hash-keys f) > #:key (λ (k) (hash-ref f k))))

  
;; tarjan algorithm
(define (scc G)
  (define i 0)
  (define-hashes index lowlink)
  (define S null)
  (define (S-push x) (set! S (cons x S)))
;  (define (S-pop) (begin0 (car S) (set! S (cdr S))))
  
  (define SCC null)
  
  (define (strongconnect v)
    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
    
    (for ([w (in-neighbors G v)])
      (cond [(not (hash-has-key? index w))
             (strongconnect w)
             (lowlink-set! v (min (lowlink v) (lowlink w)))]
            [(member w S)
             (lowlink-set! v (min (lowlink v) (index w)))]))
    
    (when (= (lowlink v) (index v)) ;; v is root node, so create an SCC
      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
      (set! SCC (cons (cons v new-scc) SCC))
      (set! S (cdr S-rst))))
  
  (for ([v (in-vertices G)] #:unless (hash-has-key? index v)) (strongconnect v))
  
  SCC
  )
