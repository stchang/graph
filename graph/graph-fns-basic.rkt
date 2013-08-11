#lang racket

(require "hash-utils.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo)
         "../queue/gen-queue.rkt")

(provide (except-out (all-defined-out)))


;; ----------------------------------------------------------------------------
;; bfs and dfs

(define (bfs G s)
  (define-hashes color d π)
    
  (define (init G s)
    (for ([u (in-vertices G)]) 
      (color-set! u WHITE) (d-set! u +inf.0) (π-set! u #f))
    (color-set! s GRAY) (d-set! s 0) (π-set! s #f))
    
  (define (process-neighbor? G u v) (white? (color v)))
    
  (define (process-neighbor G u v)
    (color-set! v GRAY)
    (d-set! v (add1 (d u)))
    (π-set! v u))
    
  (define (post-visit u) (color-set! u BLACK))
    
  (define (finish G s) (values color d π))

  (bfs/generic G s #:init init
                   #:process-neighbor? process-neighbor?
                   #:process-neighbor process-neighbor
                   #:post-visit post-visit
                   #:finish finish))

;; bfs : Graph Vertex -> any/c
;; s is the source vertex, default Q is from data/queue
;; see also bfs clients prim and dijkstra
(define (bfs/generic G s 
                     #:init-queue [Q (mk-empty-fifo)]
                     #:break [break? (λ _ #f)]
                     #:init [init void]
                     #:pre-visit [pre-visit void]
                     #:process-neighbor? [process-neighbor? (λ _ #t)]
                     #:process-neighbor [process-neighbor void]
                     #:post-visit [post-visit void]
                     #:finish [finish void])
  (init G s)
  (enqueue! Q s)
  (let loop () (unless (or (break?) (empty? Q))
    (define u (dequeue! Q))
    (pre-visit u)
    (for ([v (in-neighbors G u)] 
          #:when (and (not (break?)) (process-neighbor? G u v)))
      (process-neighbor G u v)
      (enqueue! Q v))
    (post-visit u)
    (loop)))
  (finish G s))

;; returns shortest path in G from source s to v
(define (shortest-path G s v)
  (define-hashes color π)
  (define found-v #f)
  
  (define (init G s)
    (for ([u (in-vertices G)]) (color-set! u WHITE) (π-set! u #f))
    (color-set! s GRAY) (π-set! s #f))
  
  (define (process-neighbor? G u v) (white? (color v)))
  
  (define (found-v?) found-v)
  
  (define (process-neighbor G v1 v2)
    (when (equal? v2 v) (set! found-v #t))
    (color-set! v2 GRAY)
    (π-set!     v2 v1))
  
  (define (post-visit u) (color-set! u BLACK))
  
  (define (finish G s) 
    (if found-v?
        (let loop ([path null] [v v])
          (if (equal? v s) (cons s path) (loop (cons v path) (π v))))
        (error 'shortest-path "no path from ~a to ~a" s v)))
  
  (bfs/generic G s #:break found-v?
                   #:init init
                   #:process-neighbor? process-neighbor?
                   #:process-neighbor process-neighbor
                   #:post-visit post-visit
                   #:finish finish))
           

(define (dfs G #:order [order (λ (vs) vs)])
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes color d π f)
  (for ([u (in-vertices G)]) 
    (color-set! u WHITE)
    (π-set!     u #f))
  (define time 0)
  
  (for ([u (order (in-vertices G))] #:when (white? (color u)))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (add1! time)
      (d-set! u time)
      (for ([v (in-neighbors G u)] #:when (white? (color v)))
        (π-set! v u)
        (dfs-visit v))
      (color-set! u BLACK)
      (add1! time)
      (f-set! u time)))
  
  (values color d π f))

(define (dag? G)
  (define-hashes color)
  (for ([u (in-vertices G)]) (color-set! u WHITE))
  
  (for/and ([u (in-vertices G)] #:when (white? (color u)))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (begin0
        (for/and ([v (in-neighbors G u)])
          (and (not (gray? (color v)))
               (or (black? (color v))
                   (dfs-visit v))))
        (color-set! u BLACK)))))

(define (tsort G)
  (define-values (color d π f) (dfs G))
  (sort (hash-keys f) > #:key (λ (k) (hash-ref f k))))

  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-hashes index lowlink)
  (define S null)
  (define (S-push x) (set! S (cons x S)))
  
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
  
  SCC)