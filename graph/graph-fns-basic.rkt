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
                   #:return finish))

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
                     #:return [finish void])
  (init G s)
  (enqueue! Q s)
  (let loop () (unless (or (break?) (empty? Q))
    (define u (dequeue! Q))
    (pre-visit u)
    (for ([v (in-neighbors G u)] 
          #:when (process-neighbor? G u v) #:break (break?))
      (process-neighbor G u v)
      (enqueue! Q v))
    (post-visit u)
    (loop)))
  (finish G s))

;; returns the path in G from s to v with the fewest vertices in between
(define (fewest-vertices-path G s v)
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
                   #:return finish))
           


(define (dfs G)
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes color d π f)
  (define time 0)
  
  (define (init G)
    (for ([u (in-vertices G)]) (color-set! u WHITE) (π-set! u #f)))
  
  (define (visit? G u) (white? (color u)))
  
  (define (pre-visit u) (color-set! u GRAY) (add1! time) (d-set! u time))
  
  (define (process-neighbor? G u v) (white? (color v)))

  (define (process-neighbor G u v) (π-set! v u))
  
  (define (post-visit u) (color-set! u BLACK) (add1! time) (f-set! u time))
  
  (define (finish G) (values color d π f))
  
  (dfs/generic G #:init init
                 #:visit? visit?
                 #:pre-visit pre-visit
                 #:process-neighbor? process-neighbor?
                 #:process-neighbor process-neighbor
                 #:post-visit post-visit
                 #:return finish))

(define (dfs/generic G #:order [order (λ (vs) vs)]
                       #:break [break? (λ _ #f)]
                       #:init [init void]
                       #:visit? [visit? (λ _ #t)]
                       #:pre-visit [pre-visit void]
                       #:process-neighbor? [process-neighbor? (λ _ #f)]
                       #:process-neighbor [process-neighbor void]
                       #:post-visit [post-visit void]
                       #:return [finish void])
  (init G)
  (for ([u (order (in-vertices G))] #:when (visit? G u) #:break (break?))
    (let dfs-visit ([u u])
      (pre-visit u)
      (for ([v (in-neighbors G u)] 
            #:when (process-neighbor? G u v) #:break (break?))
        (process-neighbor G u v)
        (dfs-visit v))
      (post-visit u)))
  (finish G))

(define (dag? G)
  (define-hashes color)
  (define not-dag #f)
  
  (define (init G) (for ([u (in-vertices G)]) (color-set! u WHITE)))
  
  (define (visit? G u) (white? (color u)))
  
  (define (pre-visit u) (color-set! u GRAY))
  
  (define (process-neighbor? G u v) 
    (if (gray? (color v)) 
        (begin0 #f (set! not-dag #t))
        (not (black? (color v)))))
  
  (define (not-dag?) not-dag)
  
  (define (post-visit u) (color-set! u BLACK))
  
  (define (finish G) (not not-dag))
    
  (dfs/generic G #:break not-dag?
                 #:init init
                 #:visit? visit?
                 #:pre-visit pre-visit
                 #:process-neighbor? process-neighbor?
                 #:post-visit post-visit
                 #:return finish)
  
  #;(for/and ([u (in-vertices G)] #:when (white? (color u)))
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