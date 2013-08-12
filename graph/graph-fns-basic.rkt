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
      #;(color-set! u WHITE) (d-set! u +inf.0) (π-set! u #f))
    #;(color-set! s GRAY) (d-set! s 0) (π-set! s #f))
    
;  (define (process-neighbor? G u v) (white? (color v)))
    
;  (define (process-neighbor G u v)
  (define (process-edge G s u v)
;    (color-set! v GRAY)
    (d-set! v (add1 (d u)))
    (π-set! v u))
    
 ; (define (post-visit u) (color-set! u BLACK))
    
  (define (finish G s) (values d π))

  (bfs/generic G s #:init init
                   #:process-edge process-edge
;                   #:process-neighbor? process-neighbor?
;                   #:process-neighbor process-neighbor
;                   #:post-visit post-visit
                   #:return finish))

;; bfs : Graph Vertex -> any/c
;; s is the source vertex, default Q is from data/queue
;; see also bfs clients prim and dijkstra
(define (bfs/generic G s 
                     #:init-queue [Q (mk-empty-fifo)]
                     #:break [break? (λ _ #f)]
                     #:init [init void]
                     #:visit? [custom-visit?-fn #f]
                     #:visit [visit void]
                     #:process-edge [process-edge void]
                     #:return [finish void])
  ; v ∈ visited means v has been seen and enqueued, ie it's no longer "white"
  (define visited (set)) 
  (define (mark-visited! v) (set! visited (set-add visited v)))
  (define visit? (or custom-visit?-fn (λ (G s u v) (not (set-member? visited v)))))
  (init G s)
  (enqueue! Q s) ; source vertex s is always visited
  (mark-visited! s)
;  (let loop () (unless (or (break?) (empty? Q))
;    (define u (dequeue! Q))
  (for ([u (in-queue Q)])
    (visit G s u)
    (for ([v (in-neighbors G u)] #:when (visit? G s u v) #:break (break?))
      (mark-visited! v)
      (process-edge G s u v)
      (enqueue! Q v)))
;    (loop)))
  (finish G s))

;; returns the path in G from s to v with the fewest vertices in between
(define (fewest-vertices-path G s v)
  (define-hashes color π)
  (define found-v #f)
  
  (define (init G s)
    (for ([u (in-vertices G)]) #;(color-set! u WHITE) (π-set! u #f))
    #;(color-set! s GRAY) (π-set! s #f))
  
;  (define (process-neighbor? G u v) (white? (color v)))
  
  (define (found-v?) found-v)
  
;  (define (process-neighbor G v1 v2)
  (define (process-edge G s v1 v2)
    (when (equal? v2 v) (set! found-v #t))
    #;(color-set! v2 GRAY)
    (π-set!     v2 v1))
  
;  (define (post-visit u) (color-set! u BLACK))
  
  (define (finish G s) 
    (if found-v?
        (let loop ([path null] [v v])
          (if (equal? v s) (cons s path) (loop (cons v path) (π v))))
        (error 'shortest-path "no path from ~a to ~a" s v)))
  
  (bfs/generic G s #:break found-v?
                   #:init init
                   #:process-edge process-edge
 ;                  #:process-neighbor? process-neighbor?
 ;                  #:process-neighbor process-neighbor
 ;                  #:post-visit post-visit
                   #:return finish))
           


(define (dfs G)
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes d π f)
  (define time 0)
  
  (define (prologue G parent u) (add1! time) (d-set! u time) (π-set! u parent))
  
  (define (epilogue G parent u) (add1! time) (f-set! u time))
  
  (define (finish G) (values d π f))
  
  (dfs/generalized G #:prologue prologue #:epilogue epilogue #:return finish))

(define (dfs/generalized G #:order [order (λ (vs) vs)]
                           #:break [break? (λ _ #f)]
                           #:init [init void]
                           #:visit? [custom-visit?-fn #f]
                           #:prologue [prologue void]
                           #:epilogue [epilogue void]
                           #:process-unvisited? [process-unvisited? (λ _ #f)]
                           #:process-unvisited [process-unvisited void]
                           #:return [finish void])
  (define visited (set))
  (define (mark-visited! v) (set! visited (set-add visited v)))
  (define visit? (or custom-visit?-fn (λ (G u v) (not (set-member? visited v)))))
  
  (init G)
  
  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u)
    (mark-visited! u)
    (prologue G parent u)
    (for ([v (in-neighbors G u)] #:break (break?))
      (cond [(visit? G u v) (do-visit u v)]
            [(process-unvisited? G u v) (process-unvisited G u v)]))
    (epilogue G parent u))
  
  ;; outer loop: picks a new start node when previous search reaches dead end
  (for ([u (order (in-vertices G))] #:break (break?))
    (cond [(visit? G #f u) (do-visit #f u)]
          [(process-unvisited? G #f u) (process-unvisited G #f u)]))
  
  (finish G))

(define (dag? G)
  (define-hashes color)
  (define not-dag #f)

  (define (init G) (for ([u (in-vertices G)]) (color-set! u WHITE)))
  
  (define (visit? G parent u) (white? (color u)))
  
  (define (prologue G parent u) (color-set! u GRAY))
  
  (define (process-unvisited? G parent v) (gray? (color v)))
  (define (process-unvisited G parent v) (set! not-dag #t))
  
  (define (not-dag?) not-dag)
  
  (define (epilogue G parent u) (color-set! u BLACK))
  
  (define (finish G) (not not-dag))
    
  (dfs/generalized G #:break not-dag?
                     #:init init
                     #:visit? visit?
                     #:prologue prologue
                     #:epilogue epilogue
                     #:process-unvisited? process-unvisited?
                     #:process-unvisited process-unvisited
                     #:return finish))

(define (tsort G)
  (define sorted null) 
  (define (add-finished! v) (set! sorted (cons v sorted)))
  (define-hashes color)

  (define (epilogue G parent u) (add-finished! u))
  
  (define (finish G) sorted)
  
  (dfs/generalized G #:epilogue epilogue #:return finish))

  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-hashes index lowlink)

  (define S null)   
  (define (S-push x) (set! S (cons x S)))
  
  (define SCC null)
  (define (build-SCC? v) (= (lowlink v) (index v)))
  (define (build-SCC v)
    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
    (set! SCC (cons (cons v new-scc) SCC))
    (set! S (cdr S-rst)))
  
  (define (prologue G parent v)
    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v))

  (define (epilogue G parent v)
    (when (build-SCC? v) (build-SCC v))
    (when parent (lowlink-set! parent (min (lowlink parent) (lowlink v)))))

  (define (process-unvisited? G v unvisited) (member unvisited S))
  (define (process-unvisited G v unvisited)
    (lowlink-set! v (min (lowlink v) (index unvisited))))
  
  (define (finish G) SCC)
  
  (dfs/generalized G #:prologue prologue
                     #:epilogue epilogue
                     #:process-unvisited? process-unvisited?
                     #:process-unvisited process-unvisited
                     #:return finish))
