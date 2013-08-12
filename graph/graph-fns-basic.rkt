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
    (for ([u (in-vertices G)]) (color-set! u WHITE) #;(π-set! u #f)))
  
  (define (visit? G parent u) (white? (color u)))
  
  (define (prologue G parent u) 
    (color-set! u GRAY) (add1! time) (d-set! u time)
    (π-set! u parent))
  
  (define (epilogue G parent u)
    (color-set! u BLACK) (add1! time) (f-set! u time))
  
  (define (finish G) (values color d π f))
  
  (dfs/generic G #:init init
                 #:visit? visit?
                 #:prologue prologue
                 #:epilogue epilogue
                 #:return finish))

(define (dfs/generic G #:order [order (λ (vs) vs)]
                       #:break [break? (λ _ #f)]
                       #:init [init void]
                       #:visit? [visit? (λ _ #t)]
                       #:prologue [prologue void]
                       #:epilogue [epilogue void]
                       #:process-unvisited? [process-unvisited? (λ _ #f)]
                       #:process-unvisited [process-unvisited void]
                       #:return [finish void])
  (init G)
  
;  (define (do-visit u)
;    (pre-visit u)
;    (for ([v (in-neighbors G u)] 
;          #:when (process-neighbor? G u v) #:break (break?))
;      (process-neighbor G u v)
;      (do-visit v))
;    (post-visit u))
;
;  (for ([u (order (in-vertices G))] #:when (visit? G u) #:break (break?)) (do-visit u))

  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u)
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
    
  (dfs/generic G #:break not-dag?
                 #:init init
                 #:visit? visit?
                 #:prologue prologue
                 #:epilogue epilogue
                 #:process-unvisited? process-unvisited?
                 #:process-unvisited process-unvisited
                 #:return finish))

;  (define (init G) (for ([u (in-vertices G)]) (color-set! u WHITE)))
;  
;  (define (visit? G u) (white? (color u)))
;  
;  (define (pre-visit u) (color-set! u GRAY))
;  
;  (define (process-neighbor? G u v) 
;    (if (gray? (color v)) 
;        (begin0 #f (set! not-dag #t))
;        (not (black? (color v)))))
;  
;  (define (not-dag?) not-dag)
;  
;  (define (post-visit u) (color-set! u BLACK))
;  
;  (define (finish G) (not not-dag))
;    
;  (dfs/generic G #:break not-dag?
;                 #:init init
;                 #:visit? visit?
;                 #:pre-visit pre-visit
;                 #:process-neighbor? process-neighbor?
;                 #:post-visit post-visit
;                 #:return finish))

(define (tsort G)
  (define res null) (define (add-finished! v) (set! res (cons v res)))
  (define-hashes color)

  (define (init G) (for ([u (in-vertices G)]) (color-set! u WHITE)))
  
  (define (visit? G parent u) (white? (color u)))
  
  (define (prologue G parent u) (color-set! u GRAY))
  
;  (define (process-neighbor? G u v) (white? (color v)))

  (define (epilogue G parent u) (color-set! u BLACK) (add-finished! u))
  
  (define (finish G) res)
  
  (dfs/generic G #:init init
                 #:visit? visit?
                 #:prologue prologue
                 #:epilogue epilogue
                 #:return finish))
;  (define (init G) (for ([u (in-vertices G)]) (color-set! u WHITE)))
;  
;  (define (visit? G u) (white? (color u)))
;  
;  (define (pre-visit u) (color-set! u GRAY))
;  
;  (define (process-neighbor? G u v) (white? (color v)))
;
;  (define (post-visit u) (color-set! u BLACK) (add-finished! u))
;  
;  (define (finish G) res)
;  
;  (dfs/generic G #:init init
;                 #:visit? visit?
;                 #:pre-visit pre-visit
;                 #:process-neighbor? process-neighbor?
;                 #:post-visit post-visit
;                 #:return finish))

  
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
  
  (define (visit? G v to-visit) (not (hash-has-key? index to-visit)))
  
  (define (prologue G parent v)
    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v))

  ;(define (process-neighbor? G v w) (not (hash-has-key? index w)))

  (define (process-unvisited? G v unvisited) (member unvisited S))
  (define (process-unvisited G v unvisited)
    (lowlink-set! v (min (lowlink v) (index unvisited))))
  
  (define (epilogue G parent v)
    (when (build-SCC? v) (build-SCC v))
    (when parent 
      (lowlink-set! parent (min (lowlink parent) (lowlink v)))))
  
  (define (finish G) SCC)
  
  (dfs/generic G #:visit? visit?
                 #:prologue prologue
                 #:epilogue epilogue
                 #:process-unvisited? process-unvisited?
                 #:process-unvisited process-unvisited
                 #:return finish))
  
;  (define (strongconnect v)
;    (printf "strongconnect ~a\n" v)
;    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
;    (printf "indices ~a\n" index)
;    (printf "lowlinks ~a\n" lowlink)
;    (printf "S ~a\n" S)
;    
;    (for ([w (in-neighbors G v)])
;      (printf "checking edge ~a ~a\n" v w)
;      (cond [(not (hash-has-key? index w))
;             (strongconnect w)
;             (printf "edge ~a ~a RETURN\n" v w)
;             (printf "SET ~a lowlink from ~a to " v (lowlink v))
;             (lowlink-set! v (min (lowlink v) (lowlink w)))
;             (printf "~a\n" (lowlink v))]
;            [(member w S)
;             (printf "SET ~a lowlink from ~a to " v (lowlink v))
;             (lowlink-set! v (min (lowlink v) (index w)))
;             (printf "~a\n" (lowlink v))]))
;    (printf "~a DONE CHECKING NEIGHBORS\n" v)
;    (printf "indices ~a\n" index)
;    (printf "lowlinks ~a\n" lowlink)
;    (when (= (lowlink v) (index v)) ;; v is root node, so create an SCC
;      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
;      (printf "POPPING ~a\n" (cons v new-scc))
;      (set! SCC (cons (cons v new-scc) SCC))
;      (set! S (cdr S-rst))))
;  
;  (for ([v (in-vertices G)] #:unless (hash-has-key? index v)) (strongconnect v))
;  
;  SCC)