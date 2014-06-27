#lang racket

(require "graph-property.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         "../queue/gen-queue.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo))

(require (for-syntax syntax/parse syntax/parse/experimental/template))

(provide (all-defined-out))

;; ----------------------------------------------------------------------------
;; bfs and dfs

;; bfs : Graph Vertex -> [Hashof Vertex -> Number] [Hashof Vertex -> Vertex]
;; s is the source vertex
;; returns 2 values: hash mapping vertices to distances and
;;  hash mapping vertices to predecessor
(define (bfs G s)
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)

  (do-bfs G s 
    #:init (d-set! s 0) (π-set! s #f)
    #:discover (from to) (d-set! to (add1 (d from))) (π-set! to from)
    #:return (values (d->hash) (π->hash))))

;; default Q is from data/queue
;; see also bfs clients prim and dijkstra
(define (bfs/generalized G s #:init-queue [Q (mk-empty-fifo)]
                             #:break [break? (λ _ #f)]
                             #:init [init void]
                             #:visit? [custom-visit?-fn #f]
                             #:discover [discover void]
                             #:visit [visit void]
                             #:return [finish void])
  ; v ∈ visited means v has been seen and enqueued, ie it's no longer "white"
  (define discovered (set)) 
  (define (mark-discovered! v) (set! discovered (set-add discovered v)))
  (define visit? (or custom-visit?-fn 
                     (λ (G s u v) (not (set-member? discovered v)))))
  
  (init G s)
  (enqueue! Q s) ; source vertex s is always visited
  (mark-discovered! s)
  (for ([u (in-queue Q)])
    (visit G s u)
    (for ([v (in-neighbors G u)] #:when (visit? G s u v) #:break (break?))
      (mark-discovered! v)
      (discover G s u v)
      (enqueue! Q v)))
  (finish G s))

;; cleaner syntax for bfs/generalized
(define-syntax (do-bfs stx)
  (syntax-parse stx 
    [(_ G s 
      (~or (~optional (~seq #:init-queue Q:expr))
           (~optional (~seq #:break break?:expr))
           (~optional (~seq #:init init:expr ...))
           (~optional (~seq #:visit? (v?-from:id v?-to:id) visit?:expr ...))
           (~optional (~seq #:discover (disc-from:id disc-to:id) disc:expr ...))
           (~optional (~seq #:visit (v:id) visit:expr ...))
           (~optional (~seq #:return return:expr ...))) ...)
     (template
      (bfs/generalized G s 
       (?? (?@ #:init-queue Q))
       (?? (?@ #:break break?))
       (?? (?@ #:init (λ _ init ...)))
       (?? (?@ #:visit? (λ (G s v?-from v?-to) visit? ...)))
       (?? (?@ #:discover (λ (G s disc-from disc-to) disc ...)))
       (?? (?@ #:visit (λ (G s v) visit ...)))
       (?? (?@ #:return (λ _ return ...)))))]))
             

;; bfs-based fns --------------------------------------------------------------
;; (see also prim and dijkstra)

;; returns the path in G from s to v with the fewest vertices in between
;; ie, the shortest path from s to v for undirected graphs, 
;;     or graphs where all edges have the same weight
(define (fewest-vertices-path G s v)
  (cond
    [(vertex=? G s v) (list s)]
    [else
     (define-vertex-property G π #:init #f)
     (define-graph-property found-v? #f)
     
     (do-bfs G s #:break get-found-v?
       #:discover (from to)
         (when (vertex=? G to v) (found-v?-set! #t))
         (π-set! to from)
       #:return
         (and found-v?
              (let loop ([path null] [v v])
                (if (vertex=? G v s) 
                    (cons s path) 
                    (loop (cons v path) (π v))))))]))

           
;; dfs ------------------------------------------------------------------------

(define (dfs G)
  ;; d[u] = discovery time, π[u] = pred, f[u] = finishing time
  (define-vertex-properties G d π f)
  (define time 0)

  (do-dfs G
    #:prologue (parent u) (add1! time) (d-set! u time) (π-set! u parent)
    #:epilogue (parent u) (add1! time) (f-set! u time)
    #:return (values (d->hash) (π->hash) (f->hash))))

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

;; cleaner syntax for dfs/generalized
(define-syntax (do-dfs stx)
  (syntax-parse stx 
    [(_ G 
     (~or (~optional (~seq #:order order:expr))
          (~optional (~seq #:break break?:expr))
          (~optional (~seq #:init init:expr ...))
          (~optional (~seq #:visit? (v?-from:id v?-to:id) visit?:expr ...))
          (~optional (~seq #:prologue (pro-from:id pro-to:id) pro:expr ...))
          (~optional (~seq #:epilogue (epi-from:id epi-to:id) epi:expr ...))
          (~optional (~seq #:process-unvisited? (pu?-from:id pu?-to:id) pu?:expr ...))
          (~optional (~seq #:process-unvisited (pu-from:id pu-to:id) pu:expr ...))
          (~optional (~seq #:return return:expr ...))) ...)
     (template
      (dfs/generalized G 
        (?? (?@ #:order order))
        (?? (?@ #:break break?))
        (?? (?@ #:init (λ _ init ...)))
        (?? (?@ #:visit? (λ (G v?-from v?-to) visit? ...)))
        (?? (?@ #:prologue (λ (G pro-from pro-to) pro ...)))
        (?? (?@ #:epilogue (λ (G epi-from epi-to) epi ...)))
        (?? (?@ #:process-unvisited? (λ (G pu?-from pu?-to) pu? ...)))
        (?? (?@ #:process-unvisited (λ (G pu-from pu-to) pu ...)))
        (?? (?@ #:return (λ _ return ...)))))]))

;; dfs-based fns --------------------------------------------------------------

(define (dag? G)
  (define-vertex-property G color #:init WHITE)
  (define-graph-property not-dag? #f)
  
  (do-dfs G #:break get-not-dag?
    #:visit? (from to) (white? (color to))
    #:prologue (parent v) (color-set! v GRAY)
    #:epilogue (parent v) (color-set! v BLACK)
    #:process-unvisited? (from to) (gray? (color to))
    #:process-unvisited (from to) (not-dag?-set! #t)
    #:return (not not-dag?)))

(define (tsort G)
  (define sorted null) 

  (do-dfs G
    #:epilogue (parent v) (set! sorted (cons v sorted)) ; add finished
    #:return sorted))
  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-vertex-properties G index lowlink)

  (define S null)   
  (define (S-push x) (set! S (cons x S)))
  
  (define SCC null)
  (define (build-SCC? v) (= (lowlink v) (index v)))
  (define (build-SCC v)
    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (vertex=? G w v)))))
    (set! SCC (cons (cons v new-scc) SCC))
    (set! S (cdr S-rst)))

  (do-dfs G 
    #:prologue (parent v)
      (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
    #:epilogue (parent v)
      (when (build-SCC? v) (build-SCC v))
      (when parent (lowlink-set! parent (min (lowlink parent) (lowlink v))))
    #:process-unvisited? (from to) (member to S)
    #:process-unvisited (from to) (lowlink-set! from (min (lowlink from) (index to)))
    #:return SCC))


      